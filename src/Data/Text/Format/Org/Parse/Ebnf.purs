module Data.Text.Format.Org.Parse.Ebnf where

import Prelude

import Effect (Effect)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.String (Pattern(..))
import Data.String (joinWith, length, split, uncons, codePointFromChar, drop, toLower, trim, stripSuffix) as String
import Data.String.CodePoints as SCP
import Data.String.CodeUnits as SCU
import Data.Int (fromString) as Int
import Data.Text.Format.Org.Types (OrgFile)
import Data.Text.Format.Org.Types as Org
import Data.Text.Format.Org.Construct as Org
import Data.Text.Format.Org.Keyword as KW
import Data.Text.Format.Org.Property as Prop
import Data.Foldable (foldl)
import Data.Array ((:))
import Data.Array (head, catMaybes, uncons, singleton) as Array
import Data.Array.NonEmpty (fromArray, singleton) as NEA
import Data.Either (Either(..))
import Data.Newtype (unwrap)

import Control.Alt ((<|>))

import Foreign (F)
import Yoga.JSON (readImpl, E, class ReadForeign)


newtype FromEbnf = FromEbnf OrgFile


data Match
    = Text String
    | Matches (Array Match)


data Rule
    = Rule String (Array Rule)
    | TextRule String String
    | RuleMatch String


toRule :: Match -> Maybe Rule
toRule =
    case _ of
        Text text -> Just $ RuleMatch text
        Matches [ Text rName, Text rValue ] -> Just $ TextRule rName rValue
        Matches matches ->
            Array.uncons matches
                >>= \{ head, tail } ->
                    case head of
                        Text rName -> Just $ Rule rName $ Array.catMaybes $ toRule <$> tail
                        _ -> Nothing


data Subject
    = B { nested :: Boolean } Org.Block
    | D Org.Drawer
    | P (Prop.OrgProperties String)
    | K (KW.OrgKeywords String)


data ContentTarget
    = TopLevel
    | GoesTo { hasLines :: Boolean } Subject
    -- List ?


type State =
    { orgf :: OrgFile
    , target :: ContentTarget
    }


init :: State
init =
     -- TODO: may be just store current block to collect the changes in the state and actually add it in the end
    { orgf : Org.empty
    , target : TopLevel
    }


extractFromRoot :: Rule -> OrgFile
extractFromRoot =
    case _ of
        Rule "S" rules ->
            appyRemainings $ foldl applySub init rules
        _ -> Org.empty
    where

        appyRemainings { orgf, target } =
            case target of
                TopLevel -> orgf
                GoesTo _ (K kws)    ->
                    if (Org.blocksn $ Org.docf orgf) > 0 || (Org.sectionsn $ Org.docf orgf) > 0 then
                        Org.wdoc (Org.wlast_bl_rec $ Org.with_kws $ unwrap kws) orgf
                    else
                        foldl (flip Org.meta_kw) orgf $ unwrap kws
                GoesTo _ (P props)  ->
                    if (Org.blocksn $ Org.docf orgf) > 0 || (Org.sectionsn $ Org.docf orgf) > 0 then
                        Org.wdoc (Org.wlast_sec $ Org.wprops $ unwrap props) orgf
                    else
                        foldl (flip Org.meta_prop) orgf $ unwrap props
                GoesTo _ (D drawer) -> orgf -- TODO
                GoesTo _ (B _ block)  -> orgf -- TODO

        applySub { orgf, target } = case _ of
            Rule "headline" hlRules ->
                { orgf : fromMaybe orgf $ Array.uncons hlRules <#> \{ head, tail } ->
                    case head of
                        TextRule "stars" val ->
                            orgf # Org.wdoc
                                (Org.snoc_sec $ foldl applySecHeadRule (Org.sece (String.length val) []) tail)
                        _ -> orgf
                , target
                }
            Rule "content-line" contentRules ->
                { orgf :
                    case target of
                        TopLevel ->
                            orgf # Org.append_bl (blockFrom $ _extractWordsRules contentRules) -- TODO: also collect to `ContentTarget` before?
                        GoesTo _ (K kws) ->
                            orgf # Org.append_bl (Org.with_kws (unwrap kws) $ blockFrom $ _extractWordsRules contentRules)
                        _ -> orgf
                , target :
                    case target of
                        TopLevel -> target
                        GoesTo { hasLines } subject ->
                            let
                                wordsToAdd = wordsFromRules $ _extractWordsRules contentRules
                                wordsToAdd' = if hasLines then Org.br : wordsToAdd else wordsToAdd -- a kind of hack to add breaks to the contents of the blocks
                            in
                            case subject of
                                B n block ->
                                    GoesTo { hasLines : true } $ B n $ Org.inject_words wordsToAdd' block
                                D drawer ->
                                    GoesTo { hasLines : true } $ D $ Org.drawer_add wordsToAdd' drawer
                                P props ->
                                    GoesTo { hasLines : true } $ P $ Prop.snoc props $ _extractProp $ String.joinWith "" $ collectTextOnly <$> _extractWordsRules contentRules
                                K kws ->
                                    TopLevel -- we reset the level and apply them above
                }
            Rule "empty-line" [] ->
                { orgf : orgf # Org.append_bl Org.blank
                , target
                }
            TextRule "horizontal-rule" _ ->
                { orgf : orgf # Org.append_bl Org.hr
                , target
                }
            Rule "block-begin-line" [ TextRule "block-name" blockName ] ->
                { orgf
                , target : _startBlock target blockName Nothing
                }
            Rule "block-begin-line" [ TextRule "block-name" blockName, TextRule "block-parameters" blockParams ] ->
                { orgf
                , target : _startBlock target blockName $ Just blockParams
                }
            Rule "block-end-line" [ TextRule "block-name" blockName ] ->
                { orgf :
                    case target of
                        GoesTo _ (B { nested } block) ->
                            if not nested then
                                orgf # Org.append_bl block
                            else
                                orgf # Org.append_bl (Org.inject_words [ Org.br, Org.text $ "#+end_" <> blockName ] block)
                        _ -> orgf -- should not happen
                , target : TopLevel
                }
            Rule "other-keyword-line" [ TextRule "kw-name" kwTitle, TextRule "kw-value" kwValue ] ->
                { orgf
                , target :
                    case target of
                        GoesTo { hasLines } (K kws) ->
                            GoesTo { hasLines } $ K $ KW.snoc kws $ KW.kw kwTitle kwValue
                        _ -> GoesTo { hasLines : false } $ K $ KW.snoc KW.empty $ KW.kw kwTitle kwValue
                }
            TextRule "fixed-width-line" fwLine ->
                { orgf : orgf # Org.append_bl (Org.fw [ Org.text fwLine ])
                , target
                }
            Rule "list-item-line" liRules ->
                { orgf : orgf # Org.append_bl (Org.det_item $ foldl applyListItemRule Org.emptyDetItem liRules)
                , target
                }
            Rule "footnote-line" [ TextRule "fn-label" fnLabel, Rule "text" wordsRules ] ->
                { orgf : orgf # Org.append_bl (Org.fn_ fnLabel $ wordsFromRules wordsRules)
                , target
                }
            Rule "comment-line" [ TextRule "comment-line-head" commentHead, TextRule "comment-line-rest" commentRest ] ->
                { orgf : orgf # Org.append_bl (Org.lcomment [ String.drop 1 commentRest ])
                , target
                }
            Rule "drawer-begin-line" [ TextRule "drawer-name" drawerName ] ->
                let
                     -- although rule is named `drawer-begin-line`, drawer end comes from parser this way
                    isDrawerEnd = String.toLower drawerName == "end"
                    isProperties = String.toLower drawerName == "properties"
                in
                    { orgf : if isDrawerEnd then _finishDrawer orgf target else orgf
                    , target : if not isDrawerEnd && not isProperties then
                            case target of
                                GoesTo _ (P props) ->
                                    -- FIXME: it handles the issue when already parsing `PROPERTIES` block and there's only `:NAME:` w/o value, which is a valid property,
                                    -- FIXME: but parser treats it as a start of drawer with the name `NAME`. See `04h-formatting-properties-and-keywords.ebnf.json`
                                     GoesTo { hasLines : false } $ P $ Prop.snoc props $ Prop.propn drawerName
                                _ -> GoesTo { hasLines : false } $ D $ Org.mk_drawer drawerName []
                        else if isProperties then
                            GoesTo { hasLines : false } $ P $ Prop.empty
                        else TopLevel
                    }
            Rule "drawer-end-line" [] ->
                { orgf : _finishDrawer orgf target
                , target : TopLevel
                }
            Rule "clock" [ Rule "timestamp-inactive-range" [ startTsRule, endTsRule ], Rule "clock-duration" [ TextRule "clock-dur-hh" hhDurValue, TextRule "clock-dur-mm" mmDurValue ] ] ->
                { orgf : orgf # Org.append_bl
                        (Org.clockB
                            (applyTimestampRule (Org.idate $ Org.d 0 0 0) startTsRule)
                            (applyTimestampRule (Org.idate $ Org.d 0 0 0) endTsRule)
                            (fromMaybe 0 $ Int.fromString hhDurValue)
                            (fromMaybe 0 $ Int.fromString mmDurValue)
                        )
                , target
                }
            Rule "clock" [ Rule "timestamp-active-range" [ startTsRule, endTsRule ], Rule "clock-duration" [ TextRule "clock-dur-hh" hhDurValue, TextRule "clock-dur-mm" mmDurValue ] ] ->
                { orgf : orgf # Org.append_bl
                        (Org.clockB
                            (applyTimestampRule (Org.adate $ Org.d 0 0 0) startTsRule)
                            (applyTimestampRule (Org.adate $ Org.d 0 0 0) endTsRule)
                            (fromMaybe 0 $ Int.fromString hhDurValue)
                            (fromMaybe 0 $ Int.fromString mmDurValue)
                        )
                , target
                }
            Rule "table" [ Rule "table-org" tableRows ] ->
                { orgf : orgf # Org.append_bl
                    (case Org.last_bl_of $ Org.docf orgf of
                        -- append line break if the table was last
                        Just (Org.Table _ _) -> Org.joinB Org.blank $ Org.table $ Array.catMaybes $ extractRowRule <$> tableRows
                        _ ->                    Org.table $ Array.catMaybes $ extractRowRule <$> tableRows
                    )
                , target : TopLevel
                }
            _ -> { orgf, target }

        _startBlock target blockName mbParams =
            case target of
                TopLevel ->
                    case blockName of
                        "src"     -> GoesTo { hasLines : false } $ B { nested : false }
                                        $ case mbParams of
                                            Nothing -> Org.code' []
                                            Just params -> Org.codeIn' params []
                        "quote"   -> GoesTo { hasLines : false } $ B { nested : false } $ Org.quote []
                        "example" -> GoesTo { hasLines : false } $ B { nested : false } $ Org.example []
                        "comment" -> GoesTo { hasLines : false } $ B { nested : false } $ Org.bcomment []
                        _ -> TopLevel
                        -- TODO: support other blocks:
                        -- COMMENT|comment|EXAMPLE|example|EXPORT|export|SRC|src
                        -- VERSE|verse
                        -- CENTER|center|QUOTE|quote
                -- when previous block wasn't finished, we should append the text to the block
                GoesTo _ (B _ block) ->
                    GoesTo { hasLines : true }
                        $ B { nested : true }
                        $ Org.inject_words
                                [ Org.br, Org.text "#+begin_", Org.text blockName
                                , case mbParams of
                                    Just params -> Org.text $ " " <> params
                                    Nothing -> Org.EmptyW
                                ]
                                block
                GoesTo _ _ -> target
                _ -> TopLevel

        _finishDrawer orgf target =
            case target of
                GoesTo _ (D drawer) ->
                    let
                        addDrawer :: Org.Section -> Org.Section
                        addDrawer sec =
                            case Org.last_bl_of $ Org.docs sec of
                                Just (Org.DetachedItem dlitem) -> -- TODO: mark that the last item was a list item using `State`
                                    sec # Org.sec_wdoc (Org.wlast_bl_rec $ const $ Org.DetachedItem $ Org.det_add_drawer drawer dlitem)
                                Just someBlock ->
                                    sec # Org.append_bl_sec (Org.IsDrawer drawer)
                                Nothing -> sec # Org.sec_append_drawer drawer
                    in
                        orgf # Org.wdoc (Org.wlast_sec addDrawer)
                GoesTo _ (P props) ->
                    if (Org.sectionsn (Org.docf orgf) > 0) then
                        orgf # Org.wdoc (Org.wlast_sec $ \sec -> foldl (flip Org.wprop) sec $ unwrap props)
                    else foldl (flip Org.meta_prop) orgf $ unwrap props
                _ -> orgf -- should not happen

        _extractWordsRules =
            case _ of
                [ Rule "text" wordsRules ] ->
                    wordsRules
                _ -> []

        _extractListType = case _ of
            "*" -> Org.Bulleted
            "-" -> Org.Hyphened
            "+" -> Org.Plussed
            _ -> Org.Bulleted

        _extractCheckboxType = case _ of
            "X" -> Org.Check
            "-" -> Org.Halfcheck
            " " -> Org.Uncheck
            _ -> Org.Uncheck

        _extractProp propText =
            case String.split (Pattern ":") propText of
                [ _, nameStr, valStr ] ->
                    let trimmedValStr = String.trim valStr
                    in case String.stripSuffix (Pattern "+") nameStr of
                        Just nameWithoutSuffix ->
                            if String.length trimmedValStr > 0 then
                                Prop.propapp nameWithoutSuffix trimmedValStr
                            else
                                Prop.propappn nameWithoutSuffix
                        Nothing ->
                            if String.length trimmedValStr > 0 then
                                Prop.prop nameStr trimmedValStr
                            else
                                Prop.propn nameStr
                _ -> Prop.prop "ERR" "ERR_VAL" -- FIXME: return `Maybe`

        applyListItemRule litem =
            case _ of
                TextRule "indent" indent -> litem # Org.det_indent indent
                TextRule "list-item-bullet" bullet -> litem # Org.det_ltype (_extractListType bullet)
                TextRule "list-item-counter" counter -> litem # Org.det_ltype (Org.Prefixed counter) -- litem # Org.det_counter (Org.Counter $ fromMaybe 0 $ Int.fromString counter)
                TextRule "list-item-counter-suffix" suffix ->
                    litem # Org.det_ch_ltype
                        (case _ of
                            Org.Prefixed str -> Org.Prefixed $ str <> suffix
                            ltype -> ltype
                        )
                TextRule "list-item-tag" tag -> litem # Org.det_tag tag
                Rule "list-item-checkbox" [ TextRule "list-item-checkbox-state" cbState ] -> litem # Org.det_check (_extractCheckboxType cbState)
                Rule "text" textRules -> litem # Org.det_add_text (wordsFromRules textRules)
                _ -> litem

        applySecHeadRule sec =
            case _ of
                Rule "text" wordsRules ->
                    sec # Org.sec_head (wordsFromRules wordsRules)
                Rule "planning" planningRules ->
                    foldl applySecPlanningRule sec planningRules
                TextRule "keyword" "TODO" ->
                    sec # Org.set Org.Todo
                TextRule "keyword" "DONE" ->
                    sec # Org.set Org.Done
                TextRule "keyword" "DOING" ->
                    sec # Org.set Org.Doing
                TextRule "keyword" "NOW" ->
                    sec # Org.set Org.Now
                TextRule "keyword" kwStr -> -- FIXME: EBNF sometimes fails and parses first heading letter as keyword, try "A Heading." / 03d-headings-with-tags
                    sec # Org.set (Org.CustomKW kwStr)
                TextRule "priority" pChar ->
                    case pChar of
                        "A" -> sec # Org.priority (Org.Alpha 'A')
                        "B" -> sec # Org.priority (Org.Alpha 'B')
                        "C" -> sec # Org.priority (Org.Alpha 'C')
                        _ -> case SCU.uncons pChar of
                                Just { head, tail } ->
                                    if head == '#' then
                                        sec # Org.priority (Org.Num $ fromMaybe 0 $ Int.fromString tail)
                                    else
                                        sec # Org.priority (Org.Alpha head)
                                Nothing -> sec
                Rule "comment-token" [] ->
                    sec # Org.comment
                _ -> sec

        applyTimestampRule ts =
            case _ of
                Rule "ts-inner-w-time"  [ TextRule "ts-date" dateStr, TextRule "ts-day" _, TextRule "ts-time" timeStr ] ->
                    ts # Org.chdate (Org.parseDate dateStr) # Org.at_ (Org.parseTime timeStr)
                Rule "ts-inner-wo-time" [ TextRule "ts-date" dateStr, TextRule "ts-day" _ ] ->
                    ts # Org.chdate (Org.parseDate dateStr)
                Rule "ts-modifiers" modifiers ->
                    foldl applyTimestampRule ts modifiers
                Rule "ts-repeater" [ TextRule "ts-repeater-type" repStr, TextRule "ts-mod-value" valStr, TextRule "ts-mod-unit" unitStr ] ->
                    ts # Org.repeat
                        (fromMaybe Org.Single $ Org.parseRepeaterMode repStr)
                        (fromMaybe 0 $ Int.fromString valStr)
                        (fromMaybe Org.Day $ Org.parseInterval unitStr)
                Rule "ts-repeater" [ TextRule "ts-repeater-type" repStr, TextRule "ts-mod-value" valStr, TextRule "ts-mod-unit" unitStr, Rule "ts-mod-at-least" [ TextRule "ts-mod-value" wValStr, TextRule "ts-mod-unit" wUnitStr ] ] ->
                    ts # Org.repeat
                        (fromMaybe Org.Single $ Org.parseRepeaterMode repStr)
                        (fromMaybe 0 $ Int.fromString valStr)
                        (fromMaybe Org.Day $ Org.parseInterval unitStr)
                       # Org.rwith
                        (fromMaybe 0 $ Int.fromString wValStr)
                        (fromMaybe Org.Day $ Org.parseInterval wUnitStr)
                Rule "ts-warning" [ TextRule "ts-warning-type" delStr, TextRule "ts-mod-value" valStr, TextRule "ts-mod-unit" unitStr ] ->
                    ts # Org.delay
                        (fromMaybe Org.One $ Org.parseDelayMode delStr)
                        (fromMaybe 0 $ Int.fromString valStr)
                        (fromMaybe Org.Day $ Org.parseInterval unitStr)
                _ -> ts

        applySecPlanningRule sec =
            case _ of
                Rule "planning-info" [ Rule "planning-keyword" kwRules, Rule "timestamp" tsRules ] ->
                    case kwRules of
                        [ Rule "planning-kw-deadline" [] ]  -> sec # Org.deadline (buildTimeStamp tsRules)
                        [ Rule "planning-kw-scheduled" [] ] -> sec # Org.schedule (buildTimeStamp tsRules)
                        [ Rule "planning-kw-closed" [] ]    -> sec # Org.close    (buildTimeStamp tsRules)
                        _ -> sec
                _ -> sec

        extractRowRule =
            case _ of
                Rule "table-row" [ Rule "table-row-cells" cellsRules ] -> NEA.fromArray (Array.catMaybes $ extractCellRule <$> cellsRules) <#> Org.Row
                Rule "table-row" [ TextRule "table-row-sep" sepText ] -> Just $ Org.BreakT
                _ -> Nothing

        extractCellRule =
            case _ of
                TextRule "table-cell" cellText -> Just $ Org.Column $ NEA.singleton $ Org.text cellText
                _ -> Nothing

        buildTimeStamp tsRules =
            case tsRules of
                [ Rule "timestamp-active"   [ Rule "ts-inner" innerTsRules ] ] -> foldl applyTimestampRule (Org.adate $ Org.d 0 0 0) innerTsRules
                [ Rule "timestamp-inactive" [ Rule "ts-inner" innerTsRules ] ] -> foldl applyTimestampRule (Org.idate $ Org.d 0 0 0) innerTsRules
                _ -> Org.adate $ Org.d 0 0 0

        collectTextOnly =
            case _ of
                TextRule "text-normal" textVal ->
                    textVal
                TextRule "text-sty-code" textVal ->
                    "~" <> textVal <> "~"
                TextRule "text-sty-bold" textVal ->
                    "*" <> textVal <> "*"
                TextRule "text-sty-italic" textVal ->
                    "/" <> textVal <> "/"
                TextRule "text-sty-verbatim" textVal ->
                    "=" <> textVal <> "="
                TextRule "text-sty-strikethrough" textVal ->
                    "+" <> textVal <> "+"
                TextRule "text-sty-underlined" textVal ->
                    "_" <> textVal <> "_"
                Rule "text-sub" [ TextRule "text-subsup-word" textVal ] ->
                    "_" <> textVal
                Rule "text-sup" [ TextRule "text-subsup-word" textVal ] ->
                    "^" <> textVal
                _ -> ""

        toWordsFromRule =
            case _ of
                TextRule "text-normal" textVal ->
                    Just $ Org.Plain textVal
                -- FIXME: EBNF parser can not parse inner formatting, but we can do some post-processing
                TextRule "text-sty-code" textVal ->
                    Just $ Org.ic textVal
                TextRule "text-sty-bold" textVal ->
                    Just $ Org.b textVal
                TextRule "text-sty-italic" textVal ->
                    Just $ Org.i textVal
                TextRule "text-sty-verbatim" textVal ->
                    Just $ Org.v textVal
                TextRule "text-sty-strikethrough" textVal ->
                    Just $ Org.s textVal
                TextRule "text-sty-underlined" textVal ->
                    Just $ Org.u textVal
                Rule "text-sub" [ TextRule "text-subsup-word" textVal ] ->
                    Just $ Org.subs textVal
                Rule "text-sup" [ TextRule "text-subsup-word" textVal ] ->
                    Just $ Org.sups textVal
                Rule "link-format" [ Rule "link" linkRules, TextRule "link-description" linkDescr ] ->
                    Just $ Org.to (createLinkTarget linkRules) linkDescr
                Rule "link-format" [ Rule "link" linkRules ] ->
                    Just $ Org.ref (createLinkTarget linkRules)
                Rule "footnote-link" [ TextRule "fn-label" label ] ->
                    Just $ Org.fn label
                Rule "footnote-link" [ TextRule "fn-label" label, RuleMatch text ] ->
                    Just $ Org.fndef label text
                TextRule "footnote-link" defText ->
                    Just $ Org.fndef' defText
                Rule "timestamp" [ TextRule "timestamp-diary" diaryValue ] ->
                    Just $ Org.diary diaryValue
                Rule "timestamp" tsRules ->
                    Just $ Org.at $ buildTimeStamp tsRules
                _ -> Nothing

        createLinkTarget linkRules =
            case linkRules of
                [ Rule "link-int" [ TextRule "link-file-loc-string" headingStr ] ] -> Org.head headingStr
                [ Rule "link-ext" [ TextRule "link-ext-file" fileLoc ] ] -> Org.loc fileLoc
                [ Rule "link-ext" [ Rule "link-ext-other" [ TextRule "link-url-scheme" linkScheme, TextRule "link-url-rest" linkRest ] ] ] -> Org.rem $ linkScheme <> ":" <> linkRest
                _ -> Org.rem "LINK"

        wordsFromRules wordsRules = Array.catMaybes $ toWordsFromRule <$> wordsRules

        blockFrom =
            Org.para <<< wordsFromRules



instance Show Match where
    show = case _ of
        Text str -> "T:" <> str
        Matches ms -> "M: {" <> String.joinWith ";" (show <$> ms) <> "} "


instance Show Rule where
    show = case _ of
        Rule str rules -> "Rule:" <> str <> " -> <" <> (String.joinWith "> ; <" (show <$> rules)) <> ">"
        TextRule rName rValue -> "TextRule:<" <> rName <> ":" <> rValue <> ">"
        RuleMatch rm -> "JustRule:" <> show rm


instance ReadForeign FromEbnf where
    readImpl json =
        (readImpl json :: F ({ data :: Array Match })) <#> (\rec ->
            let
                mbRootRule =
                    case Array.uncons rec.data of
                        Just { head, tail } ->
                            case head of
                                Text "S" -> Just $ Rule "S" $ Array.catMaybes $ toRule <$> tail
                                _ -> Nothing
                        Nothing -> Nothing
            in
                FromEbnf $ maybe Org.empty extractFromRoot mbRootRule
        )
        -- pure $ FromEbnf $ Org.empty


instance ReadForeign Match where
    readImpl json
        =   (Text    <$> (readImpl json :: F String))
        <|> (Matches <$> (readImpl json :: F (Array Match)))



newtype EbnfGrammar = EbnfGrammar String
newtype OrgText = OrgText String
newtype TestFileSlug = TestFileSlug String

foreign import parseOrgWithEbnf :: EbnfGrammar -> OrgText -> String
foreign import writeEbnfJsonFor :: TestFileSlug -> Effect Unit