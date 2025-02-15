module Data.Text.Format.Org.Ebnf where

import Prelude

import Debug as Debug

import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.String (Pattern(..))
import Data.String (joinWith, length, split, uncons, codePointFromChar, drop, toLower) as String
import Data.String.CodePoints as SCP
import Data.String.CodeUnits as SCU
import Data.Int (fromString) as Int
import Data.Text.Format.Org.Types (OrgFile)
import Data.Text.Format.Org.Types as Org
import Data.Text.Format.Org.Construct as Org
import Data.Foldable (foldl)
import Data.Array ((:))
import Data.Array (head, catMaybes, uncons, singleton) as Array
import Data.Either (Either(..))

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
    = B Org.Block
    | D Org.Drawer
    | P Org.Properties


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
            _.orgf $ foldl applySub init rules
        _ -> Org.empty
    where

        applySub { orgf,target } rule = case Debug.spy "rule" rule of
            Rule "other-keyword-line" [ TextRule "kw-name" kwTitle, TextRule "kw-value" kwValue ] ->
                { orgf : orgf # Org.meta kwTitle kwValue
                , target
                }
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
                        TopLevel -> orgf # Org.append_bl (blockFrom $ _extractWordsRules contentRules) -- TODO: also collect to `ContentTarget` before?
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
                                B block ->
                                    GoesTo { hasLines : true } $ B $ Org.inject_words (Debug.spy "block-words-to-inject" wordsToAdd') block
                                D drawer ->
                                    GoesTo { hasLines : true } $ D $ Org.drawer_add (Debug.spy "drawer-words-to-inject" wordsToAdd') drawer
                                P props ->
                                    target -- FIXME: TODO
                }
            Rule "empty-line" [] ->
                { orgf : orgf # Org.append_bl Org.blank
                , target
                }
            TextRule "horizontal-rule" _ ->
                { orgf : orgf # Org.append_bl Org.hr
                , target
                }
            Rule "block-begin-line" [ TextRule "block-name" name ] ->
                { orgf
                , target : case name of
                    "src"     -> GoesTo { hasLines : false } $ B $ Org.code' [] -- orgf # Org.append_bl
                    "quote"   -> GoesTo { hasLines : false } $ B $ Org.quote [] -- orgf # Org.append_bl
                    "example" -> GoesTo { hasLines : false } $ B $ Org.example [] -- orgf # Org.append_bl
                    "comment" -> GoesTo { hasLines : false } $ B $ Org.bcomment [] -- orgf # Org.append_bl
                    _ -> TopLevel
                }
            Rule "block-begin-line" [ TextRule "block-name" name, TextRule "block-parameters" params ] ->
                { orgf
                , target : case name of
                    "src" -> GoesTo { hasLines : false } $ B $ Org.codeIn' params [] -- # Org.wdoc (Org.snoc_bl $ Org.codeIn' params []) orgf
                    _ -> target
                }
            Rule "block-end-line" [ TextRule "block-name" name ] ->
                { orgf :
                    case target of
                        GoesTo _ (B block) -> orgf # Org.append_bl block
                        _ -> orgf -- should not happen
                , target : TopLevel
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
                in
                { orgf : if isDrawerEnd then _finishDrawer orgf target else orgf
                , target : if not isDrawerEnd then GoesTo { hasLines : false } $ D $ Org.mk_drawer drawerName [] else TopLevel
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
            _ -> { orgf, target }

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
                _ -> orgf -- should not happen

        _extractWordsRules contentRules =
            case Debug.spy "content-rules" contentRules of
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

        applyListItemRule litem rule =
            case Debug.spy "list-rule" rule of
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

        applySecHeadRule sec rule =
            case Debug.spy "sec-head-rule" rule of
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

        applyTimestampRule ts rule =
            case Debug.spy "timestamp-rule" rule of
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

        applySecPlanningRule sec rule =
            case Debug.spy "sec-plan-rule" rule of
                Rule "planning-info" [ Rule "planning-keyword" kwRules, Rule "timestamp" tsRules ] ->
                    case kwRules of
                        [ Rule "planning-kw-deadline" [] ]  -> sec # Org.deadline (buildTimeStamp tsRules)
                        [ Rule "planning-kw-scheduled" [] ] -> sec # Org.schedule (buildTimeStamp tsRules)
                        [ Rule "planning-kw-closed" [] ]    -> sec # Org.close    (buildTimeStamp tsRules)
                        _ -> sec
                _ -> sec

        buildTimeStamp tsRules =
            case tsRules of
                [ Rule "timestamp-active"   [ Rule "ts-inner" innerTsRules ] ] -> foldl applyTimestampRule (Org.adate $ Org.d 0 0 0) innerTsRules
                [ Rule "timestamp-inactive" [ Rule "ts-inner" innerTsRules ] ] -> foldl applyTimestampRule (Org.idate $ Org.d 0 0 0) innerTsRules
                _ -> Org.adate $ Org.d 0 0 0

        toWordsFromRule rule =
            case Debug.spy "word-rule" rule of
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
                Rule "link-format" [ Rule "link" linkRules, TextRule "link-description" linkDescr ] ->
                    let
                        _ = Debug.spyWith "link-rules" show linkRules
                    in Just $ Org.to (createLinkTarget linkRules) linkDescr
                Rule "link-format" [ Rule "link" linkRules ] ->
                    let
                        _ = Debug.spyWith "link-rules" show linkRules
                    in Just $ Org.ref (createLinkTarget linkRules)
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
                _ = Debug.spyWith "matches" show rec.data
                mbRootRule = Debug.spyWith "rules" show
                        $ case Array.uncons rec.data of
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