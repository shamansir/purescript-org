module Data.Text.Format.Org.Ebnf where

import Prelude

import Debug as Debug

import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.String (joinWith, length) as String
import Data.Text.Format.Org.Types (OrgFile)
import Data.Text.Format.Org.Types as Org
import Data.Text.Format.Org.Construct as Org
import Data.Foldable (foldl)
import Data.Array ((:))
import Data.Array (catMaybes, uncons) as Array

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


extractFromRoot :: Rule -> OrgFile
extractFromRoot =
    case _ of
        Rule "S" rules ->
            foldl applySub Org.empty rules
        _ -> Org.empty
    where
        applySub orgf rule = case Debug.spy "rule" rule of
            Rule "other-keyword-line" [ TextRule "kw-name" kwTitle, TextRule "kw-value" kwValue ] ->
                orgf # Org.meta kwTitle kwValue
            Rule "headline" hlRules ->
                fromMaybe orgf $ Array.uncons hlRules <#> \{ head, tail } ->
                    case head of
                        TextRule "stars" val ->
                            Org.wdoc
                                (Org.snoc_sec $ foldl applySecHeadRule (Org.sece (String.length val) []) tail)
                                orgf
                        _ -> orgf
            Rule "content-line" contentRules ->
                Org.wdoc (Org.wlast_sec $ \sec -> foldl applySecContentRule sec contentRules) orgf
            Rule "empty-line" [] ->
                Org.wdoc (Org.wlast_sec $ Org.sec_wdoc $ Org.snoc_bl $ Org.blank) orgf
            _ -> orgf
        toWordRule rule =
            case Debug.spy "word-rule" rule of
                TextRule "text-normal" textVal ->
                     Just $ Org.Plain textVal
                _ -> Nothing
        applySecContentRule sec rule =
            case Debug.spy "sec-rule" rule of
                Rule "text" wordsRules ->
                    sec # Org.sec_wdoc (Org.snoc_bl $ Org.para $ Array.catMaybes $ toWordRule <$> wordsRules)
                _ -> sec
        applySecHeadRule sec rule =
            case Debug.spy "sec-rule" rule of
                Rule "text" wordsRules -> sec # Org.sec_head (Array.catMaybes $ toWordRule <$> wordsRules)
                _ -> sec



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