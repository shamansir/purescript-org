module Data.Text.Format.Org.Ebnf where

import Prelude

import Debug as Debug

import Data.Maybe (Maybe(..))
import Data.String (joinWith) as String
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
            case Array.uncons matches of
                Just { head, tail } ->
                    case head of
                        Text rName -> Just $ Rule rName $ Array.catMaybes $ toRule <$> tail
                        _ -> Nothing
                Nothing -> Nothing


extractRoot :: Rule -> OrgFile
extractRoot =
    case _ of
        Rule "S" rules ->
            foldl (\f -> const f) Org.empty rules
        _ -> Org.empty
    -- where
    --     extractSub org = case _ of



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
                _ = Debug.spyWith "rules" show
                        $ case Array.uncons rec.data of
                            Just { head, tail } ->
                                case head of
                                    Text "S" -> Just $ Rule "S" $ Array.catMaybes $ toRule <$> tail
                                    _ -> Nothing
                            Nothing -> Nothing

            in
                FromEbnf $ Org.empty
        )
        -- pure $ FromEbnf $ Org.empty


instance ReadForeign Match where
    readImpl json
        =   (Text    <$> (readImpl json :: F String))
        <|> (Matches <$> (readImpl json :: F (Array Match)))