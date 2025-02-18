module Test.Org.Samples where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.String.CodeUnits (fromCharArray) as String

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

import Yoga.JSON (readJSON, E)

import Test.Spec.Assertions (shouldEqual, fail)

import Data.Text.Format.Org.Types (OrgFile)
import Data.Text.Format.Org.Construct as Org
import Data.Text.Format.Org.Parse.Ebnf
import Data.Text.Format.Org.Parse.Ebnf as Ebnf

import Org.Test.Test01 as Test01
import Org.Test.Test02a as Test02a
import Org.Test.Test02b as Test02b
import Org.Test.Test03a as Test03a
import Org.Test.Test03b as Test03b
import Org.Test.Test03c as Test03c
import Org.Test.Test03d as Test03d
import Org.Test.Test03e as Test03e
import Org.Test.Test04a as Test04a
import Org.Test.Test04b as Test04b
import Org.Test.Test04c as Test04c
import Org.Test.Test04d as Test04d
import Org.Test.Test04e as Test04e
import Org.Test.Test04f as Test04f
import Org.Test.Test04g as Test04g
import Org.Test.Test04h as Test04h
import Org.Test.Test04i as Test04i
import Org.Test.Test06a as Test06a


-- TODO: use Test.Spec.Util.Assertions from `noodle` or `purescript-text-formatting`

ebnfGrammarSrc = "./src/Data/Text/Format/Org/Parse/org.ebnf" :: String
testDir = "./test/examples/org-test/" :: String


type Sample =
    { slug :: String
    , friendly :: String
    , file :: Effect OrgFile
    }


data Category
    = Category Int (Maybe Char)


data Episode = Episode { season :: Int, episode :: Int, mbChar :: Maybe Char }


newtype Slug = Slug String


newtype Friendly = Friendly String


pursifiedSamples :: Array Sample
pursifiedSamples =

    [ mkPursSample Test01.test (ce 1) (s "empty") $ f "empty sample"

    , mkPursSample Test02a.test (c 2 'a') (s "meta")         $ f "meta sample"
    , mkPursSample Test02b.test (c 2 'b') (s "meta-special") $ f "special meta sample"

    , mkPursSample Test03a.test (c 3 'a') (s "headings-with-no-content") $ f "basic headings and levels"
    , mkPursSample Test03b.test (c 3 'b') (s "headings-with-content")    $ f "heading with some content"
    , mkPursSample Test03c.test (c 3 'c') (s "headings-with-planning")   $ f "heading with planning"
    , mkPursSample Test03d.test (c 3 'd') (s "headings-with-tags")       $ f "heading with tags"
    , mkPursSample Test03e.test (c 3 'e') (s "basic-structuring")        $ f "basic structure"

    , mkPursSample Test04a.test (c 4 'a') (s "formatting-headings")  $ f "formatting: headings"
    , mkPursSample Test04b.test (c 4 'b') (s "formatting-blocks")    $ f "formatting: blocks"
    , mkPursSample Test04c.test (c 4 'c') (s "formatting-lists")     $ f "formatting: lists"
    , mkPursSample Test04d.test (c 4 'd') (s "formatting-tables")    $ f "formatting: tables"
    , mkPursSample Test04e.test (c 4 'e') (s "formatting-footnotes") $ f "formatting: footnotes"
    , mkPursSample Test04f.test (c 4 'f') (s "formatting-comments")  $ f "formatting: comments"
    , mkPursSample Test04g.test (c 4 'g') (s "formatting-dates")     $ f "formatting: dates"
    , mkPursSample Test04h.test (c 4 'h') (s "formatting-properties-and-keywords") $ f "formatting: properties & keywords"
    , mkPursSample Test04i.test (c 4 'i') (s "formatting-drawers")   $ f "formatting: drawers"
    , mkPursSample Test04i.test (c 4 'i') (s "formatting-drawers")   $ f "formatting: drawers"

    -- , mkPursSample Test06a.test (c 6 'a') (s "properties-and-drawers") $ f "properties and drawers"
    ]

    where
        f = Friendly
        s = Slug
        c n = Category n <<< Just
        ce n = Category n Nothing


parsedSamples :: Array Sample
parsedSamples =

    -- From OrgMode tutorial by Rainer König: https://youtube.com/playlist?list=PLVtKhBrRV_ZkPnBtt_TD1Cs9PJlU0IIdE&si=Uo4uuf4-RM0ImLjK

    [ mkEbnfSample' noRefresh (c 5 'a') (ep 1 1) (s "headlines") (f "headlines : expanded")
    , mkEbnfSample' noRefresh (c 5 'b') (ep' 1 2 'a') (s "todo-keywords.v1") (f "TODO keywords. v1")
    , mkEbnfSample' noRefresh (c 5 'c') (ep' 1 2 'b') (s "todo-keywords.v2") (f "TODO keywords. v2")
    , mkEbnfSample' noRefresh (c 5 'd') (ep' 1 3 'a') (s "schedule") (f "types of schedules")
    , mkEbnfSample' noRefresh (c 5 'e') (ep 1 4) (s "repeating") (f "task repeating")
    , mkEbnfSample' noRefresh (c 5 'f') (ep 1 5) (s "checklists") (f "checklists")
    , mkEbnfSample' noRefresh (c 5 'g') (ep 2 1) (s "tags") (f "tags")
    , mkEbnfSample' noRefresh (c 5 'h') (ep 2 4) (s "drawers-logging") (f "logging in drawers")
    , mkEbnfSample' noRefresh (c 5 'i') (ep' 3 4 'a') (s "properties-drawer") (f "properties drawer")
    -- , mkEbnfSample' noRefresh (c 5 'j') (ep' 3 4 'b') (s "template") (f "template") -- fails (and it's ok)
    , mkEbnfSample' noRefresh (c 5 'k') (ep 4 1) (s "ordered-tasks") (f "ordered tasks")
    , mkEbnfSample' noRefresh (c 5 'l') (ep 4 2) (s "timers") (f "timers")
    , mkEbnfSample' noRefresh (c 5 'm') (ep 5 4) (s "priorities") (f "priorities")
    , mkEbnfSample noRefresh (c 6 'a') (s "properties-and-drawers") (f "properties and drawers")
    -- , mkEbnfSample noRefresh (c 7 'a') (s "org-syntax-cheatsheet") (f "ORG syntax cheatsheet") -- fails
    -- , mkEbnfSample noRefresh (c 7 'b') (s "organice.sample") (f "ORG Sample from organice") -- fails
    , mkEbnfSample noRefresh (c 7 'c') (s "simple") (f "ORG in a simplest") -- fails
    , mkEbnfSample noRefresh (c 7 'd') (s "test_a") (f "different features") -- fails
    , mkEbnfSample noRefresh (c 7 'e') (s "test") (f "different features v.2") -- fails
    ]

    where
        f = Friendly
        s = Slug
        c n = Category n <<< Just
        ce n = Category n Nothing
        ep season episode = Episode { season, episode, mbChar : Nothing }
        ep' season episode ch = Episode { season, episode, mbChar : Just ch }


samples :: Array Sample
samples =
    -- [ mkEbnfSample' refresh
    --     (Category 5 $ Just 'h')
    --     (Episode { season : 2, episode : 4, mbChar : Nothing })
    --     (Slug "drawers-logging")
    --     (Friendly "logging in drawers") ]
    pursifiedSamples <> parsedSamples


charToString char = String.fromCharArray [ char ]


mkSample :: Effect OrgFile -> Category -> Slug -> Friendly -> Sample
mkSample fileEff cat@(Category n mbChar) s@(Slug slug) (Friendly friendly) =
    { file : fileEff, slug : mkFileSlug cat s, friendly : show n <> ". works with " <> friendly <> frsuffix }
    where
        frsuffix = case mbChar of
            Just char -> " (" <> charToString char <> ")."
            Nothing -> "."


mkSample' :: Effect OrgFile -> Slug -> Friendly -> Sample
mkSample' fileEff (Slug slug) (Friendly friendly) =
    { file : fileEff, slug, friendly : "Works with " <> friendly }


mkFileSlug :: Category -> Slug -> String
mkFileSlug (Category n mbChar) (Slug slug) =
    prefix <> "-" <> slug
    where
        prefix = leadingZero n <> case mbChar of
            Just char -> charToString char
            Nothing -> ""


mkPursSample :: OrgFile -> Category -> Slug -> Friendly -> Sample
mkPursSample file = mkSample $ pure file


mkEbnfSample :: RefreshEbnfFile -> Category -> Slug -> Friendly -> Sample
mkEbnfSample ref cat slug = mkSample (parseEbnf (refreshToBool ref) $ mkFileSlug cat slug) cat slug


data RefreshEbnfFile
    = Yes
    | No


refresh = Yes
noRefresh = No


refreshToBool = case _ of
    Yes -> true
    No -> false


instance Show Episode where
    show (Episode def) =
        "s" <> leadingZero def.season <>
        "e" <> leadingZero def.episode <>
        case def.mbChar of
            Just ch -> charToString ch
            Nothing -> ""


mkEbnfSample' :: RefreshEbnfFile -> Category -> Episode -> Slug -> Friendly -> Sample
mkEbnfSample' ref cat episode (Slug slug) (Friendly friendly) =
    mkEbnfSample ref cat
        (Slug $ show episode <> "-" <> slug)
        (Friendly $ show episode <> " — " <> slug)


parseEbnf :: Boolean -> String -> Effect OrgFile
parseEbnf updateEbnf fileSlug = do
    -- grammarText <- readTextFile UTF8 ebnfGrammarSrc
    -- orgTestText <- readTextFile UTF8 (testDir <> fileSlug <> ".org")
    when updateEbnf $ Ebnf.writeEbnfJsonFor $ Ebnf.TestFileSlug fileSlug -- ENABLE to write `ebnf.json` result to file before
    jsonEbnfStr <- readTextFile UTF8 (testDir <> fileSlug <> ".ebnf.json")
    -- let jsonEbnfStr = Ebnf.parseOrgWithEbnf (Ebnf.EbnfGrammar grammarText) (Ebnf.OrgText orgTestText)
    let eOrgFile = (readJSON jsonEbnfStr :: E FromEbnf)
    case eOrgFile of
        Left errors -> do
            fail "Parse failed"
            pure Org.empty
        Right (FromEbnf orgFile)-> do
            pure orgFile




leadingZero :: Int -> String
leadingZero n | n < 10 = "0" <> show n
leadingZero n | otherwise = show n
