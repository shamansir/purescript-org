module Test.Org.Samples where

import Prelude


import Data.Text.Format.Org.Types (OrgFile)

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


-- TODO: use Test.Spec.Util.Assertions from `noodle` or `purescript-text-formatting`


type Sample =
    { slug :: String
    , friendly :: String
    , file :: OrgFile
    }


data IndentMode
    = ZeroIndent
    | SmartIndent


samples :: IndentMode -> Array Sample
samples = case _ of
    ZeroIndent -> zeroIndentedSamples
    SmartIndent -> smartIndentedSamples


zeroIndentedSamples :: Array Sample
zeroIndentedSamples =
    [ { file : Test01.test, slug : "01-empty", friendly : "01. works with the empty sample" }
    , { file : Test02a.test, slug : "02a-meta", friendly : "02. works with the meta sample (a)" }
    , { file : Test02b.test, slug : "02b-meta-special", friendly : "02. works with the special meta sample (b)" }
    , { file : Test03a.test, slug : "03a-headings-with-no-content", friendly : "03. works with basic headings and levels (a)" }
    , { file : Test03b.test, slug : "03b-headings-with-content", friendly : "03. works with heading with some content (b)" }
    , { file : Test03c.test, slug : "03c-headings-with-planning", friendly : "03. works with headings with planning (c)" }
    , { file : Test03d.test, slug : "03d-headings-with-tags", friendly : "03. works with headings with tags (d)" }
    , { file : Test03e.test, slug : "03e-basic-structuring", friendly : "03. works with basic structure (e)" }
    , { file : Test04a.test, slug : "04a-formatting-headings", friendly : "04. formatting: headings (a)" }
    , { file : Test04b.test, slug : "04b-formatting-blocks", friendly : "04. formatting: blocks (b)" }
    , { file : Test04c.test, slug : "04c-formatting-lists", friendly : "04. formatting: lists (c)" }
    -- , { file : Test04d.test, slug : "04d-formatting-tables", friendly : "04. formatting: tables (d)" }
    , { file : Test04e.test, slug : "04e-formatting-footnotes", friendly : "04. formatting: footnotes (e)" }
    -- , { file : Test04f.test, slug : "04f-formatting-comments", friendly : "04. formatting: comments (f)" }
    -- , { file : Test04g.test, slug : "04g-formatting-dates", friendly : "04. formatting: dates (g)" }
    -- , { file : Test04h.test, slug : "04h-formatting-properties-and-keywords", friendly : "04. formatting: properties & keywords (h)" }
    -- , { file : Test04i.test, slug : "04i-formatting-drawers", friendly : "04. formatting: drawers (i)" }
    ]


smartIndentedSamples :: Array Sample -- TODO: include only different samples
smartIndentedSamples =
    [ { file : Test01.test, slug : "01-empty", friendly : "01. works with the syntax sample" }
    , { file : Test02a.test, slug : "02a-meta", friendly : "02. works with the meta sample (a)" }
    , { file : Test02b.test, slug : "02b-meta-special", friendly : "02. works with the special meta sample (b)" }
    , { file : Test03a.test, slug : "03a-headings-with-no-content", friendly : "03. works with basic headings and levels (a)" }
    , { file : Test03b.test, slug : "03b-headings-with-content.indented", friendly : "03. works with heading with some content (b)" }
    , { file : Test03c.test, slug : "03c-headings-with-planning", friendly : "03. works with headings with planning (c)" }
    , { file : Test03d.test, slug : "03d-headings-with-tags", friendly : "03. works with headings with tags (d)" }
    , { file : Test03e.test, slug : "03e-basic-structuring.indented", friendly : "03. works with basic structure (e)" }
    , { file : Test04a.test, slug : "04a-formatting-headings.indented", friendly : "04. formatting: headings (a)" }
    , { file : Test04b.test, slug : "04b-formatting-blocks", friendly : "04. formatting: blocks (b)" }
    , { file : Test04c.test, slug : "04c-formatting-lists.indented", friendly : "04. formatting: lists (c)" }
    , { file : Test04d.test, slug : "04d-formatting-tables", friendly : "04. formatting: tables (d)" }
    , { file : Test04e.test, slug : "04e-formatting-footnotes", friendly : "04. formatting: footnotes (e)" }
    , { file : Test04f.test, slug : "04f-formatting-comments.indented", friendly : "04. formatting: comments (f)" }
    , { file : Test04g.test, slug : "04g-formatting-dates", friendly : "04. formatting: dates (g)" }
    , { file : Test04h.test, slug : "04h-formatting-properties-and-keywords.indented", friendly : "04. formatting: properties & keywords (h)" }
    , { file : Test04i.test, slug : "04i-formatting-drawers.indented", friendly : "04. formatting: drawers (i)" }
    ]