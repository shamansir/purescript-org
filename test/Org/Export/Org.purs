module Test.Org.Export.Org where

import Prelude

import Effect.Class (liftEffect, class MonadEffect)
import Control.Monad.Error.Class (class MonadThrow)

import Data.Text.Doc as D
import Data.Text.Format.Org.Types (OrgFile)
import Data.Text.Format.Org.Render as R

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)

import Test.Utils as U

import Test.Org.Export.Samples (IndentMode(..), samples)


spec :: Spec Unit
spec = do

  describe "export to ORG" $ do

    {-
    it "works with the empty .org files" $ do
        (read_ $ write Org.empty) `shouldEqual` (Just Org.empty)
    -}

    U.helper
        { title : const (_.friendly >>> (<>) "Zero: ")
        , spec : \{ slug, file } -> qtest slug R.defaultRO file
        }
        $ samples ZeroIndent

    -- it "---" $ pure unit

    U.helper
        { title : const (_.friendly >>> (<>) "Deep: ")
        , spec : \{ slug, file } -> qtest slug R.indentByDeep file
        }
        $ samples SmartIndent


renderOptions :: D.Options
renderOptions = { break : D.All, indent : D.Spaces 1 }


qtest
    :: forall (m ∷ Type -> Type)
     . Bind m => MonadEffect m => MonadThrow _ m
    => String -> R.RenderOptions -> OrgFile -> m Unit
qtest fileSlug ro orgFile = do
    orgTestText <- liftEffect $ readTextFile UTF8 ("./test/examples/org-test/" <> fileSlug <> ".org")
    (D.render renderOptions $ R.layoutWith ro orgFile)
            `shouldEqual` orgTestText