module Test.Utils where

import Prelude

import Effect.Exception (Error)
import Effect.Class (class MonadEffect)

import Control.Monad.Error.Class (class MonadThrow)

import Data.Tuple.Nested ((/\), type (/\))
import Data.FoldableWithIndex (foldlWithIndex, class FoldableWithIndex)
import Data.Text.Diff (class Diffable)
import Data.Text.Diff (diffCompare, diffStackCompare, onlyDifferentCompare) as Diff

import Test.Spec (Spec, SpecT, it, class Example)
import Test.Spec.Assertions (shouldEqual, fail) as TA

-- Monad m ⇒ Example t arg g ⇒ String → t → SpecT g arg m Unit

--helper :: forall a t. { title :: Int -> a -> String, spec :: a -> t } -> Array a -> Spec Unit
helper
    :: forall
        (f ∷ Type -> Type)
        (idx ∷ Type)
        (a ∷ Type)
        (m ∷ Type -> Type)
        (t ∷ Type)
        (arg ∷ Type)
        (g ∷ Type -> Type)
     . FoldableWithIndex idx f
    => Apply m
    => Monad m
    => Example t arg g
    => { title :: idx -> a -> String
       , spec :: a -> t
       }
    -> f a
    -> SpecT g arg m Unit
helper { title, spec } =
    foldlWithIndex
        (\idx prev a -> do
            prev
            *>
            (it (title idx a) $
                spec a
            )
        )
        (pure unit)


data Comparator
    = Stack
    | Zip
    | Plain
    | OnlyDifferent
    | Silent


compareBy :: forall m. MonadEffect m ⇒ MonadThrow Error m ⇒ Comparator -> (String -> String -> m Unit)
compareBy Stack = Diff.diffStackCompare
compareBy Zip = Diff.diffCompare
compareBy OnlyDifferent = Diff.onlyDifferentCompare
compareBy Plain = TA.shouldEqual
compareBy Silent = \sA sB -> if sA == sB then pure unit else TA.fail "x"


shouldEqual :: forall m. MonadEffect m ⇒ MonadThrow Error m ⇒ String -> String -> m Unit
shouldEqual = compareBy Silent