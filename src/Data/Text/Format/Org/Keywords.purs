module Data.Text.Format.Org.Keywords where

import Prelude

import Foreign (Foreign, F)

import Data.Array as Array
import Data.Maybe (Maybe (..))
import Data.Map (Map)
import Data.Map (empty, insert, size, fromFoldable, toUnfoldable, isEmpty) as Map
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Yoga.JSON (class ReadForeign, readImpl, class WriteForeign)




type KeywordRow v = ( name :: String, value :: Maybe v, default :: Maybe v )
type KeywordRec v = Record (KeywordRow v)
newtype Keyword v = Keyword (KeywordRec v)


newtype Keywords v = Keywords (Array (Keyword v)) -- TODO: Variant r
type OrderedKeywords v = Array (KeywordRec v)
type JsonKeywords v = OrderedKeywords v


derive instance Newtype (Keywords v) _
derive instance Newtype (Keyword v) _


instance Functor Keyword where
    map f =
        unwrap >>> mapRec >>> wrap
        where
            mapRec { name, value, default } =
                { name
                , value : f <$> value
                , default : f <$> default
                }
instance Functor Keywords where
    map f =
        unwrap >>> map (map f) >>> wrap



{- instance (ReadForeign k, ReadForeign v) => ReadForeign (KeywordsOf k v)
    where readImpl f = (readImpl f :: F (Array (Int /\ (k /\ v)))) <#> toKeywords -}


make :: forall v. Array (Keyword v) -> Keywords v
make = wrap


kw :: forall v. String -> v -> Keyword v
kw name value = Keyword { name, value : Just value, default : Nothing }


kwopt :: forall v. String -> v -> Keyword v
kwopt name default = Keyword { name, value : Nothing, default : Just default }


kwoptv :: forall v. String -> v -> v -> Keyword v
kwoptv name value default = Keyword { name, value : Just value, default : Just default }


kwon :: forall v. String -> Keyword v
kwon name = Keyword { name, value : Nothing, default : Nothing }


-- TODO: Maybe value + maybe default
-- TODO: toKeywords converts to `Keyword`s instances


empty :: forall v. Keywords v
empty = wrap []


hasKeywords :: forall v. Keywords v -> Boolean
hasKeywords kws = Array.length (unwrap kws) > 0


isEmpty :: forall v. Keywords v -> Boolean
isEmpty = not hasKeywords


push :: forall v. Keywords v -> Keyword v -> Keywords v
push kws kv = unwrap kws # flip Array.snoc kv # wrap


pile :: forall v. Keyword v -> Keywords v -> Keywords v
pile kv = unwrap >>> Array.cons kv >>> wrap


qpush :: forall v. Keywords v -> String -> v -> Keywords v
qpush kws k v = push kws $ kw k v


qpile :: forall v. String -> v -> Keywords v -> Keywords v
qpile k v = pile $ kw k v


qpushopt :: forall v. Keywords v -> String -> v -> Keywords v
qpushopt kws k ov = push kws $ kwopt k ov


qpileopt :: forall v. String -> v -> Keywords v -> Keywords v
qpileopt k ov = pile $ kwopt k ov


qpushoptv :: forall v. Keywords v -> String -> v -> v -> Keywords v
qpushoptv kws k v ov = push kws $ kwoptv k v ov


qpileoptv :: forall v. String -> v -> v -> Keywords v -> Keywords v
qpileoptv k v ov = pile $ kwoptv k v ov


qpushon :: forall v. Keywords v -> String -> Keywords v
qpushon kws k = push kws $ kwon k


qpileon :: forall v. String -> Keywords v -> Keywords v
qpileon k = pile $ kwon k


{-
insert' :: forall v. Int -> String -> v -> Keywords v -> Keywords v
insert' n k v = unwrap >>> Map.insert (n /\ k) (Just v) >>> wrap
-}


toRec :: forall v. Keyword v -> KeywordRec v
toRec = unwrap


fromRec :: forall v. KeywordRec v -> Keyword v
fromRec = wrap


toKeywords :: forall v. JsonKeywords v -> Keywords v
toKeywords = wrap <<< map wrap


toKeywords' :: forall v. OrderedKeywords v -> Keywords v
toKeywords' = toKeywords


fromKeywords :: forall v. Keywords v -> JsonKeywords v
fromKeywords = map unwrap <<< unwrap


fromKeywords' :: forall v. Keywords v -> OrderedKeywords v
fromKeywords' = fromKeywords


{-
-- t a (t b c) ->  t (t a b) c
_swap3 :: forall a b c. a /\ (b /\ c) -> (a /\ b) /\ c
_swap3 (a /\ (b /\ c)) = (a /\ b) /\ c


-- t (t a b) c ->  t a (t b c)
_bswap3 :: forall a b c. (a /\ b) /\ c -> a /\ (b /\ c)
_bswap3 ((a /\ b) /\ c) = a /\ (b /\ c)
-}
