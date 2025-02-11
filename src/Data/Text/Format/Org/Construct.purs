module Data.Text.Format.Org.Construct where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map (empty, insert, size, fromFoldable) as Map
import Data.Enum (class BoundedEnum)
import Data.Tuple (curry, uncurry)
import Data.Foldable (class Foldable)
import Data.Unfoldable (class Unfoldable)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array ((:))
import Data.Array (toUnfoldable, length, mapWithIndex, singleton, delete, foldl, foldr, snoc, intersperse) as Array
import Data.Array.NonEmpty as NEA
import Data.String (joinWith, toUpper) as String
import Data.Newtype (unwrap, wrap)
import Data.Time (Time(..), hour, minute, second) as T
import Data.Date (Date, canonicalDate) as T
import Data.Enum (toEnum, fromEnum)


import Data.Text.Format.Org.Types
import Data.Text.Format.Org.Path (Path)
import Data.Text.Format.Org.Path as P
import Data.Text.Format.Org.Keywords as Keywords


type Property = Keywords.Keyword String


prop :: String -> String -> Property
prop = Keywords.kw


empty :: OrgFile
empty = f emptyDoc


emptyDoc :: OrgDoc
emptyDoc =
    OrgDoc { zeroth : [], sections : [] }


f :: OrgDoc -> OrgFile
f = f_ []


f_ :: Array Property -> OrgDoc -> OrgFile
f_ props doc =
    OrgFile { meta : Keywords.make props, doc }


ds :: Array Section -> OrgDoc
ds sections = OrgDoc { zeroth : [], sections }


ds1 :: Section -> OrgDoc
ds1 = ds <<< Array.singleton


db :: Array Block -> OrgDoc
db blocks = OrgDoc { zeroth : blocks, sections : [] }


db1 :: Block -> OrgDoc
db1 = db <<< Array.singleton


dbs :: Array Block -> Array Section -> OrgDoc
dbs blocks sections = OrgDoc { zeroth : blocks, sections }


meta :: String -> String -> OrgFile -> OrgFile
meta prop val (OrgFile { meta, doc }) =
    OrgFile
        { meta : Keywords.qpush meta prop val
        , doc : doc
        }


meta_ :: String -> String -> OrgFile -> OrgFile
meta_ prop val (OrgFile { meta, doc }) =
    OrgFile
        { meta : meta # Keywords.qpile prop val
        , doc : doc
        }


data ProgressStep = Progress String
data FinishStep = Finish String


todoSequence :: Array ProgressStep -> Array FinishStep -> OrgFile -> OrgFile
todoSequence pss fss =
    meta "SEQ_TODO" $ String.joinWith " " (pssToString <$> pss) <> " | " <> String.joinWith " " (fssToString <$> fss)
    where
     pssToString (Progress str) = String.toUpper str
     fssToString (Finish str) = String.toUpper str


quote :: Array Words -> Block
quote = Of Quote <<< __neafws


example :: Array Words -> Block
example = Of Example <<< __neafws


code :: String -> Block
code str = Of (Code Nothing) $ __neafws [ Plain str ]
-- code = Of $ Code Nothing


codeIn :: String -> String -> Block
codeIn lang str = Of (Code $ Just $ Language lang) $ __neafws [ Plain str ]
-- codeIn = Of <<< Code <<< Just <<< ?wh <<< Language


bcomment :: Array String -> Block
bcomment ws = Of Comment $ __neafws $ Array.intersperse Break $ Plain <$> ws


list :: ListType -> Array Item -> Block
list lt = List <<< __items lt


item :: Array Words -> Item
item ws =
    Item
        { check : Nothing, counter : Nothing, tag : Nothing, drawers : [] }
        (__neafws ws)
        Nothing


item1 :: Words -> Item
item1 = item <<< Array.singleton


table :: Array TableRow -> Block
table = Table Nothing <<< __neaf tbreak


tbreak :: TableRow
tbreak = BreakT


tcol :: Array Words -> TableColumn
tcol = Column <<< __neafws


tcol1 :: Words -> TableColumn
tcol1 = tcol <<< Array.singleton


tskip :: TableColumn
tskip = Empty


trow :: Array TableColumn -> TableRow
trow = Row <<< __neaf Empty
-- trow = Row <<< __neaf Empty <<< map toColumn
--     where 
--         toColumn [] = Empty
--         toColumn arr = Column $ __neafws arr


qrow :: Array Words -> TableRow 
qrow items = trow $ tcol1 <$> items


qrow' :: Array String -> TableRow
qrow' items = qrow $ text <$> items


check :: Check -> Item  -> Item
check ch (Item opts ws is) =
    Item
        (opts { check = Just ch })
        ws
        is


count :: Int -> Item  -> Item
count cnt (Item opts ws is) =
    Item
        (opts { counter = Just $ Counter cnt })
        ws
        is


sub :: ListType -> Array Item -> Item -> Item
sub lt is (Item opts ws _) =
    Item
        opts
        ws
        $ Just
        $ __items lt is


tagi :: String -> Item -> Item
tagi tag (Item opts ws is) =
    Item
        (opts { tag = Just tag })
        ws
        is


idrawer :: String -> Array Words -> Item -> Item
idrawer name content (Item rec iws inner) = 
    Item 
        (rec 
            { drawers = 
                Array.snoc rec.drawers $ Drawer { name, content : __neafws content } 
            }
        )
        iws 
        inner


idrawer1 :: String -> Words -> Item -> Item
idrawer1 name = idrawer name <<< Array.singleton


bdrawer :: String -> Array Words -> Block
bdrawer name content = IsDrawer $ Drawer { name, content : __neafws content }


bdrawer1 :: String -> Words -> Block
bdrawer1 name = bdrawer name <<< Array.singleton


para :: Array Words -> Block
para = Paragraph <<< __neafws


para1 :: Words -> Block
para1 = Paragraph <<< NEA.singleton


blank :: Block
blank = para1 $ text ""


bold :: MarkupKey
bold = Bold


italic :: MarkupKey
italic = Italic


hilite :: MarkupKey
hilite = Highlight


under :: MarkupKey
under = Underline


verbatim :: MarkupKey
verbatim = Verbatim


icode :: MarkupKey
icode = InlineCode


strike :: MarkupKey
strike = Strike


both :: MarkupKey -> MarkupKey -> MarkupKey
both = And


b :: String -> Words
b = marked bold


i :: String -> Words
i = marked Italic


hl :: String -> Words
hl = marked Highlight


u :: String -> Words
u = marked Underline


v :: String -> Words
v = marked Verbatim


ic :: String -> Words
ic = marked InlineCode


s :: String -> Words
s = marked Strike


a :: String -> String -> Words
a = to <<< Remote


a' :: String -> Words
a' = ref <<< Remote


to :: LinkTarget -> String -> Words
to lt = Link lt <<< Just


ref :: LinkTarget -> Words
ref lt = Link lt Nothing


rem :: String -> LinkTarget
rem = Remote


loc :: String -> LinkTarget
loc = Local


head :: String -> LinkTarget
head = Heading


irem :: String -> ImageSource
irem = RemoteSrc


iloc :: String -> ImageSource
iloc = LocalSrc


img :: String -> Words
img = Image <<< RemoteSrc


img_ :: ImageSource -> Words
img_ = Image


text :: String -> Words
text = Plain


br :: Words
br = Break


marked :: MarkupKey -> String -> Words
marked = Marked


icomment :: String -> Words
icomment = marked $ Inline IComment


{-
atime :: T.Time -> OrgDateTime
atime = ?wh


itime :: T.Time -> OrgDateTime
itime = ?wh
-}


t :: Int -> Int -> T.Time
t h m = T.Time
            (toEnum h # fromMaybe bottom)
            (toEnum m # fromMaybe bottom)
            bottom
            bottom


d :: Int -> Int -> Int -> T.Date
d year month day =
    T.canonicalDate
        (toEnum year # fromMaybe bottom)
        (toEnum month # fromMaybe bottom)
        (toEnum day # fromMaybe bottom)


clock :: T.Time -> Words
clock t = ClockW $ Clock
            { hour : fromEnum $ T.hour t
            , minute : fromEnum $ T.minute t
            , second : Just $ fromEnum $ T.second t
            }



adate :: T.Date -> OrgDateTime
adate date =
    OrgDateTime
        { date
        , time : Nothing
        , active : true
        , delay : Nothing
        , repeat : Nothing
        }


idate :: T.Date -> OrgDateTime
idate =
    adate >>> unwrap >>> _ { active = false } >>> wrap


adatetime :: T.Date -> T.Time -> OrgDateTime
adatetime date time =
    adate date # at_ time


idatetime :: T.Date -> T.Time -> OrgDateTime
idatetime date =
    adatetime date >>> unwrap >>> _ { active = false } >>> wrap


at_ :: T.Time -> OrgDateTime -> OrgDateTime
at_ time =
    unwrap
        >>> _ { time = Just $ at_r time }
        >>> wrap


fromto :: T.Time -> T.Time -> OrgDateTime -> OrgDateTime
fromto start end =
    unwrap
        >>> _ { time = Just $ fromto_r start end }
        >>> wrap


chdate :: T.Date -> OrgDateTime -> OrgDateTime
chdate date =
    unwrap
        >>> _ { date = date }
        >>> wrap


afromto :: T.Date -> T.Time -> T.Time -> OrgDateTime
afromto date start end =
    adate date # fromto start end


ifromto :: T.Date -> T.Time -> T.Time -> OrgDateTime
ifromto date start end =
    idate date # fromto start end


at :: OrgDateTime -> Words
at start = DateTime { start, end : Nothing }


range :: OrgDateTime -> OrgDateTime -> Words
range start end = DateTime { start, end : Just end }


sec :: Int -> Array Words -> OrgDoc -> Section
sec level heading doc =
    Section
        { todo : Nothing
        , priority : Nothing
        , cookie : Nothing
        , check : Nothing
        , heading : __neaf (text "") heading
        , level
        , tags : []
        , planning :
            Planning
                { closed : Nothing
                , deadline : Nothing
                , scheduled : Nothing
                , timestamp : Nothing
                }
        , props : Keywords.empty
        , drawers : []
        , comment : false
        , doc
        }


sec1 :: Int -> Words -> OrgDoc -> Section
sec1 l = sec l <<< Array.singleton


sece :: Int -> Array Words -> Section
sece l ws = sec l ws emptyDoc


sece1 :: Int -> Words -> Section
sece1 l = sece l <<< Array.singleton


ssec :: Int -> Array Words -> OrgDoc -> OrgDoc
ssec level heading doc =
    ds [ sec level heading doc ]


ssec1 :: Int -> Words -> OrgDoc -> OrgDoc
ssec1 l = ssec l <<< Array.singleton


set :: Todo -> Section -> Section
set val = __qset _ { todo = Just val }


priority :: Priority -> Section -> Section
priority val = __qset _ { priority = Just val }


low :: Section -> Section
low = identity -- TODO


hi :: Section -> Section
hi = identity -- TODO


cookie :: Cookie -> Section -> Section
cookie val = __qset _ { cookie = Just val }


tag :: String -> Section -> Section
tag s = __qset $ \sec -> sec { tags = s : sec.tags }


untag :: String -> Section -> Section
untag s = __qset $ \sec -> sec { tags = sec.tags # Array.delete s }


level :: Int -> Section -> Section
level val = __qset _ { level = val }


inc :: Section -> Section
inc = __qset $ \sec -> sec { level = min 20 $ sec.level + 1 }


dec :: Section -> Section
dec = __qset $ \sec -> sec { level = max 0 $ sec.level - 1 }


close :: OrgDateTime -> Section -> Section
close dt = __qplan $ _ { closed = Just dt }


deadline :: OrgDateTime -> Section -> Section
deadline dt = __qplan $ _ { deadline = Just dt }


schedule :: OrgDateTime -> Section -> Section
schedule dt = __qplan $ _ { scheduled = Just dt }


timestamp :: OrgDateTime -> Section -> Section
timestamp dt = __qplan $ _ { timestamp = Just dt }


wprop :: String -> String -> Section -> Section
wprop prop value = __qset $ \sec -> sec { props = Keywords.qpush sec.props prop value }


wprop_ :: String -> Section -> Section
wprop_ prop = __qset $ \sec -> sec { props = Keywords.qpushon sec.props prop }


drawer :: String -> Array Words -> Section -> Section
drawer name content = __qset $ \sec -> sec { drawers = Drawer { name, content : __neafws content } : sec.drawers } -- FIXME


drawer1 :: String -> Words -> Section -> Section
drawer1 name content = __qset $ \sec -> sec { drawers = Drawer { name, content : NEA.singleton content } : sec.drawers } -- FIXME


note :: String -> Section -> Section
note _ = identity  -- FIXME -- LOGBOOK


comment :: Section -> Section
comment = __qset _ { comment = true }


diary :: String -> Words
diary expr = DiaryW $ Diary { expr, time : Nothing }


diary_r :: String -> OrgTimeRange -> Words
diary_r expr range = DiaryW $ Diary { expr, time : Just range }


at_r :: T.Time -> OrgTimeRange
at_r time = OrgTimeRange { start : time, end : Nothing }


fromto_r :: T.Time -> T.Time -> OrgTimeRange
fromto_r start end = OrgTimeRange { start : start, end : Just end }


repeat :: RepeaterMode -> Int -> Interval -> OrgDateTime -> OrgDateTime
repeat mode value interval =
    unwrap
        >>> _ { repeat = Just $
                    wrap { mode, value, interval, with : Nothing }
              }
        >>> wrap


rwith :: Int -> Interval -> OrgDateTime -> OrgDateTime
rwith value interval =
    unwrap
        >>> (\r -> r { repeat = r.repeat <#> updateWith })
        >>> wrap
    where
        updateWith = unwrap >>> _ { with = Just { value, interval } } >>> wrap



delay :: DelayMode -> Int -> Interval -> OrgDateTime -> OrgDateTime
delay mode value interval =
    unwrap
        >>> _ { delay = Just $
                    wrap { mode, value, interval }
              }
        >>> wrap


fn :: String -> Words
fn label = FootnoteRef { label, def : Nothing }


fndef :: String -> String -> Words
fndef label def = FootnoteRef { label, def : Just def }


fndef' :: String -> Words
fndef' def = FootnoteRef { label : "", def : Just def }


fn_ :: String -> Array Words -> Block
fn_ label = Footnote label <<< __neafws


hr :: Block
hr = HRule


fw :: Array Words -> Block
fw = FixedWidth <<< __neafws


kw :: String -> String -> Keyword
kw = Keywords.kw


kwopt :: String -> String -> String -> Keyword
kwopt = Keywords.kwoptv


with_kw :: String -> String -> Block -> Block
with_kw name value = WithKeyword $ kw name value


with_kws :: Array Keyword -> Block -> Block
with_kws kws block = Array.foldr WithKeyword block kws


lcomment :: Array String -> Block
lcomment = LComment


{-
data At
    = AtMeta String String
    | AtBlock Block
    | AtWords Words
    | AtSection Section
    | AtHeading Words
    | AtProperty String String
    | AtTag String String
    | AtDrawer Drawer
    | AtPriority Priority
    | AtPlanning -- TODO


type Cursor a =
    { path :: Path a
    , parent :: Maybe At
    , current :: At
    }


mapTraverse :: forall a x z. (x -> x -> z) -> (Cursor a -> x) -> x -> OrgFile -> Array z
mapTraverse = ?wh


mapTraverse' :: forall a x z. (x -> x -> z) -> (Cursor a -> x) -> x -> OrgDoc -> Array z
mapTraverse' = ?wh
-}

-- data At :: forall k. (k -> Type) -> k -> Type
data At :: (Type -> Type) -> Type -> Type
data At f a
    = AtBlock Block
    | AtSection Section (f a)


-- traverse ∷ ∀ (x ∷ Type) (b ∷ Type) (a ∷ x) (f ∷ x -> Type). (Array b → Array b → f a) → (At f a → b) → OrgDoc → f a
traverse :: forall b a (f ∷ Type -> Type). Unfoldable f => (f b → f b → f a) -> (At f a -> b) -> OrgDoc -> f a
traverse join f (OrgDoc doc) =
    join (Array.toUnfoldable $ map (f <<< AtBlock) doc.zeroth) (Array.toUnfoldable $ map (f <<< uncurry AtSection <<< deepF) doc.sections)
    where deepF (Section sec) = Section sec /\ traverse join f sec.doc


findBlock :: forall a. BoundedEnum a => OrgFile -> Path a -> Maybe Block
findBlock file path = Nothing


findSection :: forall a. BoundedEnum a => OrgFile -> Path a -> Maybe Section
findSection file path = Nothing


addSection :: forall a. Path a -> OrgFile -> Section -> Path a /\ OrgFile
addSection where_ file _ =
    {- case where_ of
        Root -> -} P.root /\ file


addBlock :: forall a. Path a -> OrgFile -> Block -> Path a /\ OrgFile
addBlock where_ file _ = P.root /\ file


addSection' :: forall a. OrgFile -> Section -> Path a /\ OrgFile
addSection' = addSection P.root


addBlock' :: forall a. OrgFile -> Block -> Path a /\ OrgFile
addBlock' = addBlock P.root


isDocEmpty :: OrgDoc -> Boolean
isDocEmpty (OrgDoc { zeroth, sections }) =
    Array.length zeroth == 0 && Array.length sections == 0


__items :: ListType -> Array Item -> ListItems
__items lt = ListItems lt <<<  __neaf (item [])


__qset f (Section sec) = Section $ f sec --  unwrap >>> f >> wrap


__qplan f =
    __qset $ \sec -> sec { planning = Planning $ f $ case sec.planning of Planning p -> f p }


__neaf def = fromMaybe (NEA.singleton def) <<< NEA.fromArray


__neafws = __neaf $ Plain "\n" -- FIXME: see Types.importWords
