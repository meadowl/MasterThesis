#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
-- invoke : stack Lens.hs
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

import Test.Hspec

data Entity where
  Deity :: String -> Entity
  Human :: String -> Entity
  deriving (Show,Eq)

data Authority where
    Commoner :: Authority
    Royalty :: Authority
    Noble :: Authority
    Godhood :: Authority
    deriving (Show,Eq)

data Creation = Creation {
    _entity :: !Entity
    , _title :: String
    , _nation :: String
    , _rank :: Int
    , _authority :: Authority}
    deriving (Show, Eq)
    --, _pateron :: Creation}
--    | Brief { _entity :: !Entity}

--makeLenses ''Creation

semiramis :: Creation
semiramis = Creation {
    _entity = Human("Semiramis")
    , _title = "Queen of the Hanging Gardens"
    , _nation = "Babylonia"
    , _rank = 2
    , _authority = Royalty
    {-, _pateron = Creation {
        _entity = Deity("Ishtar")
    }
-}
}

type MyLens s a = forall f. Functor f => (a -> f a) -> s -> f s

newtype Identity a = Identity a

runIdentity:: Identity s -> s 
runIdentity (Identity x) = x

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

newtype Const v a = Const v

runConst :: Const v a -> v
runConst (Const x) = x

instance Functor (Const v) where
    fmap f (Const x) = Const x

set_fld :: a -> b -> Identity a
set_fld a _ = Identity a

set' :: forall s a. MyLens s a -> (a -> s -> s)
set' ln x s = runIdentity (ln (set_fld x) s)

view' :: MyLens s a -> (s -> a)
view' ln s = runConst (ln Const s)

over' :: MyLens s a -> (a -> a) -> s -> s
over' ln f s = runIdentity (ln (Identity . f) s)

getPateronName :: Creation -> Entity
getPateronName = view' (entity)


entity :: MyLens Creation Entity
entity some_function (Creation e t n r a) = (\e'->  Creation e' t n r a) <$> (some_function e)

item_name :: MyLens Creation String
item_name some_function (Creation e t n r a) = (\t'->  Creation e t' n r a) <$> (some_function t)

getPateronTitle :: Creation -> String
getPateronTitle = view' item_name

setPateronTitle :: Creation
setPateronTitle = set' item_name "Twice Married" semiramis

nontemplateLens :: MyLens Creation Int
nontemplateLens some_function (Creation e t n r a) = (\rank' -> Creation e t n rank' a) <$>
    (some_function (if r == 0 then 1 else r))

type MyTraversal s a = forall f. Applicative f => (a -> f a) -> s -> f s

instance Applicative Identity where
    pure x = Identity x
    (Identity y) <*> (Identity z) = Identity (y z)

instance Monoid a => Applicative (Const a) where
    pure x = Const mempty
    (Const vf) <*> (Const va) = Const (vf `mappend` va)

set'' :: forall s a. MyTraversal s a -> (a -> s -> s)
set'' ln x s = runIdentity (ln (set_fld x) s)

view'' :: forall s a. Monoid a => MyTraversal s a -> (s -> a)
view'' ln s = runConst (ln Const s)

over'' :: MyTraversal s a -> (a -> a) -> s -> s
over'' ln f s = runIdentity (ln (Identity . f) s)

title_nation :: MyTraversal Creation String
title_nation some_function (Creation e t n r a) = pure (\t' n' -> Creation e t' n' r a) <*>
    (some_function t)<*>
    (some_function n)

-- Unable to pull out both Int and String? Either Monad creates problems.
-- Can pull them both out as a String easily, but affects ability to manipulate two things at once.
title_rank :: MyTraversal Creation String
title_rank some_function (Creation e t n r a) = pure (\t' r' -> Creation e t' n (read r' :: Int) a) <*>
    (some_function t)<*>
    (some_function $ show r)

{- title_rank :: MyTraversal Creation (Either String Int)
title_rank some_function (Creation e t n r a) = pure (\t' r' -> Creation e t' n r' a) <*>
    (some_function (Left t))<*>
    (some_function (Right r)) -}

interesting_view = view'' title_nation semiramis

interesting_set = set'' title_rank "Junk" semiramis

{-

set and get are primary parts of a lens
for any basic extension would need the ability to retrieve and mutate values

so remote monad applicative, can be a traversal?
We need the ability to run through and look at multiple parts at once.

weak on this information, need details on weak strong, packets.
really just server things in general. 

Need a walk through the actual code, attempting to read it lacking webprogramming knowledge and other things wasn't doable.

there are lens packages that get or send comamands to webpages?
http://www.serpentine.com/wreq/

Every Monad is an Applicative but,
not every Applicative is a Monad.

And from what I assume, every Applicative is a Functor but,
not every Functor is an Applicative?

Conceptial hole, never studied Applicatives in math.
What makes the implimentations different if all Monads are Applicatives???

Monoid meaning in Haskell, what is all of the mappened? I saw something online about "Const"
The skillshare kind of handwaved over it.

I made Identity Applicative 100% on my own, a website verified that my intuition was correct.

Also, what are we even trying to do? Run an applicative over a list a commands contained in a record? 
-}

main :: IO ()
main = hspec $ do
  it "Semiramis" $
    getPateronTitle semiramis `shouldBe` "Queen of the Hanging Gardens"
  it "Semiramis Again" $
    getPateronTitle setPateronTitle `shouldBe` "Twice Married"
  it "Semiramis Thrice" $
    interesting_view `shouldBe` "Queen of the Hanging GardensBabylonia"
  it "Is she Junk?" $
    shouldBe (view'' title_rank interesting_set) "JunkJunk"

view_a :: (a,b) -> a
view_a (x,y) = x

set_a :: (a,b) -> a -> (a,b)
set_a (x,y) x' = (x',y)

data BasicLens a b = BasicLens {
    viewer :: (a,b) -> a
    , setter :: (a,b) -> a -> (a,b)}

a_Lens :: BasicLens a b
a_Lens = BasicLens {
    viewer = view_a
    , setter = set_a}

applyview ::  BasicLens a b -> (a, b) -> a
applyview (BasicLens viewer setter) = viewer

applyset ::  BasicLens a b -> (a,b) -> a -> (a,b)
applyset (BasicLens viewer setter) = setter

recovered_set_a :: (a,b) -> a -> (a,b)
recovered_set_a = applyset a_Lens

recovered_view_a :: (a,b) -> a
recovered_view_a = applyview a_Lens

-- *Main> recovered_set_a (2,3) 4
-- (4,3)
-- *Main> recovered_view_a (2,3)
-- 2