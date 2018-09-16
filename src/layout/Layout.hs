{-# language CPP #-}
{-# language OverloadedLists #-}
{-# language LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Layout where

import Control.Lens
import Data.Default

#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup
#endif

import Relative.Class
import Relative.Delta
import Relative.Cat as Cat
import Rev
import Syntax.Prefix

import Dyck
-- import Token

data LayoutMismatch = LayoutMismatch !Delta !Prefix !Prefix

instance Relative LayoutMismatch where
  rel d (LayoutMismatch d' p q) = LayoutMismatch (d <> d') p q

data Run = Run {-# unpack #-} !Prefix !(Cat Dyck) {-# unpack #-} !Dyck !(Cat LayoutMismatch)

instance Relative Run where
  rel d (Run p ds ts es) = Run p (rel d ds) (rel d ts) (rel d es)

runDyck :: Run -> Dyck
runDyck (Run _ _ ts _) = ts

runsDyck :: Cat Run -> Dyck
runsDyck Empty = Empty
runsDyck (x :< xs) = runDyck x <> runsDyck xs

instance HasPrefix Run where
  prefix (Run p _ _ _) = p

-- could use Foldable for this, but when in Rome...

runDycks :: Run -> Cat Dyck
runDycks (Run _ ds _ _) = ds

runsDycks :: Cat Run -> Cat Dyck
runsDycks Empty = Empty
runsDycks (x :< xs) = runDycks x <> runsDycks xs

runMismatch :: Run -> Cat LayoutMismatch
runMismatch (Run _ _ _ es) = es

runsMismatch :: Cat Run -> Cat LayoutMismatch
runsMismatch Empty = Empty
runsMismatch (x :< xs) = runMismatch x <> runsMismatch xs

data Layout
  = E {-# unpack #-} !Delta
  | S {-# unpack #-} !Delta {-# unpack #-} !Run
  | V {-# unpack #-} !Delta !(Cat Run) {-# unpack #-} !Run !(Rev Cat Run)

instance HasDelta Layout where
  delta (E d) = d
  delta (S d _) = d
  delta (V d _ _ _) = d

instance AsEmpty Layout where
  _Empty = prism (const $ E 0) $ \case
    E 0 -> Right ()
    x   -> Left x

dyckLayout :: Delta -> Prefix -> Dyck -> Layout
dyckLayout d _ Empty = E d
dyckLayout d p t = S d $ Run p [t] t []

boring :: Dyck -> Bool
boring = views dyckLayoutMode (def ==)

revCat :: Relative a => Cat a -> Cat a
revCat Empty = Empty
revCat (x :< xs) = snocCat (revCat xs) x

instance Semigroup Layout where
  E 0 <> xs = xs
  xs <> E 0 = xs
  E d <> E d' = E (d <> d')
  E d <> S d' (Run p ds ts es) = S (d <> d') $ Run p (rel d ds) (rel d ts) (rel d es)
  E d <> V d' l m r = V (d <> d') (rel d l) m (rel d r)
  S d (Run p ds ts es) <> E d' = S (d <> d') $ Run p ds ts es
  S d lr@(Run p ds ts es) <> S d' rr@(Run p' ds' ts' es') = case joinAndCompare p p' of
    Left p'' -> S (d <> d') $ Run p'' (ds <> rel d ds') (ts <> rel d ts') (snocCat es (LayoutMismatch d p p') <> rel d es') -- no common prefix
    Right LT -- indent
      | boring ts -> S (d <> d') $ Run p (ds <> rel d ds') (ts <> rel d ts') Empty
      | otherwise -> V (d <> d') Empty lr $ Rev $ Cat.singleton (rel d rr)
    Right EQ -> S (d <> d') $ Run p (ds <> rel d ds') (ts <> rel d ts') (es <> rel d es')
    Right GT -> V (d <> d') (Cat.singleton lr) (rel d rr) Empty

  --S d lr@(Run p ds ts es) <> V d' l m r = case joinAndCompare p (prefix m) of
  --   Left p'' | has _Empty r -> undefined -- S $ Run (d <> d') p''
     -- ... -- TODO: resume here

  -- what happens if `ts` is boring in the S <> V or V <> S cases?
  -- what do we do about the error cases?
  --   - pack everything into an S?
  --   - pack everything into whatever we would have had in the EQ case?
  -- need to double check `rel`s in relation to the middle bit of Vs

  -- TODO check the `Rev`s
  -- TODO should be special casing when a we have S attaching to V where the V is Empty on that side?
  --      or is it taken care of for us anyhow?

  -- a
  -- fg h ji/Rij
  S d lr@(Run p ds ts es) <> V d' l m@(Run p' ds' ts' es') r = case joinAndCompare p p' of
    Left p'' -> undefined
    -- Empty a fghij/Rjihgf
    Right LT ->
      V (d <> d') Empty lr (Rev (revCat (rel d l)) <> Rev (Cat.singleton (rel d m)) <> rel d r)
    -- Empty afgh ji/Rij
    Right EQ ->
      let
        rdl = rel d l
      in
        V (d <> d') Empty (Run p (ds <> runsDycks rdl <> rel d ds') (ts <> runsDyck rdl <> rel d ts') (es <> runsMismatch rdl <> rel d es')) (rel d r)
    -- afg h ji/Rij
    Right GT ->
      V (d <> d') (Cat.singleton lr <> rel d l) (rel d m) (rel d r)

  V d l m r <> E d' = V (d <> d') l m r

  -- ab c ed/Rde
  -- f
  V d l m@(Run p ds ts es) r@(Rev rr) <> S d' rr'@(Run p' ds' ts' es') = case joinAndCompare p p' of
    Left p'' -> undefined
    -- ab c fed/Rdef
    Right LT -> V (d <> d') l m (Rev (Cat.singleton (rel d rr')) <> r)
    -- ab (cdef) Empty
    Right EQ ->
      let
        rrr = revCat rr
      in
        V (d <> d') l (Run p' (ds <> runsDycks rrr <> rel d ds') (ts <> runsDyck rrr <> rel d ts') (es <> runsMismatch rrr <> rel d es')) Empty
    -- abcde f Empty
    Right GT -> V (d <> d') (l <> Cat.singleton m <> revCat rr) (rel d rr') Empty

  -- ab c ed/Rde
  -- fg h ji/Rij
  V d l m@(Run p ds ts es) r@(Rev rr) <> V d' l' m'@(Run p' ds' ts' es') r' = case joinAndCompare p p' of
    Left p'' -> undefined
    -- ab c jihgfed/Rdefghij
    Right LT -> V (d <> d') l m (rel d r' <> Rev (Cat.singleton (rel d m')) <> Rev (revCat (rel d l')) <> r)
    -- ab cdefgh ji/Rij
    Right EQ ->
      let
        rrr = revCat rr
        rdl' = rel d l'
      in
        V (d <> d') l (Run (prefix m) (ds <> runsDycks rrr <> runsDycks rdl' <> rel d ds') (ts <> runsDyck rrr <> runsDyck rdl' <> rel d ts') (es <> runsMismatch rrr <> runsMismatch rdl' <> rel d es')) (rel d r')
    -- abcdefg h ji/Rij
    Right GT -> V (d <> d') (l <> Cat.singleton m <> revCat rr <> rel d l') (rel d m') (rel d r')

instance Monoid Layout where
  mempty = E 0
  mappend = (<>)
