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

data LayoutMismatch = LayoutMismatch !Delta !Prefix !Prefix
  deriving (Eq, Show) -- this is for debugging the Layout Monoid

instance Relative LayoutMismatch where
  rel d (LayoutMismatch d' p q) = LayoutMismatch (d <> d') p q

data Run = Run {-# unpack #-} !Prefix !(Cat Dyck) {-# unpack #-} !Dyck !(Cat LayoutMismatch)
  deriving (Eq, Show) -- this is for debugging the Layout Monoid

instance Relative Run where
  rel d (Run p ds ts es) = Run p (rel d ds) (rel d ts) (rel d es)

runDyck :: Run -> Dyck
runDyck (Run _ _ ts _) = ts

runsDyck :: Cat Run -> Dyck
runsDyck Empty = Empty
runsDyck (x :< xs) = runDyck x <> runsDyck xs

instance HasPrefix Run where
  prefix (Run p _ _ _) = p

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
  deriving (Eq, Show) -- this is for debugging the Layout Monoid

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

-- this should almost certainly be revAppendCat :: Relative a => Cat a -> Cat a -> Cat a
revCat :: Relative a => Cat a -> Cat a
revCat Empty = Empty
revCat (x :< xs) = snocCat (revCat xs) x

-- S V(empty left)
-- S V(cons left)

-- V(empty right) S
-- V(cons right) S

-- V(empty right) V(empty left)
-- V(empty right) V(cons left)
-- V(cons right) V(empty left)
-- V(cons right) V(cons left)

-- boring -> packed into a run

-- the main thing i remember in there was using two forms of runs one for closings and one for openings because openings aren't collapsed yet
-- and the need to keep track of the full concatenation of all the dyck language bits just in case you got outdented and needed to dump all your text into a peer

instance Semigroup Layout where
  E 0 <> xs = xs
  xs <> E 0 = xs
  E d <> E d' = E (d <> d')
  E d <> S d' (Run p ds ts es) = S (d <> d') $ Run p (rel d ds) (rel d ts) (rel d es)
  E d <> V d' l m r = V (d <> d') (rel d l) (rel d m) (rel d r)
  S d (Run p ds ts es) <> E d' = S (d <> d') $ Run p ds ts es
  S d lr@(Run p ds ts es) <> S d' rr@(Run p' ds' ts' es') = case joinAndCompare p p' of
    Left p'' -> S (d <> d') $ Run p'' (ds <> rel d ds') (ts <> rel d ts') (snocCat es (LayoutMismatch d p p') <> rel d es') -- no common prefix
    Right LT -- indent
      | boring ts -> S (d <> d') $ Run p (ds <> rel d ds') (ts <> rel d ts') (es <> rel d es')
      | otherwise -> V (d <> d') Empty lr $ Rev $ Cat.singleton (rel d rr)
    Right EQ -> S (d <> d') $ Run p (ds <> rel d ds') (ts <> rel d ts') (es <> rel d es')
    Right GT -> V (d <> d') (Cat.singleton lr) (rel d rr) Empty

  --S d lr@(Run p ds ts es) <> V d' l m r = case joinAndCompare p (prefix m) of
  --   Left p'' | has _Empty r -> undefined -- S $ Run (d <> d') p''
     -- ... -- TODO: resume here

  -- can we use a V with an empty middle bit to handle runs?
  -- not so much, because we'd need to normalize the balance between either end
  -- we also want access to both ends of the run, which could make normalizing it hard
  -- we could add a normalize function, and have our test be equality-after-normalization
  -- we possibly want R (Cat Run) and RV (Cat Run) (Cat Run) (Rev (Cat Run)) (Rev (Cat Run))

  -- what do we do about the error cases?
  --   - pack everything into an S?
  --   - pack everything into whatever we would have had in the EQ case?

  -- a
  -- fg h ji/Rij
  S d lr@(Run p ds ts es) <> V d' l m@(Run p' ds' ts' es') r = case joinAndCompare p p' of
    Left p'' -> error "boom 1"

    -- Empty a fghij/Rjihgf
    Right LT ->
      case preview _Cons l of
          Nothing
            | boring ts ->
              V (d <> d') Empty (Run p (ds <> rel d ds') (ts <> rel d ts') (es <> rel d es')) (rel d r)
            | otherwise ->
              V (d <> d') Empty lr (Rev (Cat.singleton (rel d m)) <> rel d r)
          Just (Run p'' ds'' ts'' es'', xs) 
            | boring ts'' -> -- TODO compare prefixes?
              V (d <> d') Empty (Run p (ds <> rel d ds'') (ts <> rel d ts'') (es <> rel d es'')) (Rev (revCat (rel d xs)) <> Rev (Cat.singleton (rel d m)) <> rel d r)
            | otherwise ->
              V (d <> d') Empty lr (Rev (revCat (rel d l)) <> Rev (Cat.singleton (rel d m)) <> rel d r)

    -- Empty afgh ji/Rij
    Right EQ ->
      let
        rdl = rel d l
        m' = Run p (ds <> runsDycks rdl <> rel d ds') (ts <> runsDyck rdl <> rel d ts') (es <> runsMismatch rdl <> rel d es')
      in
        if has _Empty r
        then S (d <> d') m'
        else V (d <> d') Empty m' (rel d r)

    -- afg h ji/Rij
    Right GT ->
      V (d <> d') (Cat.singleton lr <> rel d l) (rel d m) (rel d r)

  V d l m r <> E d' = V (d <> d') l m r

  -- ab c ed/Rde
  -- f
  V d l m@(Run p ds ts es) r@(Rev rr) <> S d' rr'@(Run p' ds' ts' es') = case joinAndCompare p p' of
    Left p'' -> error "boom 2"

    -- ab c fed/Rdef
    Right LT -> 
      case preview _Cons rr of
          Nothing 
            | boring ts ->
              V (d <> d') l (Run p (ds <> rel d ds') (ts <> rel d ts') (es <> rel d es')) Empty
            | otherwise -> 
              V (d <> d') l m (Rev (Cat.singleton (rel d rr')))
          Just (Run p'' ds'' ts'' es'', xs)
            | boring ts'' -> -- TODO compare prefixes?
              V (d <> d') l m $ Rev $ review _Cons (Run p'' (ds'' <> rel d ds') (ts'' <> rel d ts') (es'' <> rel d es'), xs)
            | otherwise ->
              V (d <> d') l m (r <> Rev (Cat.singleton (rel d rr')))

    -- ab (cdef) Empty
    Right EQ ->
      let
        rrr = revCat rr
        m' = Run p' (ds <> runsDycks rrr <> rel d ds') (ts <> runsDyck rrr <> rel d ts') (es <> runsMismatch rrr <> rel d es')
      in
        if has _Empty l
        then S (d <> d') m'
        else V (d <> d') l m' Empty

    -- abcde f Empty
    Right GT -> V (d <> d') (l <> Cat.singleton m <> revCat rr) (rel d rr') Empty

  -- ab c ed/Rde
  -- fg h ji/Rij
  V d l m@(Run p ds ts es) r@(Rev rr) <> V d' l' m'@(Run p' ds' ts' es') r' = case joinAndCompare p p' of
    Left p'' -> error "boom 3"

    -- ab c jihgfed/Rdefghij
    Right LT -> 
      case preview _Cons rr of
        Nothing
          | boring ts -> 
            case preview _Cons l' of
              Nothing -> 
                V (d <> d') l (Run p (ds <> rel d ds') (ts <> rel d ts') (es <> rel d es')) (rel d r')
              Just (Run p''' ds''' ts''' es''', xs') ->
                V (d <> d') l (Run p (ds <> rel d ds''') (ts <> rel d ts''') (es <> rel d es''')) (Rev (revCat (rel d xs')) <> Rev (Cat.singleton (rel d m')) <> rel d r')
          | otherwise ->
              V (d <> d') l m (r <> Rev (revCat (rel d l')) <> Rev (Cat.singleton (rel d m')) <> rel d r')
        Just (Run p'' ds'' ts'' es'', xs)
          | boring ts'' -> 
            case preview _Cons l' of
              Nothing ->
                V (d <> d') l m (Rev (Cat.singleton(Run p'' (ds'' <> rel d ds') (ts'' <> rel d ts') (es'' <> rel d es'))) <> (rel d r'))
              Just (Run p''' ds''' ts''' es''', xs') -> case joinAndCompare p'' p' of
                Left p'' -> error "boom 4"
                Right EQ ->
                  -- dropping stuff seems risky
                  V (d <> d') l m (Rev xs <> Rev (Cat.singleton (Run p'' (ds'' <> rel d ds''' <> rel d ds') (ts'' <> rel d ts''' <> rel d ts') (es'' <> rel d es''' <> rel d es'))) <> rel d r')
                  -- V (d <> d') l m (Rev xs <> Rev (Cat.singleton (Run p'' (ds'' <> rel d ds''') (ts'' <> rel d ts''') (es'' <> rel d es'''))) <> Rev (revCat (rel d xs')) <> Rev (Cat.singleton (rel d m')) <> rel d r')
                _ ->
                  V (d <> d') l m (Rev xs <> Rev (Cat.singleton (Run p'' (ds'' <> rel d ds''') (ts'' <> rel d ts''') (es'' <> rel d es'''))) <> Rev (revCat (rel d xs')) <> Rev (Cat.singleton (rel d m')) <> rel d r')
          | otherwise -> 
              V (d <> d') l m (r <> Rev (revCat (rel d l')) <> Rev (Cat.singleton (rel d m')) <> rel d r')

    -- ab cdefgh ji/Rij
    Right EQ ->
      let
        rrr = revCat rr
        rdl' = rel d l'
        m'' = Run (prefix m) (ds <> runsDycks rrr <> runsDycks rdl' <> rel d ds') (ts <> runsDyck rrr <> runsDyck rdl' <> rel d ts') (es <> runsMismatch rrr <> runsMismatch rdl' <> rel d es')
      in
        if has _Empty l && has _Empty r'
        then S (d <> d') m''
        else V (d <> d') l m'' (rel d r')

    -- abcdefg h ji/Rij
    Right GT -> V (d <> d') (l <> Cat.singleton m <> revCat rr <> rel d l') (rel d m') (rel d r')

instance Monoid Layout where
  mempty = E 0
  mappend = (<>)
