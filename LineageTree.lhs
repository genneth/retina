> {-# LANGUAGE NoMonomorphismRestriction #-}

> module LineageTree where
 
We assume that we have one kind of progenitor, and several different types
of differentiated cells. We may also wish to attach data to these trees.

> data LineageTree t a b = 
>     Progenitor a (LineageTree t a b) (LineageTree t a b) 
>   | Differentiated t b
>   deriving (Eq, Show, Read)

This actually should form a bifunctor, but can't be bothered to actually
derive this and make it all fit. In addition, it should be foldable.

> mapLT2 f g (Progenitor d t1 t2) 
>  = Progenitor (f d) (mapLT2 f g t1) (mapLT2 f g t2)
> mapLT2 f g (Differentiated t d) = Differentiated t (g d)
> mapLT f lt = mapLT2 f f lt

> foldLT f g (Progenitor d t1 t2) = f d (foldLT f g t1) (foldLT f g t2)
> foldLT f g (Differentiated t d) = g t d

Convenience function --- we often actually want to build a list, fairly
independently of the children:

> toList f g = foldLT (\x l r -> (f x) ++ l ++ r) g

Another activity which we do a lot is to find subtrees which match a certain
criterion:

> findSubLT match t@(Progenitor _ l r) = res ++ (findSubLT match l) ++ (findSubLT match r)
>   where res = if match t then [t] else []
> findSubLT match n@(Differentiated _ _) = if match n then [n] else []

Combinators to generate frequently used matchers:

> matchAllProlif ml mr t@(Progenitor _ l r)
>   | (ml l) && (mr r) = True
>   | (ml r) && (mr l) = True
>   | otherwise        = False
> matchAllProlif _ _ _    = False

> matchAllProlif3 aunt d1 d2 = matchAllProlif aunt (matchAllProlif d1 d2)

> matchAny = const True

> matchProg (Progenitor _ _ _)   = True
> matchProg (Differentiated _ _) = False

> matchDiff = not . matchProg

> matchCell c (Progenitor _ _ _)    = False
> matchCell c (Differentiated c' _) = c == c'


