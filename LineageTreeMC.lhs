The goal is to facilitate the generation of random LineageTree's. Note that
unlike MC0DFramework, this generates *entire* trees, rather than trying to build
the evolving population.

> module LineageTreeMC (generateRandomLineageTree) where

> import LineageTree
> import Control.Monad.Random

The framework assumes several kinds of statistical independence. In general,
they can be boiled down to a "zero dimensional" assumption. We can depend on
global properties such as time, but nothing about the state of parents,
siblings, etc. are allowed. As such, we also forget about potential mean fields,
partly as a necessity of trying to deal with generating each lineage tree
independently from others.

As such, things are well specified by supplying three (sets of) functions:
 1. Cell cycle time tau
 2. Branching functions r_pp, r_dd (r_pd = 1 - r_pp - r_dd).
 3. Fate choices --- returns one of the cell types

> generateRandomLineageTree :: (MonadRandom m) =>
>   (Double -> m Double) ->
>   ((Double -> Double), (Double -> Double)) -> 
>   (Double -> m c) ->
>   m (LineageTree c Double ())
> generateRandomLineageTree = generateRandomLineageTree' 0.0

> generateRandomLineageTree' t cycle (r_pp, r_dd) fate = do
>     tau <- cycle t
>     branch <- getRandomR (0.0, 1.0)
>     if branch < (r_pp t)
>       then pair tau p p
>       else if branch < (1 - (r_dd t))
>         then pair tau p d
>         else pair tau d d
>   where
>     pair tau c1 c2 = do
>       t1 <- c1 tau
>       t2 <- c2 tau
>       return $ Progenitor tau t1 t2
>     p tau = generateRandomLineageTree' (t + tau) cycle (r_pp, r_dd) fate
>     d tau = do
>       c <- fate tau
>       return $ Differentiated c ()

