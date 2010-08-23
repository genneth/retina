{-# LANGUAGE NoMonomorphismRestriction #-}

import Graphics.Rendering.Chart 
import Graphics.Rendering.Chart.Gtk
import Data.Accessor
import Data.Colour
import Data.Colour.SRGB

import Control.Arrow
import Data.List
import Data.Ratio
import Control.Monad

import LineageTree
import LineageTreeMC
import Control.Monad.Random

white = sRGB 1 1 1

--- we have progenitor cells, and 4 other differentiated cell types.
--- data comes in the form of lineage trees, and we'd like to minimise the
--- amount of typing required.

-- the types are actually colours
data InputLineageTree = 
    P Double InputLineageTree InputLineageTree 
  | B | R | G | Y | U 
  deriving (Show, Read, Eq)

-- the first number is a measurement in mm; 21 mm == 50 hrs
scaleFirstTime = map (\(P d t1 t2) -> P (d/21.0*50.0) t1 t2)

inputRetinalLineageData1 = scaleFirstTime [
  P 7 (P 45 B B) (P 38 (P 53.6 R G) (P 55.5 (P 30.5 B R) (P 28.5 G Y))),
  P 1 (P 51.5 (P 34.4 B B) (P 37 (P 56 B B) (P 41 (P 52 G (P 32.3 B B)) (P 43.8 (P 34.3 B B) (P 36 G Y))))) B,
  P 23 (P 68.1 (P 34.4 R G) (P 30.1 (P 49.1 (P 37 B (P 42 Y B)) (P 37.3 (P 48.3 U B) B)) (P 49.1 R R))) B,
  P 2 (P 36.1 B R) (P 64.8 (P 51 (P 60.5 B Y) B) B),
  P 24.5 (P 60.4 Y B) B,
  P 6 (P 73.5 Y B) B,
  P 6 G (P 172.9 (P 40.2 B B) Y),
  P 7 B (P 46.75 B (P 36.93 B (P 72.26 Y B))),
  P 0 G (P 75 G Y),
  P 32 (P 34.3 (P 28.6 (P 41.2 B B) B) Y) B,
  P 4 (P 86.6 Y B) R,
  P 1.5 B (P 50.7 B (P 48.4 B B)),
  P 5 B (P 60.8 B (P 27.1 B B)),
  P 5 B (P 58.3 B (P 36.7 B B)),
  P 7 B (P 60 B (P 38.7 B B)),
  P 5 B (P 46.9 B (P 57.3 B B)),
  P 6 B (P 72 B (P 54.1 B B)),
  P 7 B (P 58.5 B (P 48.5 B B)),
  P 2.5 B (P 45.8 B (P 55.1 B B)),
  P 0.5 B (P 77 B (P 51 B B)),
  P 4 B (P 52.8 B (P 59.9 B B)),
  P 8.5 B (P 80.2 B (P 52.3 B B)),
  P 4.5 B (P 35.6 B (P 28.2 B B)),
  P 5 B (P 58.9 (P 30.5 B (P 71.4 B B)) (P 30.2 B (P 31.3 B B))),
  P 14 B (P 51.4 B (P 34.1 B (P 53.8 B B)))]

inputRetinalLineageData2 = scaleFirstTime [
  P 5 (P 54.9 B B) (P 47 B (P 70 B B)),
  P 2 B (P 57.0 B B),
  P 8 B (P 47.1 B B),
  P 3 B (P 66.7 B B),
  P 3 B (P 51.1 B B),
  P 36 B (P 51 B B),
  P 7.5 B (P 46.4 B B),
  P 16 B (P 43.8 B B),
  P 18 B (P 42.6 B B),
  P 11 B (P 58.8 B B),
  P 11 B (P 57.2 B B),
  P 4 B (P 56 B B),
  P 3 B (P 60.6 B B),
  P 2 B (P 56.8 B B),
  P 2 B (P 52.2 B B),
  P 4.5 B (P 61 B B),
  P 11 B (P 55.2 B B),
  P 13 B (P 48.8 B B),
  P 8 B (P 72.1 B B),
  P 4 B (P 50.4 B B),
  P 3 B (P 52.6 B B),
  P 21 B (P 44.6 B B),
  P 0.5 B (P 89 B B),
  P 9 B (P 64 B B),
  P 2.5 B (P 52.1 B B),
  P 13 B (P 48.3 B B)]

inputRetinalLineageData3 = scaleFirstTime [
  P 12 B (P 51.4 B B),
  P 30 B (P 55.2 B B),
  P 6 B (P 58.3 B B),
  P 4.5 B (P 56.7 B B),
  P 11 B (P 83.4 B B),
  P 7 B (P 46 B B),
  P 3 B (P 53.9 B B),
  P 32 B (P 46.8 B B),
  P 6 B (P 47.7 B B),
  P 2 B (P 48.1 B B),
  P 2.5 B (P 78.6 B B),
  P 0 B (P 40.2 B B),
  P 5 B (P 43.2 B B),
  P 6.5 B (P 64.2 B B),
  P 18 B (P 47 B B),
  P 6 B (P 71.1 B B),
  P 12 B (P 57.8 B B),
  P 4 B (P 50 B B),
  P 0.5 R (P 40.1 B B),
  P 6 B (P 66.3 G B),
  P 19 B (P 51 B R),
  P 3 B (P 58.2 G G),
  P 6 B (P 65.3 G R),
  P 3.5 B (P 54.7 B R),
  P 4.5 B (P 92 R R)]

inputRetinalLineageData4 = scaleFirstTime [
  P 0 B (P 96.1 B R),
  P 8.5 G (P 55 B B),
  P 5.5 G (P 73.8 B R),
  P 2 B (P 64 R R),
  P 9 B (P 62 B R),
  P 1 G (P 72 B B),
  P 7 G (P 48.4 G R),
  P 5.5 B (P 85.4 B R),
  P 4 B (P 87 R R),
  P 5 G (P 68.3 B B),
  P 11 G (P 70.6 G R),
  P 8 G (P 63.5 G R),
  P 4 G (P 60.4 G R),
  P 11.5 G (P 44 G R),
  P 3 G (P 89.8 B B),
  P 6 B (P 42.2 G B),
  P 0 G (P 63 B B),
  P 5 G (P 42.8 B R),
  P 6.5 G (P 64 B B),
  P 2 B (P 44.7 G B),
  P 6 B (P 95.9 G B),
  P 9.5 B (P 65.4 B R),
  P 3 B (P 71.1 B R),
  P 7.5 B (P 79.1 G B),
  P 0 G (P 135.7 B R)]

inputRetinalLineageData5 = scaleFirstTime [
  P 13.5 B (P 85.3 B R),
  P 2 B (P 66 R R),
  P 5 B (P 75.9 G B),
  P 6 B (P 69.3 G G),
  P 6 G (P 45.4 G R),
  P 4 R (P 44.8 B B),
  P 0 B (P 50.3 B (P 53 R (P 70.9 G U))),
  P 1 (P 72.8 G B) (P 38 B B),
  P 12.5 (P 43.3 G R) (P 30.3 (P 52.9 G R) B),
  P 5 (P 44 G R) (P 67.7 B (P 51.1 B R)),
  P 4 (P 43.2 B B) (P 92.9 G B),
  P 0.5 (P 44.5 B B) (P 40.4 (P 115.7 U B) B),
  P 0 (P 48.9 B R) (P 56 (P 36.5 B B) B),
  P 2 (P 38 B B) (P 60 G G),
  P 2 R (P 72.1 (P 32.8 B (P 71.3 B B)) (P 31.2 (P 38.9 B B) B)),
  P 4 B (P 56.6 G (P 52.4 B B)),
  P 6 G (P 86.1 B Y),
  P 6 B (P 72.2 G Y),
  P 4 B (P 68.9 R (P 45.2 B B)),
  P 4.5 B (P 43.2 B (P 52 R R)),
  P 5 B (P 52.6 B (P 48.1 G B)),
  P 12 R (P 68.8 B (P 34.6 G B)),
  P 1 B (P 74 R (P 61 B B)),
  P 3 B (P 94.8 B (P 62.2 G B)),
  P 3.5 R (P 40.1 G (P 47.8 B B)),
  P 9.5 B (P 36.2 G R),
  P 5 B (P 42.2 B (P 59 B (P 72.2 G B))),
  P 2 B (P 87.5 B (P 64.2 G G))]

-- convert to generic LineageTree
data RetinalCells = Rph | Ama | Bip | Mul | Unk deriving (Eq, Enum, Show, Read)
type RetinalLineageTree = LineageTree RetinalCells

const2 x = (\_ _ -> x)

fromInputLineageTree (P c t1 t2) 
  = Progenitor c (fromInputLineageTree t1) (fromInputLineageTree t2)
fromInputLineageTree B           = Differentiated Rph ()
fromInputLineageTree R           = Differentiated Ama ()
fromInputLineageTree G           = Differentiated Bip ()
fromInputLineageTree Y           = Differentiated Mul ()
fromInputLineageTree U           = Differentiated Unk ()

retinalLineageData :: [RetinalLineageTree Double ()]
retinalLineageData = concat retinalLineageDataSets
retinalLineageDataSets = map (map fromInputLineageTree) [inputRetinalLineageData1,
  inputRetinalLineageData2, inputRetinalLineageData3, inputRetinalLineageData4,
  inputRetinalLineageData5]

-- the conversion into graphs proceeds in stages:
--  1. layout each tree
--  2. work out the width of each tree
--  3. layout trees next to each other
--  4. convert into graphics primitives to be drawn
--  5. produce plot using Graphics.Rendering.Chart
-- at various stages, we need to retain the basic tree shape whilst introducing
-- more tags; this is where our generalised tree type pays off

-- some basic sizes
symRenewXOffset = 10.0
asymRenewXOffset = 4.0
symDiffXOffset = 3.5
diffYOffset = 4.0
treePadding = 3.0

type Position = (Double, Double)
type LayoutLineageTree c = LineageTree c ((Double, Double, Double), Position) Position
mapLLT f = mapLT2 (second f) f

layoutTree :: LineageTree c Double () -> LayoutLineageTree c
layoutTree (Differentiated t ()) = Differentiated t (0,diffYOffset)
layoutTree (Progenitor dy t1 t2) = 
    Progenitor ((x1, x2, dy), (0,dy))
      (mapLLT lf $ layoutTree t1)
      (mapLLT rf $ layoutTree t2)
  where
    lf = (subtract x1) *** (dy+)
    rf = (x2 +) *** (dy+)
    (x1,x2) = case (t1,t2) of
                (Progenitor _ _ _, Progenitor _ _ _)     -> (symRenewXOffset, symRenewXOffset)
                (Differentiated _ _, Progenitor _ _ _)   -> (asymRenewXOffset, 0.0)
                (Progenitor _ _ _, Differentiated _ _)   -> (0.0, asymRenewXOffset)
                (Differentiated _ _, Differentiated _ _) -> (symDiffXOffset, symDiffXOffset)

treeExtent :: LayoutLineageTree c -> (Double, Double)
treeExtent t = (x1 - treePadding, x2 + treePadding)
  where x1 = foldLT (\(_,(x,_)) y1 y2 -> x `min` y1 `min` y2) (\_ (x,_) -> x) t
        x2 = foldLT (\(_,(x,_)) y1 y2 -> x `max` y1 `max` y2) (\_ (x,_) -> x) t

layoutTrees tts = layoutTrees' 0.0 tts
  where layoutTrees' _  []     = []
        layoutTrees' x0 (t:ts) = (mapLLT (first (+(x0-left))) t):
                                   (layoutTrees' (x0-left+right) ts)
          where (left,right) = treeExtent t
    
extractCellPositions :: Eq c => c -> LayoutLineageTree c -> [Position]
extractCellPositions cell = toList (const []) 
                                   (\c p -> if c == cell then [p] else [])

extractProgenitorLines :: LayoutLineageTree c -> [[Position]]
extractProgenitorLines = toList (\((dx1,dx2,dy),(x,y)) -> [[(x,y),(x,y-dy)],[(x-dx1,y),(x+dx2,y)]])
                                (const2 [])

-- actually produce the plots

---toGraph :: Show a => [a] -> [LineageTree RetinalCells Double ()] -> "Graph"
toGraph names trees = 
      layout1_plots ^= [
        Left (toPlot (plot_lines_values ^= concatMap extractProgenitorLines lts
                    $ plot_lines_style ^= solidLine 1.0 (black `withOpacity` 1.0)
                    $ defaultPlotLines)),
        Left (toPlot (plot_points_title ^= "Rod photoreceptor"
                    $ plot_points_style ^= filledCircles 3.0 (sRGB 0 0 1 `withOpacity` 1.0)
                    $ plot_points_values ^= concatMap (extractCellPositions Rph) lts
                    $ defaultPlotPoints)),
        Left (toPlot (plot_points_title ^= "Amacrine"
                    $ plot_points_style ^= filledCircles 3.0 (sRGB 1 0 0 `withOpacity` 1.0)
                    $ plot_points_values ^= concatMap (extractCellPositions Ama) lts
                    $ defaultPlotPoints)),
        Left (toPlot (plot_points_title ^= "Bipolar"
                    $ plot_points_style ^= filledCircles 3.0 (sRGB 0 1 0 `withOpacity` 1.0)
                    $ plot_points_values ^= concatMap (extractCellPositions Bip) lts
                    $ defaultPlotPoints)),
        Left (toPlot (plot_points_title ^= "Müller glial"
                    $ plot_points_style ^= filledCircles 3.0 (sRGB 1 1 0 `withOpacity` 1.0)
                    $ plot_points_values ^= concatMap (extractCellPositions Mul) lts
                    $ defaultPlotPoints)),
        Left (toPlot (plot_points_title ^= "Unknown fate"
                    $ plot_points_style ^=  hollowCircles 2.5 1.0 (sRGB 0 0 0 `withOpacity` 1.0)
                    $ plot_points_values ^= concatMap (extractCellPositions Unk) lts
                    $ defaultPlotPoints))]
    $ layout1_left_axis ^: (laxis_title ^= "Time (hours)")
                         . (laxis_reverse ^= True)
                         . (laxis_style ^: (axis_label_gap ^= 3.0))
    $ layout1_bottom_axis ^: (laxis_style ^: (axis_line_style ^= solidLine 0.0 (black `withOpacity` 0.0))
                                           . (axis_grid_style ^= solidLine 0.0 (black `withOpacity` 0.0))
                                           . (axis_label_style ^: font_size ^= 0.0))
    $ layout1_top_axis ^: (laxis_visible ^= const True)
                        . (laxis_style ^: (axis_label_gap ^= 2.0)
                                        . (axis_grid_style ^= solidLine 0.0 (black `withOpacity` 0.0))
                                        . (axis_label_style ^: font_size ^= 5.0))
                        . (laxis_override ^= (axis_ticks ^= zip roots (repeat (-2)))
                                           . (axis_labels ^= zip roots (map show names)))
    $ defaultLayout1
  where
    lts = layoutTrees $ map layoutTree $ trees
    roots = map (\(Progenitor (_,(x,_)) _ _) -> x) lts

-- next up: more sanity checks
--  1. plot cell cycle time vs absolute time
--  2. plot cell cycle time for various kinds of divisions
--    a. three channels (fig 4a)
--    b. differentiative outcome (fig 5b)
--  3. table for sychrony of siblings (graph??)

-- kinda like LayoutLT, but without the x coord
toTimeLT :: LineageTree c Double () -> LineageTree c (Double, Double) Double
toTimeLT (Differentiated c ()) = Differentiated c 0.0
toTimeLT (Progenitor dy t1 t2) = 
  Progenitor (0.0, dy)
    (mapTLT (+dy) $ toTimeLT t1)
    (mapTLT (+dy) $ toTimeLT t2)

mapTLT f (Differentiated c y) = Differentiated c (f y)
mapTLT f (Progenitor (y1,y2) t1 t2) = 
  Progenitor (f y1, f y2) 
    (mapTLT f t1) (mapTLT f t2)

retinalTLT = map toTimeLT retinalLineageData

cycleLengthvsTime = 
  concatMap (toList (\(y1,y2) -> if y1 == 0.0 then [] else [(y1,y2-y1)])
                    (const2 []))
            retinalTLT

cycleLengthvsTimePlot =
    layout1_plots ^= [Left (toPlot
       (plot_points_style ^= filledCircles 3.0 (black `withOpacity` 1.0)
      $ plot_points_values ^= cycleLengthvsTime
      $ defaultPlotPoints))]
  $ layout1_bottom_axis ^: (laxis_title ^= "Progenitor birth time (hours)")
  $ layout1_left_axis ^: (laxis_title ^= "Cell cycle (hours)")
  $ defaultLayout1

-- appears to be no variation of significance with average cycle length
-- but possibly changing variance?
-- assume no changes, and plot the distribution
cycleLengths = map snd cycleLengthvsTime
cycleLengthHist = histogram (map (*5) [0..39]) cycleLengths

histogram [x] ys = [(x, genericLength $ filter (x <=) ys)]
histogram (x:x2:xs) ys = (x, genericLength $ filter (\y -> x <= y && y < x2) ys):(histogram (x2:xs) ys)

logNormal mu s2 x = 1/(x * sqrt(2*pi*s2)) * exp(-(log x - mu)^2/(2*s2))

cycleLengthHistPlot =
    layout1_plots ^= [
      Left (plotBars
         (plot_bars_values ^= map (second ((:[]) . fromIntegral)) cycleLengthHist
        $ plot_bars_spacing ^= BarsFixGap 0.0 0.0
        $ plot_bars_alignment ^= BarsLeft
        $ defaultPlotBars)),
      Left (toPlot
         (plot_lines_values ^= [map (\x -> (x,5 * (fromIntegral $ length cycleLengths) * 
                                              (logNormal (mean $ map log cycleLengths)
                                                         (variance $ map log cycleLengths)
                                                         x)))
                                   $ [1..199]]
        $ plot_lines_style ^= solidLine 2.0 (black `withOpacity` 0.8)
        $ defaultPlotLines))]
  $ layout1_bottom_axis ^: (laxis_title ^= "Cell cycle (hours)")
  $ layout1_left_axis ^: (laxis_title ^= "Frequency")
  $ defaultLayout1

-- matcher that skips the first division
matchProlif ml mr (Progenitor (y1, y2) l r)
  | y1 /= 0 && (ml l) && (mr r) = True
  | y1 /= 0 && (ml r) && (mr l) = True
  | otherwise = False
matchProlif _ _ _ = False

cycleLengthOf match = map (\(Progenitor (y1,y2) _ _) -> y2-y1) 
                    $ concatMap (findSubLT match) retinalTLT

mean     xs = (foldl (+) 0 xs) / (fromIntegral $ length xs)
variance xs = mean (zipWith (*) dxs dxs)
  where dxs = zipWith (-) xs (repeat $ mean xs)
mstd     xs = sqrt $ (foldl (+) 0 $ zipWith (*) dxs dxs) / (fromIntegral $ (length xs) - 1)
  where dxs = zipWith (-) xs (repeat $ mean xs)

cycleLengthvsFatePlot = 
      layout1_plots ^= [
        Left (toPlot $ PlotHidden [] [0, 150]),
        Left (toPlot
           (plot_errbars_values ^= zipWith3 (\x y dy -> ErrPoint (ErrValue x x x) (ErrValue (y-dy) y (y+dy)))
                                            xs
                                            (map mean dists)
                                            (map mstd dists)
          $ plot_errbars_tick_length ^= 4
          $ plot_errbars_line_style ^= solidLine 1.0 (black `withOpacity` 1.0)
          $ defaultPlotErrBars)),
        Left (plotBars
           (plot_bars_values ^= (zip xs $ map ((:[]).mean) dists)
          $ plot_bars_item_styles ^= [(solidFillStyle (white `withOpacity` 1.0),
                                Just $ solidLine 1.0 (black `withOpacity` 1.0))]
          $ plot_bars_spacing ^= BarsFixGap 5.0 0.0
          $ plot_bars_alignment ^= BarsCentered
          $ defaultPlotBars))]
    $ layout1_bottom_axis ^: (laxis_override ^= (axis_ticks ^= (take (length labels) $ zip xs $ repeat (-2.0)))
                                              . (axis_labels ^= (zip xs labels)))
                           . (laxis_style ^: (axis_label_gap ^= 2.0)
                                           . (axis_grid_style ^= solidLine 0.0 (black `withOpacity` 0.0))
                                           . (axis_label_style ^: font_size ^= 6.0))
    $ layout1_left_axis ^: (laxis_title ^= "Cell cycle (hours)")
                         . (laxis_override ^= (axis_ticks ^= (zip [0,50,100,150] $ repeat (-2.0)))
                                            . (axis_labels ^= [(0,"0"),(50,"50"),(100,"100"),(150,"150")]))
                         . (laxis_style ^: (axis_grid_style ^= solidLine 0.0 (black `withOpacity` 0.0))
                                         . (axis_label_style ^: font_size ^= 8.0))
    $ defaultLayout1
  where
    xs = [25,75..] :: [Int]
    dists = map (cycleLengthOf . uncurry matchProlif)
                [(matchCell Ama, not . matchCell Ama),
                 (matchCell Ama, matchCell Ama),
                 (matchCell Rph, not . matchCell Rph),
                 (matchCell Rph, matchCell Rph),
                 (matchCell Bip, not . matchCell Bip),
                 (matchCell Bip, matchCell Bip),
                 (matchCell Mul, not . matchCell Mul),
                 (matchProg, matchDiff),
                 (matchProg, matchProg),
                 (matchAny, matchAny)]
    labels = ["Ama+X", "2Ama", "Rph+X", "2Rph", "Bip+X", "2Bip", "Mül+X", "RPC+X", "2RPC", "All"]

-- test hypothesis: daughters are more synchronous
daughterSynchrony = map (\(Progenitor _ (Progenitor (_,y1) _ _)
                                        (Progenitor (_,y2) _ _))
                        -> y2 - y1)
                  $ concatMap (findSubLT $ matchAllProlif matchProg matchProg) 
                  $ retinalTLT

-- test hypothesis: correlation between parent division time and children's
divisionSynchrony = concatMap subtractDivisions
                  $ concatMap (findSubLT $ matchProlif matchProg matchAny)
                  $ retinalTLT
  where subtractDivisions (Progenitor (y1,y2) (Progenitor (y1',  y2')  _ _)
                                              (Progenitor (y1'', y2'') _ _))
          = [(y2'-y1') - (y2-y1), (y2''-y1'') - (y2-y1)]
        subtractDivisions (Progenitor (y1,y2) (Progenitor (y1', y2') _ _)
                                              (Differentiated _ _))
          = [(y2'-y1') - (y2-y1)]
        subtractDivisions (Progenitor (y1,y2) (Differentiated _ _)
                                              (Progenitor (y1', y2') _ _))
          = [(y2'-y1') - (y2-y1)]
        subtractDivisions (Progenitor (y1,y2) (Differentiated _ _)
                                              (Differentiated _ _))
          = []

-- test hypothesis: finishing times are synchronised
finishTime :: LineageTree c (Double, Double) Double -> Double
finishTime = foldLT (\_ l r -> max l r) (\_ x -> x)
finishTimesHist :: [LineageTree c (Double, Double) Double] -> [(Double, Double)]
finishTimesHist ts = map (second (/ n)) $ histogram [0,10..300] $ map finishTime ts
  where n = genericLength ts

finishTimesHistPlot experiment theory =
    layout1_plots ^= [
      Left (plotBars
         (plot_bars_values ^= map (second (:[])) (finishTimesHist experiment)
        $ plot_bars_spacing ^= BarsFixGap 0.0 0.0
        $ plot_bars_alignment ^= BarsLeft
        $ defaultPlotBars)),
      Left (toPlot
         (plot_lines_values ^= [finishTimesHist theory]
        $ plot_lines_style ^= solidLine 2.0 (black `withOpacity` 1.0)
        $ defaultPlotLines))]
  $ layout1_bottom_axis ^: (laxis_title ^= "Finish time (hours)")
  $ layout1_left_axis ^: (laxis_title ^= "Relative frequency")
  $ defaultLayout1

-- first division time
firstDivisionTime = map (\(Progenitor y _ _) -> y) retinalLineageData
firstDivisionTimeHist :: [(Double, Int)]
firstDivisionTimeHist = histogram [0,5..100] firstDivisionTime

firstDivisionTimeHistPlot =
    layout1_plots ^= [
      Left (plotBars
         (plot_bars_values ^= map (second (:[])) firstDivisionTimeHist
        $ plot_bars_spacing ^= BarsFixGap 0.0 0.0
        $ plot_bars_alignment ^= BarsLeft
        $ defaultPlotBars))]
  $ layout1_bottom_axis ^: (laxis_title ^= "First division (hours)")
  $ layout1_left_axis ^: (laxis_title ^= "Frequency")
  $ defaultLayout1

-- then, the big table of doom (tm)
countDivisions mr ml 
  = genericLength
  $ concatMap (findSubLT $ matchAllProlif ml mr) 
  $ retinalLineageData

divisionTimeOf match = map (\(Progenitor (y2) _ _) -> y2) 
                     $ concatMap (findSubLT match) retinalTLT

countDivisions3 aunt d1 d2 
  = genericLength
  $ concatMap (findSubLT $ matchAllProlif3 aunt d1 d2)
  $ retinalLineageData

countCellType c
  = genericLength
  $ concatMap (findSubLT $ matchCell c)
  $ concatMap (\(Progenitor _ l r) -> filter matchProg [l,r])
  $ retinalLineageData

printTableLn = mapM_ printRowLn

printTableWithHeadingsLn cols rows t = do
    putStr "\t"
    printHeadings cols
    putStrLn ""
    sequence_ $ zipWith (\h r -> do {putStr h; putStr "\t"; printRowLn r})
                        rows t
  where printHeadings = sequence_ . intersperse (putStr "\t") . map putStr

printRow = sequence_ . intersperse (putStr "\t") . map (putStr . show)
printRowLn r = do {printRow r; putStrLn ""}

-- differentiation channels vs absolute time


-- monte-carlo

exponentialVariable tau = do
  x <- getRandomR (0.0, 1.0)
  return $ -(log x) * tau

normalVariable mu sigma = do
  u <- getRandomR (0.0, 1.0)
  v <- getRandomR (0.0, 1.0)
  let x = sqrt(-2 * (log u)) * cos(2 * pi * v)
  return $ x*sigma + mu

logNormalVariable mu sigma = liftM exp $ normalVariable mu sigma

cellCycle tau
  | tau == 0.0 = do
      t1 <- cellCycle'
      t2 <- cellCycle'
      return $ abs (t1 - t2)
  | otherwise  = cellCycle'
  where 
    cellCycle' = logNormalVariable (mean logCycles) (sqrt $ variance logCycles)
    logCycles = map log cycleLengths

retinalFate _ = return Rph

theoreticalLineageTrees 
  = replicateM 100000
  $ generateRandomLineageTree cellCycle (const 0.04, const 0.66) retinalFate

theoryTLT = liftM (map toTimeLT) $ theoreticalLineageTrees
                        

main = do
  {-
  renderableToPDFFile (toRenderable $ toGraph [1..25] $ retinalLineageDataSets !! 0) (12*72) (6*72) "retinalLineageData1.pdf"
  renderableToPDFFile (toRenderable $ toGraph [26..51] $ retinalLineageDataSets !! 1) (12*72) (6*72) "retinalLineageData2.pdf"
  renderableToPDFFile (toRenderable $ toGraph [52..76] $ retinalLineageDataSets !! 2) (12*72) (6*72) "retinalLineageData3.pdf"
  renderableToPDFFile (toRenderable $ toGraph [77..101] $ retinalLineageDataSets !! 3) (12*72) (6*72) "retinalLineageData4.pdf"
  renderableToPDFFile (toRenderable $ toGraph [102..129] $ retinalLineageDataSets !! 4) (12*72) (6*72) "retinalLineageData5.pdf"
  
  renderableToPDFFile (toRenderable cycleLengthvsTimePlot) (5*72) (4*72) "cycleLengthvsTime.pdf"
  renderableToPDFFile (toRenderable cycleLengthHistPlot) (5*72) (4*72) "cycleLengthHist.pdf"

  renderableToPDFFile (toRenderable cycleLengthvsFatePlot) (4*72) (3*72) "cycleLengthvsFate.pdf"
  -}
  theory <- evalRandIO theoryTLT
  renderableToPDFFile (toRenderable $ finishTimesHistPlot retinalTLT $ filter (matchAllProlif matchProg matchAny) theory) (5*72) (4*72) "finishTimesHist.pdf"
  renderableToPDFFile (toRenderable firstDivisionTimeHistPlot) (5*72) (4*72) "firstDivisionTimeHist.pdf"
  
  let ntot = (countDivisions matchAny matchAny) + 208
      ndd  = (countDivisions matchDiff matchDiff) + 208
      npd  = countDivisions matchProg matchDiff
      npp  = countDivisions matchProg matchProg
      pdd  = ndd % ntot
      ppd  = npd % ntot
      ppp  = npp % ntot
  putStrLn $ "sym diff:    " ++ (show ndd) ++ "  " ++ (show $ round $ pdd * 100) ++ "%"
  putStrLn $ "asym diff:   " ++ (show npd) ++ "  " ++ (show $ round $ ppd * 100) ++ "%"
  putStrLn $ "sym prolif:  " ++ (show npp) ++ "  " ++ (show $ round $ ppp * 100) ++ "%"
  putStrLn $ "total:       " ++ (show ntot)
  putStr   $ "error:       " ++ (show $ ntot - ndd - npd - npp) ++ "  "
  putStrLn $ if (ntot - ndd - npd - npp) == 0 then "good" else "BAD!!!"

  let cellTypes  = enumFromTo Rph Mul
      totalDiffs = sum $ map countCellType cellTypes
  forM_ cellTypes (\c -> do
    putStr   $ (show c) ++ ": " ++ (show $ countCellType c) ++ "  "
    putStrLn $ (show $ round $ 100*(countCellType c) % totalDiffs) ++ "%")
  putStrLn $ "Tot: " ++ (show totalDiffs)

  let n3tot = countDivisions3 matchAny matchAny matchAny
      cellMatchers = matchProg:(map matchCell $ enumFromTo Rph Mul)
      bigTableOfDoom = [[countDivisions3 aunt d1 d2 |
                         i <- [0..4],
                         j <- dropWhile (<i) [0..4],
                         let d1 = cellMatchers !! i,
                         let d2 = cellMatchers !! j] | 
                        aunt <- cellMatchers]
      cellNames = map (:[]) "prabm"
      tableColHs = [d1 ++ d2 | i <- [0..4], j <- dropWhile (<i) [0..4],
                              let d1 = cellNames !! i,
                              let d2 = cellNames !! j]
  printTableWithHeadingsLn tableColHs cellNames bigTableOfDoom
