module Render
(	Renderable(..),
	Point,
	Area(..),
	Screen,
	Output,
	project, 
	initRenderable,
	renderFile,
	render,
	triToLine,
	scaleLineMatrix,
	optimizeGrid
	) 
where
import Data.List
import Matrix

data Renderable m a = Renderable {
	_screen :: Screen a,
	_out :: Output a,
	_col :: Color a, 
	_edgematrix :: m a, 
	_lineMatrix :: [m a], 
	_triangleMatrix :: [m a]
} deriving (Show,Eq)


data Area a = Area {xRange :: (a,a), yRange :: (a,a)} deriving (Show, Eq)
type Screen = Area
type Output = Area

type Point a = (a,a)
type Color a = (a,a,a)


--POST--

project :: Matrix m => (Float,Float,Float) -> [m Float] -> [m Float]
project (ex,ey,ez) m = map (Matrix.fromList . (map ((\(x,y,z) -> [x,y,z,1]) . (\(x:y:z:_)->perspective (ex,ey,ez) (x,y,z))) .  Matrix.rows)) m

perspective (ex,ey,ez) (px,py,pz) = (ex - (ez * (px-ex)/(pz-ez)), ey - (ez *(py-ey)/(pz-ez)), 0)


-------------------


initRenderable :: (Matrix m,Num a) => Screen a -> Output a -> Color a -> Renderable m a
initRenderable scr out col = Renderable scr out col (identity 4 4) [] []

renderFile :: (Matrix m, RealFrac a, Ord a, Integral b) => Renderable m a -> [(b,b,b)]
renderFile buffer@(Renderable scr out col edge mls mtri) = pixelsGridOM (wh out) (0,0,0) . map integralize . optimizeGrid $ render buffer
	where 
		wh :: (RealFrac a,Integral b) => Area a -> (b,b)
		wh (Area (xl,xh) (yl,yh)) = (floor $ xh-xl,floor $ yh-yl)
		integralize :: (Integral a,RealFrac b) => (a,a,(b,b,b)) -> (a,a,(a,a,a))
		integralize (x,y,(r,g,b)) = (x,y,(truncate r,truncate g,truncate b))

--Triangles

triToLine :: (Matrix m) => m a -> [m a]
triToLine mat = let tri = rows mat in zipWith (\a b -> fromList $ [a,b]) tri $ drop 1 (cycle tri)

--renderLineMatrix :: (Matrix m,RealFrac a,Integral b) => [m a] -> Area a -> Area a -> (b,b,b) -> [(b,b,(b,b,b))]
render :: (Matrix m, RealFrac a, Ord a, Integral b) => Renderable m a -> [(b,b,(a,a,a))]
render (Renderable scr out col edge mls mtri) = sort . concat . flip renderPointArray col $ scaleLineMatrix lineMatrix scr out
	where lineMatrix = mls ++ concatMap triToLine mtri

scaleLineMatrix :: (Matrix m, RealFrac a) => [m a] -> Area a -> Area a -> [[Point a]]
scaleLineMatrix ls src scale = map ( map (toPixels src scale . (\(x:y:_) -> (x,y))) . rows) ls

renderPointArray :: (Integral a,RealFrac b) => [[Point b]] -> (b,b,b) -> [[(a,a,(b,b,b))]]
renderPointArray ls col = map (\(p1:p2:_) -> pixelLine (integralize p1) (integralize p2) col) ls
	where
		integralize (a,b) = (floor a,floor b)

toPixels :: Fractional a => Area a -> Area a -> Point a -> Point a
toPixels src scale p = (sizeToRange (xRange src) (xRange scale) $ fst p, sizeToRange (yRange src) (yRange scale) $ snd p)
	where 
		sizeToRange (l,h) (n,w) p = n + ((w-n) * (p-l)/(h-l))

-- Bresenheim line algorithm
pixelLine :: Integral a => (a,a) -> (a,a) -> (b,b,b) -> [(a,a,(b,b,b))] 
pixelLine (x1,y1) (x2,y2) col
	| x1 == x2 = [(y,x1,col) | let ys = order y1 y2, y <- [snd ys..fst ys]]
	| abs (x2 - x1) >= abs (y2 - y1) = [(y,x,col) |let axes = order (x1,y1) (x2,y2), let yDist = (snd (fst axes)) - (snd (snd axes)), let xDist = fst (fst axes) - fst (snd axes), x <- [fst (snd axes)..fst (fst axes)], let y = minor x xDist yDist axes]
	| otherwise = [(y,x,col) |let axes = order (y1,x1) (y2,x2), let xDist = (snd (fst axes)) - (snd (snd axes)), let yDist = fst (fst axes) - fst (snd axes), y <- [fst (snd axes)..fst (fst axes)],let x = minor y yDist xDist axes]
	where
		order p1 p2
			| p1 > p2 = (p1,p2)
			| otherwise = (p2,p1);
		minor maj majDist minDist axes = let cur = maj - fst (snd axes) in snd (snd axes) + div (cur * minDist) majDist



-- Eliminates duplicated pixels in a sorted pixel grid
-- Intended use: occlude invisible pixels from non default list before processing with pixelsGrid
optimizeGrid :: Integral a => [(a,a,b)] -> [(a,a,b)]
optimizeGrid [] = []
optimizeGrid grid@((xi,yi,dat):ds) = (xi,yi,dat) : optGHelp (xi,yi) ds
	where 	optGHelp _ [] = []
		optGHelp loc ((x,y,d):rem) 
			| loc == (x,y) = optGHelp loc rem
			| otherwise = (x,y,d) : optGHelp (x,y) rem

genGrid :: Integral a => (a,a) -> b -> [(a,a,b)]
genGrid (w,h) dat = [(y,x,dat) | y <- [0..h-1], x <- [0..w-1]]

-- renders a pixelsGrid from the new samples, wherein w * h pixels are rendered within which the pixels from the changed pixels list
-- are drawn in their expected location, while default pixels are drawn elsewhere

--STANDARD pixelsGrid function, used for final data retrieval
-- fastest and optimized for all scenarios thanks to haskell's lazy eval
pixelsGridOM :: Integral a => (a,a) -> (a,a,a) -> [(a,a,(a,a,a))] -> [(a,a,a)]
pixelsGridOM (w,h) defaultPixel pixels = map getCol $ compilePixelsGrid pixels $ genGrid (w,h) defaultPixel

getCol :: Integral a=> (a,a,(a,a,a)) -> (a,a,a)
getCol (_,_,col) = col


--writes grid a into the corresponding slots in grid b
compilePixelsGrid :: Integral a => [(a,a,b)] -> [(a,a,b)] -> [(a,a,b)] 
compilePixelsGrid _ [] = []
compilePixelsGrid [] xs = xs
compilePixelsGrid src@((x1,y1,dat1):rem1) ((x2,y2,dat2):rem2)
	| (x1,y1) == (x2,y2) = (x1,y1,dat1) : compilePixelsGrid rem1 rem2
	| otherwise = (x2,y2,dat2) : compilePixelsGrid src rem2
