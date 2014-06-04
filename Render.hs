module Render
(	Renderable(..),
	Point,
	Color,
	Area(..),
	Workspace,
	Resolution,
	project, 
	initRenderable,
	render,
	pixelsGrid,
	optimizeGrid,
	scaleLine,
	inBounds,
	triToLine,
	cyan,
	red,
	green
	) 
where
import Data.List
import Data.Map (Map)
import Matrix

data Renderable m a = Renderable {
	_screen :: Workspace a,
	_col :: Color Int, 
	_lineMatrix :: [m a], 
	_triangleMatrix :: [m a]
} deriving (Show,Eq)


data Area a = Area {xRange :: (a,a), yRange :: (a,a)} deriving (Show, Eq)
type Workspace = Area
type Resolution = Area

instance Functor Area where
	fmap f (Area (x1,x2) (y1,y2)) = Area (f x1, f x2) (f y1, f y2)


type Point a = (a,a)
type Color a = (a,a,a)

cyan = (0,255,255) :: Color Int
red = (255,0,0) :: Color Int
green = (250,255,0) :: Color Int

--POST--

project :: Matrix m => (Double,Double,Double) -> [m Double] -> [m Double]
project (ex,ey,ez) m = map (Matrix.fromList . (map ((\(x,y,z) -> [x,y,z,1]) . (\(x:y:z:_)->perspective (ex,ey,ez) (x,y,z))) .  Matrix.rows)) m

perspective (ex,ey,ez) (px,py,pz) = (	ex - (ez * (px-ex)/(pz-ez)), 
					ey - (ez *(py-ey)/(pz-ez)), 
					0)


-------------------


initRenderable :: (Matrix m,Num a) => Workspace a -> Color Int -> Renderable m a
initRenderable scr col = Renderable scr col [] []

--Triangles

triToLine :: (Matrix m) => m a -> [m a]
triToLine mat = let tri = rows mat in zipWith (\a b -> fromList $ [a,b]) tri $ drop 1 (cycle tri)


render :: (Matrix m, Integral b,RealFrac a) => Resolution Int -> Renderable m a -> [(b,b,Color Int)]
render out (Renderable scr col mls mtri) = 
	optimizeGrid . sort . concat 
	. map (renderLine col) 
	. map (scaleLine scr (fmap fromIntegral out))
	. filter (inBounds scr) 
	. map pairLines
	. map rows $ lineMatrix
	where 
		lineMatrix = mls ++ concatMap triToLine mtri
		pairLines ((x1:y1:_):(x2:y2:_):_) = ((x1,y1),(x2,y2))


inBounds (Area (lx,hx) (ly,hy)) ((x1,y1),(x2,y2)) = inRange (lx,hx) x1 && inRange (lx,hx) x2 && inRange (ly,hy) y1 && inRange (ly,hy) y2
inRange (l,h) v = v >= l && v <= h

scaleLine :: (RealFrac a) => Area a -> Area a -> ((a,a),(a,a)) -> ((a,a),(a,a))
scaleLine src scale (p1,p2) = (toPixels src scale p1,toPixels src scale p2)

renderLine :: (Integral a,RealFrac b) => d -> (Point b,Point b) -> [(a,a,d)]
renderLine col (p1,p2) = pixelLine col (integralize p1) (integralize p2)
	where
		integralize (a,b) = (floor a,floor b)

toPixels :: Fractional a => Area a -> Area a -> Point a -> Point a
toPixels src scale (x,y) = (	sizeToRange (xRange src) (xRange scale) x, 
				sizeToRange (yRange src) (yRange scale) y)
	where 
		sizeToRange (l1,h1) (l2,h2) v = l2 + ((h2-l2) * (v-l1)/(h1-l1))


-- Bresenheim line algorithm
pixelLine :: Integral a => d -> (a,a) -> (a,a) -> [(a,a,d)] 
pixelLine col (x1,y1) (x2,y2) 
	| x1 == x2 = [(y,x1,col) | let ys = order y1 y2, y <- [snd ys..fst ys]]
	| abs (x2 - x1) >= abs (y2 - y1) = 
		[(y,x,col) |
			let axes = order (x1,y1) (x2,y2), 
			let yDist = (snd (fst axes)) - (snd (snd axes)), 
			let xDist = fst (fst axes) - fst (snd axes), 
			x <- [fst (snd axes)..fst (fst axes)], 
			let y = minor x xDist yDist axes]
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
pixelsGrid :: Integral a => (a,a) -> b -> [(a,a,b)] -> [b]
pixelsGrid (w,h) defaultPixel pixels = map getCol 
	$ compilePixelsGrid pixels 
	$ genGrid (w,h) defaultPixel

getCol :: Integral a=> (a,a,b) -> b
getCol (_,_,col) = col


--writes grid a into the corresponding slots in grid b
compilePixelsGrid :: Integral a => [(a,a,b)] -> [(a,a,b)] -> [(a,a,b)] 
compilePixelsGrid _ [] = []
compilePixelsGrid [] xs = xs
compilePixelsGrid src@((x1,y1,dat1):rem1) ((x2,y2,dat2):rem2)
	| (x1,y1) == (x2,y2) = (x1,y1,dat1) : compilePixelsGrid rem1 rem2
	| otherwise = (x2,y2,dat2) : compilePixelsGrid src rem2
