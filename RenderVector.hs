module RenderVector (
	initRenderable,
	renderVector,
	pixelLineVector,
	project,
	perspective,
	Renderable (..),
	Area (..),
	Workspace,
	Resolution,
	Point,
	Color,
) where
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Matrix
--renderVector :: (Matrix m,RealFrac a) => Resolution Int -> Renderable m a -> (Vector Int,Vector (Color Int))
data Renderable m a d = Renderable {
	_screen :: Workspace a,
	_col :: d, 
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



project :: Matrix m => (Double,Double,Double) -> [m Double] -> [m Double]
project (ex,ey,ez) m = map (Matrix.fromList . (map ((\(x,y,z) -> [x,y,z,1]) . (\(x:y:z:_)->perspective (ex,ey,ez) (x,y,z))) .  Matrix.rows)) m

perspective (ex,ey,ez) (px,py,pz) = (	ex - (ez * (px-ex)/(pz-ez)), 
					ey - (ez *(py-ey)/(pz-ez)), 
					0)

initRenderable :: (Matrix m,Num a) => Workspace a -> d -> Renderable m a d
initRenderable scr col = Renderable scr col [] []

renderVector :: (Matrix m,RealFrac a,V.Storable d) => Resolution Int -> Renderable m a d -> [(Vector Int,Vector d)]
renderVector out@(Area _ (_,oy)) (Renderable scr col mls mtri) = 
	 map (pixelLineVector col out)
	. map (\((a,b),(c,d)) -> ((floor a, floor b),(floor c,floor d)))
	. map (scaleLine scr (fmap fromIntegral out))
	. filter (inBounds scr) 
	. map pairLines
	. map rows $ lineMatrix
	where 
		lineMatrix = mls ++ concatMap triToLine mtri
		pairLines ((x1:y1:_):(x2:y2:_):_) = ((x1,y1),(x2,y2))

triToLine :: (Matrix m) => m a -> [m a]
triToLine mat = let tri = rows mat in zipWith (\a b -> fromList $ [a,b]) tri $ drop 1 (cycle tri)
--pixelLineVector :: d -> Resolution Int -> ((Int,Int),(Int,Int)) -> (Vector Int,Vector d)
pixelLineVector :: (V.Storable d) => d -> Resolution Int -> ((Int,Int),(Int,Int)) -> (Vector Int,Vector d)
pixelLineVector v (Area (_,ox) _) (sp,ep) = 
	(V.unfoldr (fmap (\((x,y),r) -> (y * ox + x,r)) . lineSeeder sp ep) (sp,getInitErr sp ep),
	V.replicate 10 v)

getInitErr :: (Int,Int) -> (Int,Int) -> Int
getInitErr (x0,y0) (x1,y1) = (abs $ x1 - x0) - (abs $ y1 - y0)

lineSeeder :: (Int, Int) -> (Int,Int) -> ((Int,Int),Int) -> Maybe ((Int,Int),((Int,Int),Int))
lineSeeder (x0,y0) (x1,y1) ( (x,y) , err)  = case (x,y) == (x1,y1) of
	False -> let 
			e2 = 2*err 
			nErr = 	(if e2 > (-dy) then err - dy else err) +
				(if e2 < dx then dx else 0)
			nx = if e2 > (-dy) then x + sx else x
			ny = if e2 < dx then y + sy else y
			nP = (nx,ny)
		in
		Just (nP,(nP,nErr))
		
	True -> Nothing
	where
		dx = abs $ x1 - x0
		dy = abs $ y1 - y0
		sx = if x0 < x1 then 1 else -1
		sy = if y0 < y1 then 1 else -1


scaleLine :: (RealFrac a) => Area a -> Area a -> ((a,a),(a,a)) -> ((a,a),(a,a))
scaleLine src scale (p1,p2) = (toPixels src scale p1,toPixels src scale p2)

toPixels :: Fractional a => Area a -> Area a -> Point a -> Point a
toPixels src scale (x,y) = (	sizeToRange (xRange src) (xRange scale) x, 
				sizeToRange (yRange src) (yRange scale) y)
	where 
		sizeToRange (l1,h1) (l2,h2) v = l2 + ((h2-l2) * (v-l1)/(h1-l1))

inBounds (Area (lx,hx) (ly,hy)) ((x1,y1),(x2,y2)) = inRange (lx,hx) x1 && inRange (lx,hx) x2 && inRange (ly,hy) y1 && inRange (ly,hy) y2
inRange (l,h) v = v >= l && v <= h
