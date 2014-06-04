module RenderVector (pixelLineVector) where
import Data.Vector hiding (map,filter,(++),concatMap)
import qualified Data.Vector as V (map)
import Render
import Matrix
--renderVector :: (Matrix m,RealFrac a) => Resolution Int -> Renderable m a -> (Vector Int,Vector (Color Int))

renderVector :: (Matrix m,RealFrac a) => Resolution Int -> Renderable m a -> [Vector ((Int,Int),Color Int)]
renderVector out (Renderable scr col mls mtri) = 
	map (pixelLineVector col)
	. map (\((a,b),(c,d)) -> ((floor a, floor b),(floor c,floor d)))
	. map (scaleLine scr (fmap fromIntegral out))
	. filter (inBounds scr) 
	. map pairLines
	. map rows $ lineMatrix
	where 
		lineMatrix = mls ++ concatMap triToLine mtri
		pairLines ((x1:y1:_):(x2:y2:_):_) = ((x1,y1),(x2,y2))

pixelLineVector :: d -> ((Int,Int),(Int,Int)) -> (Vector ((Int,Int),d))
pixelLineVector v (sp,ep) = unfoldr (lineSeeder v sp ep) (sp,getInitErr sp ep)

getInitErr :: (Int,Int) -> (Int,Int) -> Int
getInitErr (x0,y0) (x1,y1) = (abs $ x1 - x0) - (abs $ y1 - y0)

lineSeeder :: d -> (Int, Int) -> (Int,Int) -> ((Int,Int),Int) -> Maybe (((Int,Int),d),((Int,Int),Int))
lineSeeder v (x0,y0) (x1,y1) ( (x,y) , err)  = case (x,y) == (x1,y1) of
	False -> let 
			e2 = 2*err 
			nErr = 	(if e2 > (-dy) then err - dy else err) +
				(if e2 < dx then dx else 0)
			nx = if e2 > (-dy) then x + sx else x
			ny = if e2 < dx then y + sy else y
			nP = (nx,ny)
		in
		Just ((nP,v),(nP,nErr))
		
	True -> Nothing
	where
		dx = abs $ x1 - x0
		dy = abs $ y1 - y0
		sx = if x0 < x1 then 1 else -1
		sy = if y0 < y1 then 1 else -1
