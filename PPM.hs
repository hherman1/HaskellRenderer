module PPM
(	
	showPPM,
	maxColor
	)
where
import RenderVector
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

maxColor :: (Num a) => a
maxColor = 255 

{-
bufToPPM :: (Integral b) => Resolution Int -> [(b,b,Color Int)] -> [Color Int]
bufToPPM out coords = pixelsGrid (wh $ fmap fromIntegral out) (0,0,0) . optimizeGrid $ coords
	where
	wh (Area (xl,xh) (yl,yh)) = (xh-xl,yh-yl)
-}
showPPM :: Area Int -> Int -> Vector (Color Int) -> String
showPPM window colDepth buffer = (showHeaderPPM (wh window) colDepth "P3") ++ "\n" ++ (showColGridPPM buffer) ++ "\n"
	where wh (Area (xl,xh) (yl,yh)) = (xh-xl,yh-yl)

showHeaderPPM :: (Integral a, Show a) => (a,a) -> a -> String -> String
showHeaderPPM (w,h) colDepth form = form ++ " " ++ (show w) ++ " " ++ (show h) ++ " " ++ (show colDepth)

showColGridPPM :: (V.Unbox a, Integral a,Show a) => Vector (a,a,a) -> String 
showColGridPPM vec = V.foldr (\(r,g,b) s -> 
			   " " ++ (show r) 
			++ " " ++ (show g) 
			++ " " ++ (show b) 
			++ " " ++ s) "" vec
--showColGridPPM ((r,g,b):cols) = (" " ++ (show r) ++ " " ++ (show g) ++ " " ++ (show b)) ++ showColGridPPM cols
