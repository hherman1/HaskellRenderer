module PPM
(	bufToPPM,
	showPPM,
	maxColor
	)
where
import Render

maxColor :: (Num a) => a
maxColor = 255 

bufToPPM :: (Integral b) => Resolution Int -> [(b,b,Color Int)] -> [Color Int]
bufToPPM out coords = pixelsGrid (wh $ fmap fromIntegral out) (0,0,0) . optimizeGrid $ coords
	where
	wh (Area (xl,xh) (yl,yh)) = (xh-xl,yh-yl)

showPPM :: Area Int -> Int -> [(Int,Int,Int)] -> String
showPPM window colDepth buffer = (showHeaderPPM (wh window) colDepth "P3") ++ "\n" ++ (showColGridPPM buffer) ++ "\n"
	where wh (Area (xl,xh) (yl,yh)) = (xh-xl,yh-yl)

showHeaderPPM :: (Integral a, Show a) => (a,a) -> a -> String -> String
showHeaderPPM (w,h) colDepth form = form ++ " " ++ (show w) ++ " " ++ (show h) ++ " " ++ (show colDepth)

showColGridPPM :: (Integral a,Show a) => [(a,a,a)] -> String 
showColGridPPM [] = []
showColGridPPM ((r,g,b):cols) = (" " ++ (show r) ++ " " ++ (show g) ++ " " ++ (show b)) ++ showColGridPPM cols
