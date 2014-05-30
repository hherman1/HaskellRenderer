module PPM
(	bufToPPM,
	showPPM,
	maxColor
	)
where
import Render

maxColor :: (Num a) => a
maxColor = 255 

bufToPPM :: (RealFrac a,Ord a, Integral b) => Resolution a -> [(b,b,(a,a,a))] -> [(b,b,b)]
bufToPPM out coords = pixelsGrid (wh out) (0,0,0) . map integralize . optimizeGrid $ coords
	where
	wh :: (RealFrac a,Integral b) => Area a -> (b,b)
	wh (Area (xl,xh) (yl,yh)) = (floor $ xh-xl,floor $ yh-yl)
	integralize :: (Integral a,RealFrac b) => (a,a,(b,b,b)) -> (a,a,(a,a,a))
	integralize (x,y,(r,g,b)) = (x,y,(truncate r,truncate g,truncate b))

showPPM :: (RealFrac a) => Area a -> Int -> [(Int,Int,Int)] -> String
showPPM window colDepth buffer = (showHeaderPPM (wh window) colDepth "P3") ++ "\n" ++ (showColGridPPM buffer) ++ "\n"
	where wh (Area (xl,xh) (yl,yh)) = (floor $ xh-xl,floor $ yh-yl)

showHeaderPPM :: (Integral a, Show a) => (a,a) -> a -> String -> String
showHeaderPPM (w,h) colDepth form = form ++ " " ++ (show w) ++ " " ++ (show h) ++ " " ++ (show colDepth)

showColGridPPM :: (Integral a,Show a) => [(a,a,a)] -> String 
showColGridPPM [] = []
showColGridPPM ((r,g,b):cols) = (" " ++ (show r) ++ " " ++ (show g) ++ " " ++ (show b)) ++ showColGridPPM cols
