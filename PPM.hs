module PPM
(	showPPM
	)
where
import Render

showPPM :: (RealFrac a,Integral b,Show b) => Area a -> Int -> [(b,b,b)] -> String
showPPM window colDepth buffer = (showHeaderPPM (wh window) colDepth "P3") ++ "\n" ++ (showColGridPPM buffer) ++ "\n"
	where wh (Area (xl,xh) (yl,yh)) = (floor $ xh-xl,floor $ yh-yl)

showHeaderPPM :: (Integral a, Show a) => (a,a) -> a -> String -> String
showHeaderPPM (w,h) colDepth form = form ++ " " ++ (show w) ++ " " ++ (show h) ++ " " ++ (show colDepth)

showColGridPPM :: (Integral a,Show a) => [(a,a,a)] -> String 
showColGridPPM [] = []
showColGridPPM ((r,g,b):cols) = (" " ++ (show r) ++ " " ++ (show g) ++ " " ++ (show b)) ++ showColGridPPM cols
