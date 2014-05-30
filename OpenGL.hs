module OpenGL
(
	Vertices,
	ScreenBuffer,
	toSize,
	reshape,
	display
	)
where

import Foreign (newArray)
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Data.IORef
import Render

type Vertices d = [(Int,Int,d)]
type ScreenBuffer = Vertices (Int,Int,Int)

toSize :: (Integral a) => Area a -> Size
toSize (Area (xl,xh) (yl,yh)) = Size (fromIntegral $ xh-xl) (fromIntegral $ yh-yl)

reshape :: IORef (Area Float) -> ReshapeCallback
reshape pixels winsize = do
	(Area (_,w) (_,h)) <- get pixels
	viewport $= (Position 0 0, Size (toGLint w) (toGLint h))
	where
		toGLint = fromIntegral . truncate

toImage :: [Render.Color Int] -> IO (PixelData (Color3 GLubyte))
toImage ps = fmap (PixelData RGB UnsignedByte) $
	newArray $
	map (\(r,g,b) -> Color3 (toGLubyte r) (toGLubyte g) (toGLubyte b)) $
	ps
	where
		toGLubyte :: Int -> GLubyte
		toGLubyte = fromIntegral

display :: Size -> [Render.Color Int] -> DisplayCallback
display size@(Size w h) ps = do
	-- viewport $= (Position (0) (0), Size 1 1)
	
	clear [ ColorBuffer ]
	--let rasterPos2i = rasterPos :: Vertex2 GLint -> IO ()
	--rasterPos2i (Vertex2 0 0)
	case ps of
		[] -> return ()
		draw -> do 
			pixels <- toImage draw
			putStrLn $ show size
			drawPixels size pixels
			flush
{-
	
	renderPrimitive Points $ mapM_ (\(x,y,(r,g,b)) -> do 
		let 
			pair = convert (((fromIntegral x) - (width / 2)) / width) (((fromIntegral y)-(height / 2)) / height)
		color $ Color3 (toGLfloat r) (toGLfloat g) (toGLfloat b)
		vertex $ Vertex3 (snd pair) (- (fst pair)) 0 ) ps
	flush
	where
		toGLfloat :: Real a => a -> GLfloat
		toGLfloat = fromRational . toRational
-}

