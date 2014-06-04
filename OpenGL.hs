module OpenGL
(
	Vertices,
	ScreenBuffer,
	toSize,
	reshape,
	display,
	displayVector,
	cyan,
	red,
	green
	)
where

import Foreign (newArray)
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Data.IORef
import RenderVector

import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V

cyan = Color3 0 255 255:: Color3 GLubyte
red = Color3 255 0 0 :: Color3 GLubyte
green = Color3 0 255 0 :: Color3 GLubyte

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

toImage :: [RenderVector.Color Int] -> IO (PixelData (Color3 GLubyte))
toImage ps = fmap (PixelData RGB UnsignedByte) $
	newArray $
	map (\(r,g,b) -> Color3 (toGLubyte r) (toGLubyte g) (toGLubyte b)) $
	ps
	where
		toGLubyte :: Int -> GLubyte
		toGLubyte = fromIntegral
{-
toStorable :: Vector (RenderVector.Color Int) -> IO (PixelData (Color3 GLubyte))
toStorable v = do
	let vec = convert $
		V.map (\(r,g,b) -> Color3 (toGLubyte r) (toGLubyte g) (toGLubyte b)) $ v
	VS.unsafeWith vec (return . PixelData RGB UnsignedByte)
	where
		toGLubyte :: Int -> GLubyte
		toGLubyte = fromIntegral
-}


displayVector :: (V.Storable d) => Size -> Vector d -> DisplayCallback
displayVector size@(Size w h) ps = do
	clear [ ColorBuffer ]
	pixels <- V.unsafeWith ps (return . PixelData RGB UnsignedByte)
	putStrLn $ show size
	drawPixels size pixels
	flush

display :: Size -> [RenderVector.Color Int] -> DisplayCallback
display size@(Size w h) ps = do
	clear [ ColorBuffer ]
	case ps of
		[] -> return ()
		draw -> do 
			pixels <- toImage draw
			putStrLn $ show size
			drawPixels size pixels
			flush
