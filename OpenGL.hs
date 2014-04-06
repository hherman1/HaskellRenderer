module OpenGL
(
	Vertices,
	ScreenBuffer,
	reshape,
	display
	)
where

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Data.IORef
import Render

type Vertices d = [(Int,Int,d)]
type ScreenBuffer = IORef (Vertices (Float,Float,Float))

reshape :: IORef (Area Float) -> ReshapeCallback
reshape pixels winsize = do
	(Area (_,w) (_,h)) <- get pixels
	viewport $= (Position 0 0, Size (toGLint w) (toGLint h))
	where
		toGLint = fromIntegral . truncate


display :: ScreenBuffer -> DisplayCallback
display buffer = do
	-- viewport $= (Position (0) (0), Size 1 1)
	clear [ ColorBuffer ]
	ps <- get buffer
	(Size w h) <- get windowSize
	let
		convert x y = (toGLfloat x,toGLfloat y)
		width = fromIntegral w
		height = fromIntegral h

	
	renderPrimitive Points $ mapM_ (\(x,y,(r,g,b)) -> do 
		let 
			pair = convert (((fromIntegral x) - (width / 2)) / width) (((fromIntegral y)-(height / 2)) / height)
		color $ Color3 (toGLfloat r) (toGLfloat g) (toGLfloat b)
		vertex $ Vertex3 (snd pair) (- (fst pair)) 0 ) ps
	flush
	where
		toGLfloat :: Real a => a -> GLfloat
		toGLfloat = fromRational . toRational
