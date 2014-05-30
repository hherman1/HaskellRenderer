module Main 
(
	main
	)
where

import Data.IORef
import Control.Monad

import qualified Data.Map as ML

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import OpenGL
import Parse
import ControlParsers
import Render
import Matrix

defaultColor :: Render.Color Int
defaultColor = (150,150,150) 


main :: IO ()
main = do
	(_progName,_args) <- getArgsAndInitialize
	initialWindowSize $= Size (400 :: GLsizei) (400 :: GLsizei)
	_window <- createWindow "im not sure i really understand what im doing in haskell" 
	-- viewport $= (Position 0 0, Size 1000 1000)
	-- pixels <- newIORef $ (Area (0,500) (0,500) :: Area Float)
	-- readInput buffer
	displayCallback $= display (Size 0 0) []
	idleCallback $= Just (do 
		t <- readInput
		postRedisplay Nothing)
	-- reshapeCallback $= Just (reshape pixels)
	mainLoop

windowArea :: Size -> Area Int
windowArea (Size x y) = Area (0,fromIntegral x) (0,fromIntegral y)

readInput :: IO (Renderable ListMatrix Float)
readInput = do
	winsize <- get windowSize
	comms <- fmap lines $ retrieve
	let 
		varys = parseVarys comms
	fmap last $ mapM (parseFrame comms varys winsize defaultColor) [1..100]
	where
		parseFrame comms varys winsize defaultColor n = 
			initControlParser 
				comms
				(Parse3D n varys (identity 4 4) $ ML.fromList [])
				(initRenderable 
					defaultArea 
					(windowArea winsize) 
					defaultColor 
					:: Renderable ListMatrix Float)
		defaultArea = Area {xRange = (0,0), yRange = (0,0)}
	--output <- initParse (lines comms) (windowArea winsize) defaultColor
	--buffer $= map (\(x,y,b) -> (toRational x, toRational y, b)) output
	--buffer

retrieve :: IO String
retrieve = do
	comm <- getLine
	case words comm of
		("read":file:[]) -> readFile file;
		("parse":_) -> getContents
		("quit":_) -> error "Program exiting"
		_ -> return "end"
