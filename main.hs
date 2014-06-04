module Main 
(
	main
	)
where

import Data.IORef
import Control.Monad
import Text.Parsec
import Control.Monad.Trans.State (evalStateT)

import qualified Data.Map as ML

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import OpenGL
import Render
import Matrix
import Parser
import Execute

defaultColor :: Render.Color Int
defaultColor = (150,150,150) 


main :: IO ()
main = do
	(_progName,_args) <- getArgsAndInitialize
	case _args of
		(wx:wy:_) -> initialWindowSize $= Size (read wx) (read wy)
		_ -> initialWindowSize $= Size 600 600
	_window <- createWindow "im not sure i really understand what im doing in haskell" 
	-- viewport $= (Position 0 0, Size 1000 1000)
	-- pixels <- newIORef $ (Area (0,500) (0,500) :: Area Float)
	-- readInput buffer
	displayCallback $= display (Size 0 0) []
	idleCallback $= Just idleLoop
	-- reshapeCallback $= Just (reshape pixels)
	mainLoop

windowArea :: Size -> Area Int
windowArea (Size x y) = Area (0,fromIntegral x) (0,fromIntegral y)


idleLoop :: IO ()
idleLoop = do
	winsize <- get windowSize
	comms <- retrieve
	cs <- readInput comms
	putStrLn $ show cs
	let 
		defaultArea = Area (-2,2) (-2,2)
		renderable = initRenderable 
				defaultArea
				defaultColor
	mapM_ (evalStateT (mapM_ runCommand cs)) $ 
		map (genState renderable (windowArea winsize)) [1..100]
	postRedisplay Nothing

readInput :: String -> IO [Command]
readInput comms = do
	case parse parseContents "Reading input" comms of
		(Left err) -> error $ "Error reading input:\n" ++ show err
		(Right cs) -> do
			return cs

{-
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
-}

retrieve :: IO String
retrieve = do
	comm <- getLine
	case words comm of
		("read":file:[]) -> readFile file;
		("parse":_) -> getContents
		("quit":_) -> error "Program exiting"
		_ -> return "end"
