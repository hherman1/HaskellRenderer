module Main 
(
	main
	)
where
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Data.IORef
import OpenGL
import ControlParsers
import Render
import Matrix

defaultColor :: (Float, Float, Float)
defaultColor = (150,150,150) 


main :: IO ()
main = do
	(_progName,_args) <- getArgsAndInitialize
	initialWindowSize $= Size (500 :: GLsizei) (500 :: GLsizei)
	_window <- createWindow "im not sure i really understand what im doing in haskell" 
	-- viewport $= (Position 0 0, Size 1000 1000)
	buffer <- newIORef []
	-- pixels <- newIORef $ (Area (0,500) (0,500) :: Area Float)
	-- readInput buffer
	displayCallback $= display buffer
	idleCallback $= Just ((\buf -> do 
		readInput buf
		postRedisplay Nothing) buffer)
	-- reshapeCallback $= Just (reshape pixels)
	mainLoop

windowArea :: Size -> Area Float
windowArea (Size x y) = Area (0,fromIntegral x) (0,fromIntegral y)

readInput :: ScreenBuffer -> IO (Renderable ListMatrix Float)
readInput buffer = do
	winsize <- get windowSize
	comms <- fmap lines $ retrieve
	let 
		varys = parseVarys comms
	initControlParser comms 0 varys buffer $ (initRenderable defaultArea (windowArea winsize) defaultColor :: Renderable ListMatrix Float)
	where
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
