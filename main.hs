import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Data.IORef
import Parse
import Render

defaultColor :: (Float, Float, Float)
defaultColor = (150,150,150) 


main :: IO ()
main = do
	(_progName,_args) <- getArgsAndInitialize
	_window <- createWindow "im not sure i really understand what im doing in haskell" 
	initialWindowSize $= Size (1000 :: GLsizei) (1000 :: GLsizei)
	buffer <- newIORef []
	pixels <- newIORef $ (Area (0,500) (0,500) :: Area Float)
	-- readInput buffer
	displayCallback $= display buffer
	idleCallback $= Just ((\buf -> do 
		readInput buf
		postRedisplay Nothing) buffer)
	reshapeCallback $= Just (reshape pixels)
	mainLoop

windowArea :: Size -> Area Float
windowArea (Size x y) = Area (0,fromIntegral x) (0,fromIntegral y)

readInput :: ScreenBuffer -> IO ()
readInput buffer = do
	winsize <- get windowSize
	comms <- retrieve
	initParseIO (lines comms) buffer (windowArea winsize) defaultColor
	--output <- initParse (lines comms) (windowArea winsize) defaultColor
	--buffer $= map (\(x,y,b) -> (toRational x, toRational y, b)) output
	--buffer

reshape :: IORef (Area Float) -> ReshapeCallback
reshape pixels winsize = do
	(Area (_,w) (_,h)) <- get pixels
	viewport $= (Position 0 0, Size (toGLint w) (toGLint h))
	where
		toGLint = fromIntegral . truncate


display :: ScreenBuffer -> DisplayCallback
display buffer = do
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
retrieve :: IO String
retrieve = do
	comm <- getLine
	case words comm of
		("read":file:[]) -> readFile file;
		("parse":_) -> getContents
		("quit":_) -> error "Program exiting"
		_ -> return "end"