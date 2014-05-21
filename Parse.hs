module Parse 
(	
	initParseIO,
	)
where 

import System.Environment
import System.IO
import Data.IORef
import Data.List (sort)

import Data.Map (Map)
import qualified Data.Map as ML

import Control.Monad.Fix

import OpenGL
import Graphics.UI.GLUT.Window (postRedisplay)

import Parsers
import Matrix
import Matrix3D
import Objects
import Render
import PPM

-- Input, Screen to size from, Output to size to, transformation matrix, line matrix
maxColor = 255

cyan = (0,255,255)
red = (255,0,0)
green = (250,0,0.65)



initParseIO :: [String] -> ScreenBuffer -> Output Float -> (Float,Float,Float) -> IO ()
initParseIO ls sbuf out col = parseIO ls sbuf $ (initRenderable defaultArea out col :: Renderable ListMatrix Float)
	where 
		defaultArea = Area {xRange = (0,0), yRange = (0,0)}


parse :: Matrix m => String -> Map String (Sequence a) -> Renderable m Float -> Renderable m Float
parse str varys buf = let (w:ws) = words str in case ML.lookup w parsers of
	(Just f) -> let args = varyValues ws varys in f args buf
	Nothing -> buf


parseVarys :: [String] -> Map String (Sequence Float)
parseVarys [] = fromList []

parseIO :: (Show (m Float), Matrix m) => [String] -> ScreenBuffer -> Renderable m Float -> IO ()
parseIO (l:ls) sbuf buffer@(Renderable scr out col edge mls mtri)
	| w == "end" = do
		return ()
	| w == "file" = do
		let 
			buf = renderFile buffer
		writeFile (head ws) $ showPPM out maxColor buf
		parseIO ls sbuf buffer
	| w == "render-parallel" = do
		print "we doin this render\n"
		let
			output = optimizeGrid $ render buffer
		print output
		writeIORef sbuf $ output
		display sbuf
		parseIO ls sbuf buffer
	| w == "render-perspective-cyclops" = do
		let 
			(ex:ey:ez:_) = readFloats ws
		displayBuffer sbuf $ buffer {
			_col = green, 
			_lineMatrix = project (ex,ey,ez) mls, 
			_triangleMatrix = project (ex,ey,ez) $ filter (backFace [ex,ey,ez] . rows) mtri
		}
		parseIO ls sbuf buffer
	| w == "render-perspective-stereo" = do
		let
			(ex1:ey1:ez1:ex2:ey2:ez2:_) = readFloats ws
			left = render $ buffer {
				_col = cyan, 
				_lineMatrix = projCull (ex1,ey1,ez1) mls, 
				_triangleMatrix = projCull (ex1,ey1,ez1) mls
			}
			right = render $ buffer {_col = red, _lineMatrix = project (ex2,ey2,ez2) mls, _triangleMatrix = project (ex2,ey2,ez2) mls}
			stereo = sort (left ++ right)
		writeIORef sbuf $ optimizeGrid stereo
		display sbuf
		parseIO ls sbuf buffer
	| w == "spinc" = do
		let
			(ex:ey:ez:rx:ry:rz:_) = readFloats ws
			rotate x y z = matrixProduct (rotateX x) . matrixProduct (rotateY y) $ rotateZ z
		renderBuf <- newIORef buffer
		fix $ \loop -> do
			tbuf@(Renderable _ _ tcol tedge tmls tmtri) <- readIORef renderBuf
			writeIORef renderBuf $ tbuf {
				_col = green,
				_lineMatrix = map ((flip matrixProduct) (rotate rx ry rz)) $ tmls, 
				_triangleMatrix =  map ((flip matrixProduct) (rotate rx ry rz)) $ tmtri
			}
			writeIORef sbuf $ optimizeGrid $ render $ tbuf {
				_lineMatrix = project (ex,ey,ez) $ _lineMatrix tbuf,
				_triangleMatrix = projCull (ex,ey,ez) $ _triangleMatrix tbuf	
			}
			display sbuf
			loop
	| otherwise = parseIO ls sbuf $ parse l buffer
	where
		(w:ws) = words l
		readInts src = map read src ::[Int]
		displayBuffer sbuf buf = do 
			writeIORef sbuf $ optimizeGrid $ render buf
			display sbuf
		cull eye = filter (backFace eye . rows)
		projCull eye@(ex,ey,ez) = project eye $ cull [ex,ey,ez]


-- Functions
