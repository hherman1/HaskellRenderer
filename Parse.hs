module Parse 
(	
	initParseIO,
	)
where 

import System.Environment
import System.IO
import Data.IORef
import Data.List (sort)

import Control.Monad.Fix

import OpenGL
import Graphics.UI.GLUT.Window (postRedisplay)


import Matrix
import Matrix3D
import Objects
import Render
import PPM

-- Input, Screen to size from, Output to size to, transformation matrix, line matrix
maxColor = 255
sphereDivisions = 10

cyan = (0,255,255)
red = (255,0,0)
green = (0,150,0)

{- initParse :: (Matrix m) => [String] -> Output Float -> (Float,Float,Float) -> Renderable m Float
initParse str winSize col = parseIO str (Renderable defaultArea winSize col [] []) $ identity 4 4
	where 
		defaultArea = Area {xRange = (0,0), yRange = (0,0)}
-}
initParseIO :: [String] -> ScreenBuffer -> Output Float -> (Float,Float,Float) -> IO ()
initParseIO ls sbuf out col = parseIO ls sbuf $ (initRenderable defaultArea out col :: Renderable ListMatrix Float)
	where 
		defaultArea = Area {xRange = (0,0), yRange = (0,0)}

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
		writeIORef sbuf $ optimizeGrid $ render $ buffer {_col = green, _linematrix = project (ex,ey,ez) mls, _triangleMatrix = project (ex,ey,ez) $ filter (backFace [ex,ey,ez] . rows) mtri}
		display sbuf
		parseIO ls sbuf buffer
	| w == "render-perspective-stereo" = do
		let
			(ex1:ey1:ez1:ex2:ey2:ez2:_) = readFloats ws
			left = render $ buffer {_col = cyan, _linematrix = project (ex1,ey1,ez1) mls, _triangleMatrix = project (ex1,ey1,ez1) mls}
			right = render $ buffer {_col = red, _linematrix = project (ex2,ey2,ez2) mls, _triangleMatrix = project (ex2,ey2,ez2) mls}
			stereo = sort (left ++ right)
		writeIORef sbuf $ optimizeGrid stereo
		display sbuf
		parseIO ls sbuf buffer
	| w == "spinc" = do
		let
			(ex:ey:ez:_) = readFloats ws
		writeIORef sbuf $ optimizeGrid $ render $ buffer {_col = green, _linematrix = project (ex,ey,ez) mls, _triangleMatrix = project (ex,ey,ez) $ filter (backFace [ex,ey,ez] . rows) mtri}
		display sbuf
	| otherwise = parseIO ls sbuf $ parse l buffer
	where
		(w:ws) = words l
		readInts src = map read src ::[Int]
		readFloats src = map read . map (\(a:as) -> case a of '.' -> '0':a:as; '-' -> a:'0':as; _ -> a:as) $ src ::[Float]

--parseIO :: (Show (m Float), Matrix m, Integral a, Show a) => [String] -> Screen Float -> Output Float-> m Float -> [m Float] -> (a,a,a)-> IO [(a,a,(a,a,a))]
parse :: Matrix m => String -> Renderable m Float -> Renderable m Float
parse l buffer@(Renderable scr out col edge mls mtri)
	--Config
	| w == "screen" = let (xl:yl:xh:yh:_) = readFloats ws in (buffer {_screen = Area {xRange=(xl,xh),yRange=(yl,yh)}})
	| w == "pixels" = let (x:y:_) = readFloats ws in (buffer {_out = Area {xRange=(0,x),yRange=(0,y)}})
	--Objects
	| w == "line" = let (x1:y1:z1:x2:y2:z2:_) = readFloats ws in (buffer {_linematrix = (line x1 y1 z1 x2 y2 z2) : mls})
	| w == "box-t" = let (sx:sy:sz:rx:ry:rz:mx:my:mz:_) = readFloats ws in (buffer {_triangleMatrix = unitCube ++ mtri})
	| w == "sphere" = let (r:_) = readFloats ws in (buffer {_triangleMatrix = sphereTri r sphereDivisions ++ mtri})
	| w == "triangle" = buffer {_triangleMatrix = (fromList [[1,0,0,1],[0,1,0,1],[0,0,1,1]]) : mtri}
	--Transformations
	| w == "identity" = buffer { _edgematrix = identity 4 4}
	| w == "move" = let (x:y:z:_) = readFloats ws in buffer {_edgematrix = matrixProduct edge (move x y z)}
	| w == "scale" = let (x:y:z:_) = readFloats ws in buffer {_edgematrix = matrixProduct edge (scale x y z)}
	| w == "rotate-x" = let (deg:_) = readFloats ws in buffer {_edgematrix = matrixProduct edge (rotateX deg)}
	| w == "rotate-y" = let (deg:_) = readFloats ws in buffer {_edgematrix = matrixProduct edge (rotateY deg)}
	| w == "rotate-z" = let (deg:_) = readFloats ws in buffer {_edgematrix = matrixProduct edge (rotateZ deg)}
	| w == "transform" = buffer {_linematrix = map ((flip matrixProduct) edge) mls,_triangleMatrix = map ((flip matrixProduct) edge) mtri, _edgematrix = (identity 4 4)}
	| otherwise = buffer
	where
		(w:ws) = words l
		readInts src = map read src ::[Int]
		readFloats src = map read . map (\(a:as) -> case a of '.' -> '0':a:as; '-' -> a:'0':as; _ -> a:as) $ src ::[Float]

-- Functions
