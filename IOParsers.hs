module IOParsers
(	
	parseIO
	)
where

import System.Environment
import System.IO
import Data.IORef

import OpenGL
import Graphics.UI.GLUT.Window (postRedisplay)

import Control.Monad.Fix

import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as ML

import Strings
import Text.Printf

import Parse
import Matrix
import Matrix3D
import Objects
import Render
import PPM


cull eye = filter (backFace eye . rows)

projCull :: (Matrix m) => (Float,Float,Float) -> [m Float] -> [m Float]
projCull eye@(ex,ey,ez) = project eye . cull [ex,ey,ez]

ioParsers :: (Matrix m) => Map String ([String] -> Parser m Float -> Renderable m Float -> IO())
ioParsers = ML.fromList [
	("file",renderToFile),
	("files",renderFrameToFile),
	("render-parallel",renderParallel),
	("render-perspective-cyclops",renderCyclops),
	("render-perspective-stereo",renderStereo),
	("spinc",spinCyclops)
	]

parseIO :: Matrix m => [String] -> Parser m Float -> Renderable m Float -> Maybe (IO ())
parseIO [] _ _ = Nothing
parseIO (w:ws) par buf = ML.lookup w ioParsers >>= \f -> Just $f ws par buf 

renderToFile :: (Matrix m) => [String] -> Parser m Float -> Renderable m Float -> IO ()
renderToFile (fname:args) par buffer@(Renderable scr out col mls mtri) = do
	let 
		(r,g,b) = col
		(ex:ey:ez:_) = map readFloat args
		buf = bufToPPM out $ (render $ buffer {
			_triangleMatrix = projCull (ex,ey,ez) mtri
		} :: [(Int,Int,Color Int)])
	writeFile fname $ showPPM out maxColor $ buf
	--putStrLn . show $ render buffer
	--writeFile "test.ppm" 
	--putStrLn $ showPPM (Area (0,100) (0,100)) 255 $ 
	{-(\b -> (putStrLn . show . bufToPPM (Area (0,10) (0,10)) $ b) >> (putStrLn . show $ b)) $ render $ buffer {
		_out = Area (0,10) (0,10),
		_col = (100,100,100)
		}
	-}

renderFrameToFile :: (Matrix m) => [String] -> Parser m Float -> Renderable m Float -> IO ()
renderFrameToFile (w:a) par@(Parse3D n _ _ _) buf = renderToFile
	(printf "%s%05d.ppm" w n : a)
	par buf

renderParallel _ _ buffer@(Renderable scr out col mls mtri) = do
	display (toSize out) $ bufToPPM out $ (render $ 
		buffer {
			_triangleMatrix = filter (parallelCheck . rows) mtri
		} :: [(Int,Int,Color Int)])

renderCyclops args par buffer@(Renderable scr out col mls mtri) = do
	let 
		(ex:ey:ez:_) = map readFloat args
	putStrLn "Rendering cyclops"
	display (toSize out) . bufToPPM out $ (render $ buffer {
		_col = green, 
		_lineMatrix = project (ex,ey,ez) mls, 
		_triangleMatrix = projCull (ex,ey,ez) mtri
	} :: [(Int,Int,Color Int)])

renderStereo :: (Matrix m) => [String] -> Parser m Float -> Renderable m Float -> IO ()
renderStereo args par buffer@(Renderable scr out col mls mtri) = do
	let
		(ex1:ey1:ez1:ex2:ey2:ez2:_) = map readFloat args
		left = render $ buffer {
			_col = cyan, 
			_lineMatrix = projCull (ex1,ey1,ez1) mls, 
			_triangleMatrix = projCull (ex1,ey1,ez1) mls
		}
		right = render $ buffer {
			_col = red, 
			_lineMatrix = projCull (ex2,ey2,ez2) mls, 
			_triangleMatrix = projCull (ex2,ey2,ez2) mls
		}
		stereo = sort (left ++ right)
	display (toSize out) $ bufToPPM out $ (stereo :: [(Int,Int,Color Int)])


spinCyclops :: (Matrix m) => [String] -> Parser m Float -> Renderable m Float -> IO ()
spinCyclops args par buffer@(Renderable scr out col mls mtri) = do
	let
		(ex:ey:ez:rx:ry:rz:_) = map readFloat args
		rotate x y z = matrixProduct (rotateX x) . matrixProduct (rotateY y) $ rotateZ z
	renderBuf <- newIORef buffer
	fix $ \loop -> do
		tbuf@(Renderable _ _ tcol tmls tmtri) <- readIORef renderBuf
		writeIORef renderBuf $ tbuf {
			_col = green,
			_lineMatrix = map ((flip matrixProduct) (rotate rx ry rz)) $ tmls, 
			_triangleMatrix =  map ((flip matrixProduct) (rotate rx ry rz)) $ tmtri
		}
		display (toSize out) $ bufToPPM out $ (render $ tbuf {
			_lineMatrix = project (ex,ey,ez) $ _lineMatrix tbuf,
			_triangleMatrix = projCull (ex,ey,ez) $ _triangleMatrix tbuf	
		} :: [(Int,Int,Color Int)])
		loop

