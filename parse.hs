module Parse 
(	ScreenBuffer,
	Vertices,
	initParseIO,
	rotateX,
	rotateY,
	rotateZ,
	move,
	sphere,
	arc,
	line,
	perspective
	)
where 

import System.Environment
import System.IO
import Data.IORef
import Data.List (sort)

import Graphics.UI.GLUT.Window (postRedisplay)


import Matrix
import Render
import PPM

-- Input, Screen to size from, Output to size to, transformation matrix, line matrix
maxColor = 255
sphereDivisions = 20

cyan = (0,255,255)
red = (255,0,0)
green = (0,150,0)

type Vertices d = [(Int,Int,d)]
type ScreenBuffer = IORef (Vertices (Float,Float,Float))

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
		writeIORef sbuf $ optimizeGrid $ renderLineMatrix buffer
		parseIO ls sbuf buffer
	| w == "render-perspective-cyclops" = do
		let 
			(ex:ey:ez:_) = readFloats ws
		writeIORef sbuf $ optimizeGrid $ renderLineMatrix $ buffer {_col = green, _linematrix = project (ex,ey,ez) mls}
		parseIO ls sbuf buffer
	| w == "render-perspective-stereo" = do
		let
			(ex1:ey1:ez1:ex2:ey2:ez2:_) = readFloats ws
			left = renderLineMatrix $ buffer {_col = cyan, _linematrix = project (ex1,ey1,ez1) mls}
			right = renderLineMatrix $ buffer {_col = red, _linematrix = project (ex2,ey2,ez2) mls}
			stereo = sort (left ++ right)
		writeIORef sbuf $ optimizeGrid stereo
		parseIO ls sbuf buffer
	| otherwise = parseIO ls sbuf $ parse l buffer
	where
		(w:ws) = words l
		readInts src = map read src ::[Int]
		readFloats src = map read . map (\(a:as) -> case a of '.' -> '0':a:as; '-' -> a:'0':as; _ -> a:as) $ src ::[Float]

--parseIO :: (Show (m Float), Matrix m, Integral a, Show a) => [String] -> Screen Float -> Output Float-> m Float -> [m Float] -> (a,a,a)-> IO [(a,a,(a,a,a))]
parse :: Matrix m => String -> Renderable m Float -> Renderable m Float
parse l buffer@(Renderable scr out col edge mls mtri)
	| w == "screen" = let (xl:yl:xh:yh:_) = readFloats ws in (buffer {_screen = Area {xRange=(xl,xh),yRange=(yl,yh)}})
	| w == "pixels" = let (x:y:_) = readFloats ws in (buffer {_out = Area {xRange=(0,x),yRange=(0,y)}})
	| w == "identity" = buffer { _edgematrix = (transpose $ identity 4 4)}
	| w == "line" = let (x1:y1:z1:x2:y2:z2:_) = readFloats ws in (buffer {_linematrix = (line x1 y1 z1 x2 y2 z2) : mls})
	| w == "sphere" = let (r:_) = readFloats ws in (buffer {_linematrix = sphere r sphereDivisions ++ mls})
	| w == "move" = let (x:y:z:_) = readFloats ws in buffer {_edgematrix = matrixProduct (transpose edge) (move x y z)}
	| w == "scale" = let (x:y:z:_) = readFloats ws in buffer {_edgematrix = matrixProduct (transpose edge) (scale x y z)}
	| w == "rotate-x" = let (deg:_) = readFloats ws in buffer {_edgematrix = matrixProduct (transpose edge) (rotateX deg)}
	| w == "rotate-y" = let (deg:_) = readFloats ws in buffer {_edgematrix = matrixProduct (transpose edge) (rotateY deg)}
	| w == "rotate-z" = let (deg:_) = readFloats ws in buffer {_edgematrix = matrixProduct (transpose edge) (rotateZ deg)}
	| w == "transform" = buffer {_linematrix = map ((flip matrixProduct) edge . transpose ) mls,_edgematrix = (identity 4 4)}
	| otherwise = buffer
	where
		(w:ws) = words l
		readInts src = map read src ::[Int]
		readFloats src = map read . map (\(a:as) -> case a of '.' -> '0':a:as; '-' -> a:'0':as; _ -> a:as) $ src ::[Float]

-- Functions
project :: Matrix m => (Float,Float,Float) -> [m Float] -> [m Float]
project (ex,ey,ez) m = map (Matrix.fromList . transpose' . (map ((\(x,y,z) -> [x,y,z,1]) . (\(x:y:z:_)->Parse.perspective (ex,ey,ez) (x,y,z))) .  Matrix.rows)) m

perspective (ex,ey,ez) (px,py,pz) = (ex - (ez * (px-ex)/(pz-ez)), ey - (ez *(py-ey)/(pz-ez)), 0)

line x1 y1 z1 x2 y2 z2 = fromList $ [[x1,x2],[y1,y2],[z1,z2],[1,1]]

sphere :: (Matrix m) => Float -> Int -> [m Float]
sphere r divs = connectArcs $ genRotations divs $ arc r divs
	where 
		genRotations divs = take (1 + divs) . iterate ((flip matrixProduct) (rotateY $ 360 / fromIntegral divs) . transpose)
		connectArcs :: (Matrix m) => [m Float] -> [m Float]
		--connectArcs arcs = let arc = map rows arcs in (concat $ map (fromList) $ Matrix.transpose' arc) ++ (concat $ map fromList . map Matrix.transpose' . Matrix.transpose' $ arc) -- (concat $ map traceMatrix $ Matrix.transpose' arcs) -- zipWith (map traceMatrix) arcs (drop 1 arcs))
		connectArcs arcs = let arc = map rows arcs in (concat $ map traceMatrix arcs) ++ (concat $ zipWith (zipWith (\a b -> Matrix.fromList . transpose' $ [a,b])) arc (drop 1 arc))
		traceMatrix m = let mr = rows m in zipWith (\a b -> Matrix.fromList . Matrix.transpose' $ [a,b]) mr (drop 1 mr) 

arc r divs = fromList $ transpose' [ [r * cos (qu * 2 * pi / fromIntegral divs), r * sin (qu * 2 * pi / fromIntegral divs), 0, 1 ] | t <- [1..divs], let qu = fromIntegral t]

move :: (Matrix m) => Float -> Float -> Float -> m Float
move a b c = fromList $ [[1.0,0,0,a],[0,1.0,0,b],[0,0,1.0,c],[0,0,0,1.0]]

scale :: (Matrix m) => Float -> Float -> Float -> m Float
scale x y z = fromList $ [[x,0,0,1],[0,y,0,1],[0,0,z,1],[0,0,0,1]]

rotateX :: (Matrix m) => Float -> m Float
rotateX deg = let a = toRad deg in fromList $ [[1.0,0,0,0],[0,cos a, (sin a),0],[0,(-sin a),cos a, 0],[0,0,0,1.0]]

rotateY :: (Matrix m) => Float -> m Float
rotateY deg = let a = toRad deg in fromList $ [[cos a,0,(sin a),0],[0,1,0,0],[(-sin a),0,cos a,0],[0,0,0,1]]

rotateZ :: (Matrix m) => Float -> m Float
rotateZ deg = let a = toRad deg in fromList $ [[cos a,(sin a),0,0],[(-sin a),cos a,0,0],[0,0,1,0],[0,0,0,1]]

toRad :: Float -> Float
toRad t = t * pi / 180
