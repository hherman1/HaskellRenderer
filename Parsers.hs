module Parsers 
(
	Sequence(..),
	parsers
	)
where
iport Data.Map (Map)
import qualified Data.Map as ML

import Strings

import Matrix
import Matrix3D
import Objects
import Render
import PPM
{-
	parseScreen,
	parsePixels,
	parseLine,
	parseBoxT,
	parseSphere,
	parseTriangle,
	parseIdentity,
	parseMove,
	parseScale,
	parseRotX,
	parseRotY,
	parseRotZ,
	parseTransform
)
-}

parsers :: (Matrix m) => Map String ([String] -> Renderable m Float -> Renderable m Float)
parsers = ML.union formatParsers $ ML.union graphicalParsers $ transformationParsers
	where	
	formatParsers = ML.fromList [
		("screen",parseScreen),
		("pixels",parsePixels)
		]
	graphicalParsers = ML.fromList [
		("line",parseLine),
		("box-t",parseBoxT),
		("sphere",parseSphere)
		]
	transformationParsers = ML.fromList [
		("identity",parseIdentity),
		("move",parseMove),
		("scale",parseScale),
		("rotate-x",parseRotX),
		("rotate-y",parseRotY),
		("rotate-z",parseRotZ),
		("transform",parseTransform)
		]


parseScreen args buf = let (xl:yl:xh:yh:_) = map readFloat args in buf {
	_screen = Area {xRange=(xl,xh),yRange=(yl,yh)}
	}

parsePixels args buf = let (x:y:_) = map readFloat args in buf {
	_out = Area {xRange = (0,x),yRange = (0,y)}
	}

parseLine args buf@(Renderable _ _ _ _ mls _) = let (x1:y1:z1:x2:y2:z2:_) = map readFloat args in buf {
	_lineMatrix = line x1 y1 z1 x2 y2 z2 : mls
}

parseBoxT args buf@(Renderable _ _ _ _ _ mtri) = let (sx:sy:sz:rx:ry:rz:mx:my:mz:_) = map readFloat args in buf {
	_triangleMatrix = map (transform (collate [scale sx sy sz, rotate rx ry rz,move mx my mz])) unitCube ++ mtri
}

parseSphere args buf@(Renderable _ _ _ _ _ mtri) = let (r:divs:x:y:z:_) = map readFloat args in buf {
	_triangleMatrix = map (transform (move x y z)) $ sphereTri r (floor divs) ++ mtri
}

parseIdentity _ buf = buf {
	_edgematrix = identity 4 4
}

parseMove args buf@(Renderable _ _ _ edge _ _) = let (x:y:z:_) = map readFloat args in buf {
	_edgematrix = matrixProduct edge $ move x y z
}

parseScale args buf@(Renderable _ _ _ edge _ _) = let (x:y:z:_) = map readFloat args in buf {
	_edgematrix = matrixProduct edge $ scale x y z
}
parseRotate args buf@(Renderable _ _ _ edge _ _) = let (x:y:z:_) = map readFloat args in buf {
	_edgematrix = matrixProduct edge $ rotate x y z
}

parseRotX (w:_) buf@(Renderable _ _ _ edge _ _) = let x = readFloat w in buf {
	_edgematrix = matrixProduct edge $ rotateX x
}
parseRotY (w:_) buf@(Renderable _ _ _ edge _ _) = let y = readFloat w in buf {
	_edgematrix = matrixProduct edge $ rotateY y
}
parseRotZ (w:_) buf@(Renderable _ _ _ edge _ _) = let z = readFloat w in buf {
	_edgematrix = matrixProduct edge $ rotateZ z
}

parseTransform _ buf@(Renderable _ _ _ edge mls mtri) = buf {
	_edgematrix = identity 4 4,
	_triangleMatrix = map (transform edge) mtri,
	_lineMatrix = map (transform edge) mls
}

{-
parse :: Matrix m => String -> Renderable m Float -> Renderable m Float
parse l buffer@(Renderable scr out col edge mls mtri)
	--Config
	| w == "screen" = let (xl:yl:xh:yh:_) = readFloats ws in (buffer {_screen = Area {xRange=(xl,xh),yRange=(yl,yh)}})
	| w == "pixels" = let (x:y:_) = readFloats ws in (buffer {_out = Area {xRange=(0,x),yRange=(0,y)}})
	--Objects
	| w == "line" = let (x1:y1:z1:x2:y2:z2:_) = readFloats ws in (buffer {_lineMatrix = (line x1 y1 z1 x2 y2 z2) : mls})
	| w == "box-t" = let (sx:sy:sz:rx:ry:rz:mx:my:mz:_) = readFloats ws in (buffer {_triangleMatrix = unitCube ++ mtri})
	| w == "sphere" = let (r:divs:x:y:z:_) = readFloats ws in (buffer {_triangleMatrix = map ((flip matrixProduct) (move x y z)) $ sphereTri r (floor divs) ++ mtri})
	| w == "triangle" = buffer {_triangleMatrix = (fromList [[1,0,0,1],[0,1,0,1],[0,0,1,1]]) : mtri}
	--Transformations
	| w == "identity" = buffer { _edgematrix = identity 4 4}
	| w == "move" = let (x:y:z:_) = readFloats ws in buffer {_edgematrix = matrixProduct edge (move x y z)}
	| w == "scale" = let (x:y:z:_) = readFloats ws in buffer {_edgematrix = matrixProduct edge (scale x y z)}
	| w == "rotate-x" = let (deg:_) = readFloats ws in buffer {_edgematrix = matrixProduct edge (rotateX deg)}
	| w == "rotate-y" = let (deg:_) = readFloats ws in buffer {_edgematrix = matrixProduct edge (rotateY deg)}
	| w == "rotate-z" = let (deg:_) = readFloats ws in buffer {_edgematrix = matrixProduct edge (rotateZ deg)}
	| w == "transform" = buffer {_lineMatrix = map ((flip matrixProduct) edge) mls,_triangleMatrix = map ((flip matrixProduct) edge) mtri, _edgematrix = (identity 4 4)}
	| w == "vary" = buffer
	| otherwise = buffer
	where
		(w:ws) = words l
		readInts src = map read src ::[Int]
-}
