module Parsers 
(
	parseGeo,
	parseTrans,
	)
where
import Data.Map (Map)
import qualified Data.Map as ML
import Data.Maybe (fromMaybe)

import Strings

import Matrix
import Matrix3D
import Objects
import Render
import Parse
import PPM
import Sequence

parsers :: (Matrix m) => Map String ([String] -> Parser m Float -> Renderable m Float -> Renderable m Float)
parsers = ML.union formatParsers $ geometricParsers 
	where	
	formatParsers = ML.fromList [
		("screen",parseScreen),
		("pixels",parsePixels)
		]
	geometricParsers = ML.fromList [
		("transform",parseTransform),
		("line",parseLine),
		("box-t",parseBoxT),
		("sphere",parseSphere)
		]

transformationParsers :: Matrix m => Map String ([String] -> Parser m Float -> Parser m Float)
transformationParsers = ML.union cstParsers $ multiTransParsers
	where
	cstParsers =  ML.fromList [
		("identity",parseIdentity),
		("move",parseMove),
		("scale",parseScale),
		("rotate",parseRotate),
		("rotate-x",parseRotX),
		("rotate-y",parseRotY),
		("rotate-z",parseRotZ)
		]
	multiTransParsers = ML.fromList [
		("save",parseSave),
		("restore",parseRestore)
		]

parseGeo :: Matrix m => [String] -> Parser m Float -> Renderable m Float -> Maybe (Renderable m Float)
parseGeo [] _ _ = Nothing
parseGeo (w:ws) par buf = ML.lookup w parsers >>= \f -> Just $f ws par buf

parseTrans :: Matrix m => [String] -> Parser m Float -> Maybe (Parser m Float)
parseTrans [] _ = Nothing
parseTrans (w:ws) par = ML.lookup w transformationParsers >>= \f -> Just $f ws par

parseScreen args par buf = let (xl:yl:xh:yh:_) = map readFloat args in buf {
	_screen = Area {xRange=(xl,xh),yRange=(yl,yh)}
	}

parsePixels args par buf = let (x:y:_) = map read args in buf {
	_out = Area {xRange = (0,x),yRange = (0,y)}
	}

parseLine args par buf@(Renderable _ _ _ mls _) = let (x1:y1:z1:x2:y2:z2:_) = map readFloat args in buf {
	_lineMatrix = line x1 y1 z1 x2 y2 z2 : mls
}

parseBoxT args par@(Parse3D _ _ edge _) buf@(Renderable _ _ _ _ mtri) = 
	let (sx:sy:sz:rx:ry:rz:mx:my:mz:_) = map readFloat args in buf {
	_triangleMatrix = 
		map 	(transform (collate [
					scale sx sy sz,
					rotate rx ry rz,
					move mx my mz,
					edge
					])) 
			unitCube 
		++ mtri
}

parseSphere args par@(Parse3D _ _ edge _) buf@(Renderable _ _ _ _ mtri) = 
	let (r:divs:sx:sy:sz:rx:ry:rz:x:y:z:_) = map readFloat args in buf {
	_triangleMatrix =
		map 	(transform (collate [
					scale sx sy sz,
					rotate rx ry rz,
					move x y z,
					edge])) 
			(sphereTri r (floor divs))
		++ mtri
}

parseTransform args par@(Parse3D _ _ edge _) buf@(Renderable _ _ _ mls mtri) = buf {
	_triangleMatrix = map (transform edge) mtri,
	_lineMatrix = map (transform edge) mls
}

parseIdentity _ par = par {
	_currentTransform = identity 4 4
}

parseMove args par@(Parse3D _ _ edge _) = let (x:y:z:_) = map readFloat args in par {
	_currentTransform = transform edge $ move x y z
}

parseScale args par@(Parse3D _ _ edge _) = let (x:y:z:_) = map readFloat args in par {
	_currentTransform = transform edge $ scale x y z
}
parseRotate args par@(Parse3D _ _ edge _) = let (x:y:z:_) = map readFloat args in par {
	_currentTransform = transform edge $ rotate x y z
}
parseRotX (w:_) par@(Parse3D _ _ edge _) = let x = readFloat w in par {
	_currentTransform = transform edge $ rotateX x
}
parseRotY (w:_) par@(Parse3D _ _ edge _) = let y = readFloat w in par {
	_currentTransform = transform edge $ rotateY y
}
parseRotZ (w:_) par@(Parse3D _ _ edge _) = let z = readFloat w in par {
	_currentTransform = transform edge $ rotateZ z
}

parseSave (w:_) par@(Parse3D _ _ cst saves) = par {
	_transformations = ML.insert w cst saves
}

parseRestore (w:_) par@(Parse3D _ _ cst saves) = par {
	_currentTransform = fromMaybe cst $ ML.lookup w saves
}
