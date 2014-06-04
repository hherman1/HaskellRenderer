module Operations (
	cube,
	sphere,
	renderCyclops,
	writeFile,
	writeFrame
	) where
import Matrix
import Matrix3D
import Objects hiding (sphere)
import Render
import OpenGL
import PPM

import Text.Printf

import System.IO

type Tform = (Double,Double,Double)

cull eye = filter (isBackface eye . rows)
projCull eye@(ex,ey,ez) = project eye . cull [ex,ey,ez]

projectGeometry :: (Matrix m) => Tform -> Renderable m Double -> Renderable m Double
projectGeometry eye buffer@
	(Renderable {_lineMatrix = mls,
	_triangleMatrix = mtri}) 
	= buffer {
		_col = green, 
		_lineMatrix = project eye mls,
		_triangleMatrix = projCull eye mtri
	}

cube :: (Matrix m) => Tform -> Tform -> Tform -> [m Double]
cube s r m = (flip map) unitCube $ transform (collate [scale s, rotate r, move m])

sphere :: (Matrix m) => Double -> Double -> Tform -> Tform -> Tform -> [m Double]
sphere rad divs s r m = (flip map) (sphereTri rad (floor divs)) 
	$ transform (collate [scale s, rotate r, move m])

renderCyclops :: (Matrix m) => Resolution Int -> Tform -> Renderable m Double -> [Color Int]
renderCyclops out eye buffer@(Renderable scr col mls mtri) = 
	bufToPPM out $ 
		(render out $ projectGeometry eye buffer :: [(Int,Int,Color Int)])

writePPM :: String -> Resolution Int -> [Color Int] -> IO ()
writePPM s out buffer = do
	writeFile s $ showPPM out maxColor buffer

writeFrame :: String -> Int -> Resolution Int -> [Color Int] -> IO ()
writeFrame s fnum out buffer = do 
	writePPM (printf "%s%05d.ppm" s fnum) out buffer
