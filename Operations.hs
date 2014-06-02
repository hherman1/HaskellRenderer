module Operations (
	cube,
	sphere
	) where
import Matrix
import Matrix3D
import Objects hiding (sphere)
import Render
import OpenGL
import PPM
type Tform = (Double,Double,Double)

cull eye = filter (isBackface eye . rows)
projCull eye@(ex,ey,ez) = project eye . cull [ex,ey,ez]


cube :: (Matrix m) => Tform -> Tform -> Tform -> [m Double]
cube s r m = (flip map) unitCube $ transform (collate [scale s, rotate r, move m])

sphere :: (Matrix m) => Double -> Double -> Tform -> Tform -> Tform -> [m Double]
sphere rad divs s r m = (flip map) (sphereTri rad (floor divs)) 
	$ transform (collate [scale s, rotate r, move m])

renderCyclops :: (Matrix m) => Tform -> Renderable m Double -> IO ()
renderCyclops eye buffer@(Renderable scr out col mls mtri) = do
	putStrLn "Rendering cyclops"
	display (toSize out) . bufToPPM out $ (render $ buffer {
		_col = green, 
		_lineMatrix = project eye mls,
		_triangleMatrix = projCull eye mtri
	} :: [(Int,Int,Color Int)])
