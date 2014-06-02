module Operations (
	cube,
	sphere
	) where
import Matrix
import Matrix3D
import Objects hiding (sphere)
type Tform = (Double,Double,Double)


cube :: (Matrix m) => Tform -> Tform -> Tform -> [m Double]
cube s r m = (flip map) unitCube $ transform (collate [scale s, rotate r, move m])

sphere :: (Matrix m) => Double -> Double -> Tform -> Tform -> Tform -> [m Double]
sphere rad divs s r m = (flip map) (sphereTri rad (floor divs)) 
	$ transform (collate [scale s, rotate r, move m])
