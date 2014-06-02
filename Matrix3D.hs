module Matrix3D 
(	transform,
	collate,
	move,
	scale,
	rotate,
	rotateX,
	rotateY,
	rotateZ,
	toRad,
	crossProduct,
	normal,
	isBackface
	)
where
import Matrix

transform :: (Matrix m, Num a) => m a -> m a -> m a
transform = flip matrixProduct

collate :: (Matrix m, Num a) => [m a] -> m a
collate = foldr1 matrixProduct

move :: (Matrix m) => (Double,Double,Double) -> m Double
move (a,b,c) = fromList $ [[1.0,0,0,0],[0,1.0,0,0],[0,0,1.0,0],[a,b,c,1.0]]

scale :: (Matrix m) => (Double,Double,Double) -> m Double
scale (x,y,z) = fromList $ [[x,0,0,0],[0,y,0,0],[0,0,z,0],[0,0,0,1]]

rotate :: (Matrix m) => (Double,Double,Double) -> m Double
rotate (x,y,z) = matrixProduct (rotateX x) $ matrixProduct (rotateY y) $ rotateZ z

rotateX :: (Matrix m) => Double -> m Double
rotateX deg = let a = toRad deg in fromList $ [[1.0,0,0,0],[0,cos a, (-sin a),0],[0,(sin a),cos a, 0],[0,0,0,1.0]]

rotateY :: (Matrix m) => Double -> m Double
rotateY deg = let a = toRad deg in fromList $ [[cos a,0,(sin a),0],[0,1,0,0],[(-sin a),0,cos a,0],[0,0,0,1]]

rotateZ :: (Matrix m) => Double -> m Double
rotateZ deg = let a = toRad deg in fromList $ [[cos a,(-sin a),0,0],[(sin a),cos a,0,0],[0,0,1,0],[0,0,0,1]]

toRad :: Double -> Double
toRad t = t * pi / 180

crossProduct :: Num a => [a] -> [a] -> [a]
crossProduct (u1:u2:u3:_) (v1:v2:v3:_) = u2 * v3 - u3 * v2 : u3 * v1 - u1 * v3 : u1 * v2 - u2 * v1 : []

normal :: (Num a) => [a] -> [a] -> [a] -> [a]
normal p1 p2 p3 = crossProduct (zipWith (-) p2 p1) (zipWith (-) p3 p1)
normal _ _ _ = error "Normals are undefined for non vector triangles"

isBackface :: (Num a,Ord a) => [a] -> [[a]] -> Bool
isBackface eye (p1:p2:p3:_) = let x = zipWith (-) eye p1 `dotProduct` normal p1 p2 p3 in x > 0

{-
backFace :: (Eq a, Ord a, Num a) => [a] -> [[a]] -> Bool
backFace cam triangle = (>0) $ (take 3 $ zipWith (-) cam (head triangle)) `dotProduct` norm triangle
	where 
		norm tri = foldl1 crossProduct . zipWith (zipWith (-)) tri $ drop 1 tri 
-}
