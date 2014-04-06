module Matrix3D 
(	move,
	scale,
	rotateX,
	rotateY,
	rotateZ,
	toRad
	)
where
import Matrix
move :: (Matrix m) => Float -> Float -> Float -> m Float
move a b c = fromList $ [[1.0,0,0,0],[0,1.0,0,0],[0,0,1.0,0],[a,b,c,1.0]]

scale :: (Matrix m) => Float -> Float -> Float -> m Float
scale x y z = fromList $ [[x,0,0,0],[0,y,0,0],[0,0,z,0],[1,1,1,1]]

rotateX :: (Matrix m) => Float -> m Float
rotateX deg = let a = toRad deg in fromList $ [[1.0,0,0,0],[0,cos a, (-sin a),0],[0,(sin a),cos a, 0],[0,0,0,1.0]]

rotateY :: (Matrix m) => Float -> m Float
rotateY deg = let a = toRad deg in fromList $ [[cos a,0,(-sin a),0],[0,1,0,0],[(sin a),0,cos a,0],[0,0,0,1]]

rotateZ :: (Matrix m) => Float -> m Float
rotateZ deg = let a = toRad deg in fromList $ [[cos a,(-sin a),0,0],[(sin a),cos a,0,0],[0,0,1,0],[0,0,0,1]]

toRad :: Float -> Float
toRad t = t * pi / 180