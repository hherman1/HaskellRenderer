module Objects
(
	line,
	sphere,
	sphereTri
)
where
import Matrix
import Matrix3D



--Lines
line x1 y1 z1 x2 y2 z2 = fromList $ [[x1,y1,z1,1],[x2,y2,z2,1]]

--Cube
{-
  double tlf[4] = { -1, 1, 1, 1};	// top left front
  double tlb[4] = { -1, 1, -1, 1};	// top left back
  double trf[4] = {1, 1, 1, 1};	// top right front
  double trb[4] = {1, 1, -1, 1};	// top right back
  double blf[4] = { -1, -1, 1, 1};	// back left front
  double blb[4] = { -1, -1, -1, 1};	// back left back
  double brf[4] = {1, -1, 1, 1};	// back right front
  double brb[4] = {1, -1, -1, 1};	// back right back

  addtriangle(cube, tlf, trb, tlb);
  addtriangle(cube, tlf, trf, trb);
  // bottom face -- done
  addtriangle(cube, blf, brb, brf);
  addtriangle(cube, blf, blb, brb);
  // back face -- done
  addtriangle(cube, blb, trb, brb);
  addtriangle(cube, blb, tlb, trb);
  // front face -- done
  addtriangle(cube, blf, brf, trf);
  addtriangle(cube, blf, trf, tlf);
  // right face -- done
  addtriangle(cube, brf, brb, trb);
  addtriangle(cube, brf, trb, trf);
  // left face
  addtriangle(cube, blf, tlf, blb);
  addtriangle(cube, blb, tlf, tlb);
-}

unitCube :: (Matrix m) => [m Float]
unitCube = [
	  fromList [tlf, trb, tlb],
	  fromList [tlf, trf, trb],
	  -- bottom face -- done
	  fromList [blf, brb, brf],
	  fromList [blf, blb, brb],
	  -- back face -- done
	  fromList [blb, trb, brb],
	  fromList [blb, tlb, trb],
	  -- front face -- done
	  fromList [blf, brf, trf],
	  fromList [blf, trf, tlf],
	  -- right face -- done
	  fromList [brf, brb, trb],
	  fromList [brf, trb, trf],
	  -- left face
	  fromList [blf, tlf, blb],
	  fromList [blb, tlf, tlb]
	]
	where
		tlf = [-1,1,1,1]
		tlb = [-1,1,-1,1]
		trf = [1,1,1,1]
		trb = [1,1,-1,1]
		blf = [-1,-1,1,1]
		blb = [-1,-1,-1,1]
		brf = [1,-1,1,1]
		brb = [1,-1,-1,1]
--Spheres
sphere :: (Matrix m) => Float -> Int -> [m Float]
sphere r divs = connectArcs $ genRotations divs $ arc r divs
	where 
		connectArcs :: (Matrix m) => [m Float] -> [m Float]
		-- connectArcs arcs = let arc = map rows arcs in (concat $ map fromList arc) ++ (concat $ map fromList  . Matrix.transpose' $ arc) -- (concat $ map traceMatrix $ Matrix.transpose' arcs) -- zipWith (map traceMatrix) arcs (drop 1 arcs))
		connectArcs arcs = let arc = map rows arcs in (concat $ map traceMatrix arcs) ++ (concat $ zipWith (zipWith (\a b -> Matrix.fromList [a,b])) arc (drop 1 arc))
		traceMatrix m = let mr = rows m in zipWith (\a b -> Matrix.fromList $ [a,b]) mr (drop 1 mr) 

sphereTri :: (Matrix m) => Float -> Int -> [m Float]
sphereTri r divs = let arcs = genRotations divs $ arc r divs in concat $ genTriangles arcs
	where
		genTriangles :: (Matrix m) => [m a] -> [[m a]]
		genTriangles src = let arcs = map rows src in zipWith (\a b -> triangleArcs a b) arcs $ drop 1 arcs
	
		triangleArcs :: (Matrix m) => [[a]] -> [[a]] -> [m a]
		triangleArcs = ((map fromList) .) . zipTri


zipTri :: [a] -> [a] -> [[a]]
zipTri p q = zipWith (\a (b,c) -> [a,b,c]) p . zip q $ drop 1 q

genRotations :: (Matrix m, Integral a) => a -> m Float -> [m Float]
genRotations divs = take (1 + fromIntegral divs) . iterate ((flip matrixProduct) (rotateY $ 360 / fromIntegral divs))

arc :: (Integral a, Matrix m) => Float -> a -> m Float
arc r divs = fromList $ [ [r * cos (qu * 2 * pi / fromIntegral divs), r * sin (qu * 2 * pi / fromIntegral divs), 0, 1 ] | t <- [1..divs], let qu = fromIntegral t]
