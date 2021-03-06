module Objects
(
	line,
	sphere,
	sphereTri,
	unitCube
)
where
import Matrix
import Matrix3D


--Lines
line x1 y1 z1 x2 y2 z2 = fromList $ [[x1,y1,z1,1],[x2,y2,z2,1]]

--Cube

unitCube :: (Matrix m) => [m Double]
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
		tlf = [-0.5,0.5,0.5,1]
		tlb = [-0.5,0.5,-0.5,1]
		trf = [0.5,0.5,0.5,1]
		trb = [0.5,0.5,-0.5,1]
		blf = [-0.5,-0.5,0.5,1]
		blb = [-0.5,-0.5,-0.5,1]
		brf = [0.5,-0.5,0.5,1]
		brb = [0.5,-0.5,-0.5,1]
--Spheres 

sphere :: (Matrix m) => Double -> Int -> [m Double]
sphere r divs = connectArcs $ genRotations divs $ arc r divs
	where 
		connectArcs :: (Matrix m) => [m Double] -> [m Double]
		-- connectArcs arcs = let arc = map rows arcs in (concat $ map fromList arc) ++ (concat $ map fromList  . Matrix.transpose' $ arc) -- (concat $ map traceMatrix $ Matrix.transpose' arcs) -- zipWith (map traceMatrix) arcs (drop 1 arcs))
		connectArcs arcs = let arc = map rows arcs in (concat $ map traceMatrix arcs) ++ (concat $ zipWith (zipWith (\a b -> Matrix.fromList [a,b])) arc (drop 1 arc))
		traceMatrix m = let mr = rows m in zipWith (\a b -> Matrix.fromList $ [a,b]) mr (drop 1 mr) 
 
-- test
test = 4
-- no testing
sphereTri :: (Matrix m) => Double -> Int -> [m Double]
sphereTri r divs = let arcs = genRotations divs $ arc r divs in mZipTri arcs
	where
		mZipTri :: (Matrix m) => [m a] -> [m a]
		mZipTri arc = let arcs = map rows arc in concat . map (map fromList) 
			$ zipWith (zipTri) arcs (drop 1 arcs)
			++ zipWith ((map reverse .) . zipTri) (drop 1 $ map (drop 1) arcs) arcs

-- dasfasdf asdf asdf asdf 
zipTri :: [a] -> [a] -> [[a]]
zipTri p q = zipWith (\a (b,c) -> [a,b,c]) p . zip q $ drop 1 q

genRotations :: (Matrix m, Integral a) => a -> m Double -> [m Double]
genRotations divs = take (1 + fromIntegral divs) . iterate ((flip matrixProduct) (rotateY $ 360 / fromIntegral divs))

arc :: (Integral a, Matrix m) => Double -> a -> m Double
arc r divs = fromList $ [ [r * sin (qu *  pi / fromIntegral divs), r * cos (qu *  pi / fromIntegral divs), 0, 1 ] | t <- [1..divs], let qu = fromIntegral t]
