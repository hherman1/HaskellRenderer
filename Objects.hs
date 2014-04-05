module Objects
(
	sphere,
	sphereTri
)
where
import Matrix
import Matrix3D

sphere :: (Matrix m) => Float -> Int -> [m Float]
sphere r divs = connectArcs $ genRotations divs $ arc r divs
	where 
		connectArcs :: (Matrix m) => [m Float] -> [m Float]
		-- connectArcs arcs = let arc = map rows arcs in (concat $ map (fromList) $ Matrix.transpose' arc) ++ (concat $ map fromList . map Matrix.transpose' . Matrix.transpose' $ arc) -- (concat $ map traceMatrix $ Matrix.transpose' arcs) -- zipWith (map traceMatrix) arcs (drop 1 arcs))
		connectArcs arcs = let arc = map rows arcs in (concat $ map traceMatrix arcs) ++ (concat $ zipWith (zipWith (\a b -> Matrix.fromList . transpose' $ [a,b])) arc (drop 1 arc))
		traceMatrix m = let mr = rows m in zipWith (\a b -> Matrix.fromList . Matrix.transpose' $ [a,b]) mr (drop 1 mr) 

sphereTri :: (Matrix m) => Float -> Int -> [m Float]
sphereTri r divs = let arcs = genRotations divs $ arc r divs in concat $ genTriangles arcs

--Error from here
genTriangles :: (Matrix m) => [m a] -> [[m a]]
genTriangles src = let arcs = map rows src in zipWith (\a b -> triangleArcs a b ++ triangleArcs b a) arcs $ drop 1 arcs

triangleArcs :: (Matrix m) => [[a]] -> [[a]] -> [m a]
triangleArcs = (map (fromList . transpose') .) . zipTri

zipTri :: [a] -> [a] -> [[a]]
zipTri p q = zipWith (\a (b,c) -> [a,b,c]) p . zip q $ drop 1 q

genRotations :: (Matrix m, Integral a) => a -> m Float -> [m Float]
genRotations divs = take (1 + fromIntegral divs) . iterate ((flip matrixProduct) (rotateY $ 360 / fromIntegral divs) . transpose)

arc :: (Integral a, Matrix m) => Float -> a -> m Float
arc r divs = fromList $ transpose' [ [r * cos (qu * 2 * pi / fromIntegral divs), r * sin (qu * 2 * pi / fromIntegral divs), 0, 1 ] | t <- [1..divs], let qu = fromIntegral t]
