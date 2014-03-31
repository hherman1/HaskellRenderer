module Matrix
( 	ListMatrix,
	Matrix,
	numRows,
	numCols,
	row,
	rows,
	col,
	cols,
	transpose,
	transpose',
	fromList,
	toList,
	identity,
	matrixProduct,
	dotProduct,
	horizontalMatrix
	)
where


data ListMatrix d = MatrixV Int Int [[d]] | MatrixH Int Int [[d]] deriving (Eq)

class Functor f => Matrix f  where
	numRows :: f a -> Int
	numCols :: f a -> Int
	row :: Int -> f a -> [a]
	col :: Int -> f a -> [a]

	rows :: f a -> [[a]]
	cols :: f a -> [[a]]

	transpose :: f a -> f a
	fromList :: [[a]] -> f a
	toList :: f a -> [[a]]
	identity :: Num a => Int -> Int -> f a
	matrixProduct :: (Num b) => f b -> f b -> f b

instance Matrix ListMatrix where
	numRows (MatrixV r _ _) = r
	numRows (MatrixH r _ _) = r
	numCols (MatrixV _ c _) = c
	numCols (MatrixH _ c _) = c

	row n (MatrixH _ _ dat) = dat !! n
	col n (MatrixV _ _ dat) = dat !! n

	cols (MatrixV _ _ dat) = dat
	cols (MatrixH _ _ dat) = transpose' dat

	rows (MatrixV _ _ dat) = transpose' dat
	rows (MatrixH _ _ dat) = dat

	transpose m@(MatrixH r c dat) = MatrixV c r $ transpose' dat
	transpose m@(MatrixV r c dat) = MatrixH c r $ transpose' dat

	fromList dat@(d:_) = MatrixV (length d) (length dat) dat

	toList (MatrixV _ _ dat) = dat
	toList (MatrixH _ _ dat) = dat

	identity r c = MatrixH r c [[if x==y then 1 else 0 | x<- [1..c]]| y <- [1..r]]

	matrixProduct mH@(MatrixH r _ dH) mV@(MatrixV _ c dV) = MatrixV r c $ matrixByMatrix dH dV
	matrixProduct mH@(MatrixH _ _ _) mH2@(MatrixH _ _ _) = matrixProduct mH $ transpose mH2
	matrixProduct mV mO = error $ "Uh oh, cant multiply these two. A : " ++ show (horizontalMatrix mV) ++ " B : " ++ show (horizontalMatrix mO) -- ++ "\nA : \n" ++ show mV ++ "\nB: \n" ++ show mO

--instance Matrix ListMatrix
instance Functor ListMatrix where
	fmap f (MatrixV r c dat) = MatrixV r c (map (map f) dat)
	fmap f (MatrixH r c dat) = MatrixH r c (map (map f) dat)

instance (Show a) => Show (ListMatrix a) where
	show m@(MatrixH r c []) = ""
	show m@(MatrixH r c (x:xs)) = showVector x ++ show (MatrixH r c xs)
		where 
			showVector :: Show a => [a] -> String
			showVector b = "|\t" ++ foldr ((++) . (++ "\t") . show) [] b ++ "|\n"
	show m@(MatrixV _ _ _) = show . transpose $ m

--class Matrix a where

horizontalMatrix :: ListMatrix a -> Bool
horizontalMatrix (MatrixH _ _ _) = True
horizontalMatrix _ = False

transpose' :: [[a]] -> [[a]]
transpose' ([]:_) = []
transpose' dat = map head dat : transpose' (map tail dat)


matrixByMatrix :: Num a => [[a]] -> [[a]] -> [[a]]
matrixByMatrix _ [] = []
matrixByMatrix m1 m2@(c:cs) = matrixMult m1 c : matrixByMatrix m1 cs

matrixMult :: Num a => [[a]] -> [a] -> [a]
matrixMult [] _ = []
matrixMult m1@(r:rs) c = (dotProduct r c) : matrixMult rs c

dotProduct :: Num a => [a] -> [a] -> a
dotProduct [] [] = 0
dotProduct (x:xs) (y:ys) = x * y + (dotProduct xs ys)


