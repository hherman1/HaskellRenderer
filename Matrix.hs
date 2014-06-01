module Matrix
( 	ListMatrix,
	Matrix,
	numRows,
	numCols,
	rows,
	cols,
	transpose,
	transpose',
	fromList,
	toList,
	identity,
	matrixProduct,
	dotProduct,
	format,
	horizontalMatrix
	)
where


data ListMatrix d = MatrixV Int Int [[d]] | MatrixH Int Int [[d]] deriving (Eq)

class Functor f => Matrix f  where
	numRows :: f a -> Int
	numCols :: f a -> Int

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

	cols (MatrixV _ _ dat) = dat
	cols (MatrixH _ _ dat) = transpose' dat

	rows (MatrixV _ _ dat) = transpose' dat
	rows (MatrixH _ _ dat) = dat


	transpose (MatrixH r c dat) = MatrixH c r $ transpose' dat
	transpose (MatrixV r c dat) = MatrixV c r $ transpose' dat
	
	fromList [] = MatrixH 0 0 []
	fromList dat@(d:_) = MatrixH (length dat) (length d) dat

	toList (MatrixV _ _ dat) = dat
	toList (MatrixH _ _ dat) = dat

	identity r c = MatrixH r c [[if x==y then 1 else 0 | x<- [1..c]]| y <- [1..r]]

	matrixProduct (MatrixH r a dH) (MatrixV b c dV) 
		| a == b = MatrixV r c $ matrixByMatrix dH dV
		| otherwise = error $ "The columns of matrix A:" ++ show a ++ " must equal the rows of matrix B:" ++ show b
	matrixProduct mH@(MatrixH {}) mH2@(MatrixH {}) = matrixProduct mH $ format mH2
	matrixProduct mV@(MatrixV {}) mV2@(MatrixV {}) = matrixProduct (format mV) mV2
	matrixProduct mV@(MatrixV {}) mH@(MatrixH {}) = matrixProduct (format mV) (format mH)

--instance Matrix ListMatrix
instance Functor ListMatrix where
	fmap f (MatrixV r c dat) = MatrixV r c (map (map f) dat)
	fmap f (MatrixH r c dat) = MatrixH r c (map (map f) dat)

instance (Show a) => Show (ListMatrix a) where
	show (MatrixH r c []) = "rows: " ++ show r ++ "\tcolumns: " ++ show c ++ "\n"
	show (MatrixH r c (x:xs)) = showVector x ++ show (MatrixH r c xs)
		where 
			showVector :: Show a => [a] -> String
			showVector b = "|\t" ++ foldr ((++) . (++ "\t") . show) [] b ++ "|\n"
	show m@(MatrixV {}) = show . format $ m

--class Matrix a where

format :: ListMatrix a -> ListMatrix a
format (MatrixH r c dat) = MatrixV r c $ transpose' dat
format (MatrixV r c dat) = MatrixH r c $ transpose' dat

horizontalMatrix :: ListMatrix a -> Bool
horizontalMatrix (MatrixH {}) = True
horizontalMatrix _ = False

transpose' :: [[a]] -> [[a]]
transpose' ([]:_) = []
transpose' dat = map head dat : transpose' (map tail dat)


matrixByMatrix :: Num a => [[a]] -> [[a]] -> [[a]]
matrixByMatrix _ [] = []
matrixByMatrix m1 (c:cs) = matrixMult m1 c : matrixByMatrix m1 cs

matrixMult :: Num a => [[a]] -> [a] -> [a]
matrixMult [] _ = []
matrixMult (r:rs) c = dotProduct r c : matrixMult rs c

dotProduct :: Num a => [a] -> [a] -> a
dotProduct _ [] = 0
dotProduct [] _ = 0
dotProduct (x:xs) (y:ys) = x * y + dotProduct xs ys


