module Parser (
	Val (..),
	Command (..),
	Method (..),
	Transform,
	parseContents,
	parseCommand
	) where
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language
import Text.Parsec.Token


data Val a = Literal a | Variable String
	deriving Show

type Transform a = (Val a, Val a, Val a)

data Command = 	Cube (Transform Double) (Transform Double) (Transform Double) 
		| Sphere (Val Double) (Val Double) (Transform Double) (Transform Double) (Transform Double)
		| Transformation Method (Transform Double)
		| Save String
		| Restore String
		| AddVar String (Val Double, Val Double) (Val Double, Val Double)
		| RenderParallel
		| RenderCyclops (Transform Double)
		| RenderStereo (Transform Double) (Transform Double)
		| File String
		| Files String
		| Unknown
	deriving Show

data Method = Scale | Rotate | Move deriving Show

retrieve x = x $ makeTokenParser haskellStyle


double :: Parser Double
double = do
	try $ do
		char '-'
		num <- number
		return $ -1 * num
	<|> number

number :: Parser Double
number = do
	try $ retrieve float
	<|> do
		i <- retrieve integer
		return $ fromInteger i

value :: Parser a -> Parser String -> Parser (Val a)
value par str = 	
	try $ do 
		val <- par
		return $ Literal val
	<|> do
		varName <- str
		return $ Variable varName

doubleVal :: Parser (Val Double)
doubleVal = value double (many1 $ satisfy (/= ' ')) 

comThenDoubles :: Int -> String -> Parser [Val Double]
comThenDoubles n s = do
	string s
	many space
	count n $ many space >> doubleVal

parseContents :: Parser [Command] --broken
parseContents = parseCommand `sepBy` many newline

parseCommand :: Parser Command
parseCommand = choice $ map try [
	parseCube,
	parseSphere,
	parseScale,
	parseRotate,
	parseMove,
	parseSave,
	parseRestore,
	parseVar,
	parseRenderParallel,
	parseRenderCyclops,
	parseRenderStereo,
	parseFile,
	parseFiles,
	(manyTill anyToken newline) >> return Unknown
	]

parseCube,parseSphere,
	parseScale,parseRotate,parseMove,
	parseSave,parseRestore,parseVar,
	parseRenderParallel,parseRenderCyclops,parseRenderStereo,
	parseFile,parseFiles :: Parser Command
parseCube = do
	(sx:sy:sz:rx:ry:rz:mx:my:mz:_) <- comThenDoubles 9 "cube"
	return $ Cube (sx,sy,sz) (rx,ry,rz) (mx,my,mz)

parseSphere = do
	(r:d:sx:sy:sz:rx:ry:rz:mx:my:mz:_) <- comThenDoubles 11 "sphere"
	return $ Sphere r d (sx,sy,sz) (rx,ry,rz) (mx,my,mz)

parseScale = do
	(x:y:z:_) <- comThenDoubles 3 "scale"
	return $ Transformation Scale (x,y,z)

parseRotate = do
	(x:y:z:_) <- comThenDoubles 3 "rotate"
	return $ Transformation Rotate (x,y,z)

parseMove = do
	(x:y:z:_) <- comThenDoubles 3 "move"
	return $ Transformation Move (x,y,z)

parseSave = do
	string "save"
	many space
	sav <- manyTill anyToken newline
	return $ Save sav

parseRestore = do 
	string "restore"
	many space
	res <- manyTill anyToken newline
	return $ Restore res

parseVar = do
	string "vary"
	many space
	varName <- many1 $ satisfy (/= ' ')
	many space
	(vs:ve:fs:fe:_) <- count 4 $ many space >> doubleVal 
	return $ AddVar varName (vs,ve) (fs,ve)

parseRenderParallel = do
	string "render-parallel"
	return RenderParallel

parseRenderCyclops = do
	(x:y:z:_) <- comThenDoubles 3 "render-perspective-cyclops"
	return $ RenderCyclops (x,y,z)

parseRenderStereo = do
	(lx:ly:lz:rx:ry:rz:_) <- comThenDoubles 6 "render-perspective-stereo"
	return $ RenderStereo (lx,ly,lz) (rx,ry,rz)

parseFile = do
	string "file"
	many space
	fName <- many $ satisfy (/= ' ')
	return $ File fName

parseFiles = do
	string "files"
	many space
	fName <- many $ satisfy (/= ' ')
	return $ Files fName
