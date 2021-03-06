module ControlParsers
(	parseControl,
	parseVarys,
	initControlParser
	)
where
import Sequence
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as ML

import Control.Applicative

import OpenGL

import Strings

import Matrix
import Matrix3D
import Objects
import Render
import PPM

import Parse
import Parsers
import IOParsers

type Controller m = [String] -> Parser m Float ->  Renderable m Float -> IO (Renderable m Float)

initControlParser :: (Matrix m) => Controller m
initControlParser = parseLoop

controlParsers :: (Matrix m) => Map String (Controller m)
controlParsers = ML.fromList [
	("stdParse", parseLoop),
	("end", \_ _ buf -> return buf) -- Just buf)
	]


parseControl :: (Matrix m) => [String] -> [String] -> Parser m Float -> Renderable m Float -> Maybe (IO (Renderable m Float)) 
parseControl [] _ _ _ = Nothing
parseControl (w:ws) ls par buf = ML.lookup w controlParsers >>= \f -> Just $ f ls par  buf

parseLoop ::(Matrix m) => [String] -> Parser m Float -> Renderable m Float -> IO (Renderable m Float) -- Maybe (Renderable m Float)
parseLoop [] _ buf = return buf
parseLoop (l:ls) par@(Parse3D fnum varys _ _) buf = do
	putStrLn $ "Line: " ++ unwords ws
	fromMaybe (parseLoop ls par buf) -- Default case
		$   (parseIO ws par buf >>= Just . (>> parseLoop ls par buf ))
		<|> (parseGeo ws par buf >>= Just . \nBuf ->  parseLoop ls par nBuf)
		<|> (parseTrans ws par >>= Just . \nPar -> parseLoop ls nPar buf)
		<|> parseControl ws ls par buf 
	where
		nVarys = varys 
		ws = fromMaybe [] . varyFold fnum varys $ words l

