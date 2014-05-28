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

type Controller m = [String] -> Parse m Float -> ScreenBuffer -> Renderable m Float -> IO (Renderable m Float)

initControlParser :: (Matrix m) => Controller m
initControlParser = parseLoop

controlParsers :: (Matrix m) => Map String (Controller m)
controlParsers = ML.fromList [
	("stdParse", parseLoop),
	("end", \_ _ _ _ buf -> return buf) -- Just buf)
	]


parseControl :: (Matrix m) => [String] -> [String] -> Parse m Float -> ScreenBuffer -> Renderable m Float -> Maybe (IO (Renderable m Float)) 
parseControl [] _ _ _ _ _ = Nothing
parseControl (w:ws) ls par sbuf buf = ML.lookup w controlParsers >>= \f -> Just $ f ls par sbuf buf

parseLoop ::(Matrix m) => [String] -> Int -> Map String [Sequence Float] -> ScreenBuffer -> Renderable m Float -> IO (Renderable m Float) -- Maybe (Renderable m Float)
parseLoop [] _ _ _ buf = return buf
parseLoop (l:ls) par@(3DParse fnum varys _ _) sbuf buf = do
	putStrLn $ "Line: " ++ unwords ws
	fromMaybe (parseLoop ls par sbuf buf) -- Default case
		$   ((>> (parseLoop ls par sbuf buf)) <$> (parseIO ws sbuf buf)) 
		<|> (parse ws buf >>= \nBuf -> Just $ parseLoop ls par sbuf nBuf)
		<|> parseControl ws ls par sbuf buf 
	where
		nVarys = varys 
		ws = fromMaybe [] . varyFold fnum varys $ words l

