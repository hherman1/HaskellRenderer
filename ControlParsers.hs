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

import Varys
import Parsers
import IOParsers

initControlParser :: (Matrix m) => [String] -> Int -> Map String [Sequence Float] -> ScreenBuffer -> Renderable m Float -> IO (Renderable m Float) -- Maybe (Renderable m Float)
initControlParser = parseLoop

controlParsers :: (Matrix m) => Map String ([String] -> Int -> Map String [Sequence Float] -> ScreenBuffer -> Renderable m Float -> IO (Renderable m Float)) -- Maybe (Renderable m Float))
controlParsers = ML.fromList [
	("stdParse", parseLoop),
	("end", \_ _ _ _ buf -> return buf) -- Just buf)
	]


parseControl :: (Matrix m) => [String] -> [String] -> Int -> Map String [Sequence Float] -> ScreenBuffer -> Renderable m Float -> Maybe (IO (Renderable m Float)) -- Maybe (Renderable m Float)
parseControl (w:ws) ls fnum varys sbuf buf = ML.lookup w controlParsers >>= \f -> Just $ f ls fnum varys sbuf buf

parseLoop ::(Matrix m) => [String] -> Int -> Map String [Sequence Float] -> ScreenBuffer -> Renderable m Float -> IO (Renderable m Float) -- Maybe (Renderable m Float)
parseLoop [] fnum varys sbuf buf = return buf
parseLoop (l:ls) fnum varys sbuf buf = do
	putStrLn $ "Line: " ++ unwords ws
	fromMaybe (parseLoop ls fnum varys sbuf buf) -- Default case
		$   ((>> (parseLoop ls fnum varys sbuf buf)) <$> (parseIO ws sbuf buf)) 
		<|> (parse ws buf >>= \nBuf -> Just $ parseLoop ls fnum varys sbuf nBuf)
		<|> parseControl ws ls fnum varys sbuf buf 
	where
		nVarys = varys 
		ws = fromMaybe [""] . varyFold fnum varys $ words l

