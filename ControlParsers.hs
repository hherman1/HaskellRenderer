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

import Parsers
import IOParsers

initControlParser :: (Matrix m) => [String] -> Int -> Map String (Sequence Float) -> ScreenBuffer -> Renderable m Float -> IO (Renderable m Float) -- Maybe (Renderable m Float)
initControlParser = parseLoop

controlParsers :: (Matrix m) => Map String ([String] -> Int -> Map String (Sequence Float) -> ScreenBuffer -> Renderable m Float -> IO (Renderable m Float)) -- Maybe (Renderable m Float))
controlParsers = ML.fromList [
	("stdParse", parseLoop),
	("end", \_ _ _ _ buf -> return buf) -- Just buf)
	]


parseControl :: (Matrix m) => [String] -> [String] -> Int -> Map String (Sequence Float) -> ScreenBuffer -> Renderable m Float -> Maybe (IO (Renderable m Float)) -- Maybe (Renderable m Float)
parseControl (w:ws) ls fnum varys sbuf buf = ML.lookup w controlParsers >>= \f -> Just $ f ls fnum varys sbuf buf

parseLoop ::(Matrix m) => [String] -> Int -> Map String (Sequence Float) -> ScreenBuffer -> Renderable m Float -> IO (Renderable m Float) -- Maybe (Renderable m Float)
parseLoop [] fnum varys sbuf buf = return buf -- Just buf
parseLoop (l:ls) fnum varys sbuf buf = fromMaybe (parseLoop ls fnum nVarys sbuf buf)
	$   ((>> (parseLoop ls fnum nVarys sbuf buf)) <$> (parseIO ws sbuf buf)) 
	<|> (parse ws buf >>= \nBuf -> Just $ parseLoop ls fnum nVarys sbuf nBuf)
	<|> parseControl ws ls fnum nVarys sbuf buf 
	where
		nVarys = ML.filter (currentSequence fnum) varys
		ws = map (\word -> case varyReplace fnum nVarys word of (Just var) -> var; Nothing -> word) $ words l

varyReplace :: Int -> Map String (Sequence Float) -> String -> Maybe String
varyReplace fnum varys word = ML.lookup word varys >>= Just . show . \seq@(Anim3D (fs,fe) (vs,ve)) -> (fromIntegral (fnum - fs) / fromIntegral (fe - fs)) * (ve - vs) + vs

parseVarys :: [String] -> Map String (Sequence Float)
parseVarys [] = ML.fromList []
parseVarys (l:ls) = let ws = words l in case ws of
	("vary":varName:args) -> let (fs:fe:vs:ve:_) = map readFloat args in ML.insert varName Anim3D {
		_frames = (floor fs,floor fe),
		_values = (vs,ve)
	} $ parseVarys ls
	_ -> parseVarys ls
