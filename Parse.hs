module Parse
(
	Parser (..),
	varyFold,
	parseVarys,
	)
where
import Sequence
import Strings
import Data.Map (Map)
import qualified Data.Map as ML
import Data.Maybe 

data Parser m a = Parse3D { 
		_frame :: Int,
		_varys :: Map String [Sequence Float],
		_currentTransform :: m a,
		_transformations :: Map String (m a)
} deriving (Show, Eq)
--confusing code
--Intent:
--	try and replace all variables in a line with active values of those variables
--	if a variable is known but inactive,ignore the line
--	if a variable is unknown, leave it be. This case does not error when the variable in question is actually a value to be used by the function
varyFold ::Int -> Map String [Sequence Float] -> [String] -> Maybe [String]
varyFold fnum seq ws = foldr (f fnum seq) (Just []) ws
	where
		f n s w sentence = 									-- Be mysterious. This is haskell.
			case ML.lookup w s of
				Just seqs ->								-- Found Variable:
					case filter (currentSequence fnum) seqs of 			-- Ignore inactive sequences
						[] -> Nothing						-- No active sequences? Skip Line
						(s:_) -> fmap ((:) $ show $ seqVal fnum s) sentence	-- If you found an active sequence, append the value of the sequence to the words list
				Nothing -> fmap (w:) sentence						-- Did not find Variable: append word to words list

parseVarys :: [String] -> Map String [Sequence Float]
parseVarys [] = ML.fromList []
parseVarys (l:ls) = let ws = words l in case ws of
	("vary":varName:args) -> let (fs:fe:vs:ve:_) = map readFloat args in ML.insertWith (++) varName [Anim3D {
		_frames = (floor fs,floor fe),
		_values = (vs,ve)
	}] $ parseVarys ls
	_ -> parseVarys ls

