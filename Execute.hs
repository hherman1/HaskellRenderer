module Execute (
	RenderState (..),
	genState,
	runCommand
) where

import Parser
import OpenGL
import Operations
import Matrix
import Matrix3D
import Sequence
import RenderVector

import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V

import Data.Map (Map)
import qualified Data.Map as ML
import Data.Maybe
import Data.List (intersperse)
import Control.Applicative
import Control.Monad.Trans.State
import Control.Monad.IO.Class


data RenderState m a d = RenderState {_fnum :: Int, 
				_varys :: Map String [Sequence Double],
				_currentTransform :: m a,
				_transformations :: Map String (m a),
				_renderable :: Renderable m a d,
				_out :: Resolution Int,
				_buffer :: Vector (d)}
				deriving Show
genState :: (V.Storable d) => Renderable ListMatrix Double d -> d -> Resolution Int -> Int -> RenderState ListMatrix Double d
genState renderable@(Renderable {_col=col}) v out@(Area (_,ox) (_,oy))  n= 
	RenderState n (ML.fromList []) (identity 4 4) (ML.fromList []) 
		renderable out $ V.replicate (ox * oy) v

testTransformationString = "scale 1 1 2\nmove 2 0 0\n save basic\nrotate 1 1 1\ncube 1 1 1 0 0 0 0 0 0\nrestore basic\ncube 1 1 1 0 0 0 0 0 0"
{-
test :: [Command] -> IO (RenderState ListMatrix Double)
test cs = execStateT (mapM_ runCommand cs) . genState 
	(initRenderable  (Area (0,100) (0,100)) (1,1,1))
	(Area (0,100) (0,100)) $
	1 
-}
type Tform = (Double,Double,Double)

				
getValue :: (b -> a) -> Map String b -> Val a -> a
getValue _ _ (Literal x) = x
getValue f m (Variable s) = fromMaybe (error $ "var not found: " ++ s) $
	f <$> ML.lookup s m

getTransform :: (b -> a) -> Map String b -> Transform a -> (a,a,a)
getTransform f m (a, b, c) = (getValue f m a, getValue f m b, getValue f m c)

transformAndUpdateTri :: (Matrix m,Num a) => [m a] -> StateT (RenderState m a d) IO ()
transformAndUpdateTri tris = do
	ss@(RenderState {_currentTransform = cst,
			_renderable = rm}) <- get
	put $ ss { _renderable = rm {_triangleMatrix = 
					(map (transform cst) $ tris)
					++ _triangleMatrix rm }}

updateVector :: (V.Storable d) => (Vector Int,Vector d) -> Vector d -> Vector d
updateVector (ind,v) o = V.update_ o ind v

--The funky bits at the beginning of each op are extracting data from the Val's, whcih are potentially variable
runCommand :: (Matrix m,V.Storable d) => Command -> StateT (RenderState m Double d) IO ()

runCommand (Cube ts tr tm) = do
	RenderState {_fnum = fnum, _varys = vs} <- get
	let 	
		gt = getTransform (seqsVal fnum) vs
		(s,r,m) = (gt ts,gt tr,gt tm)
	transformAndUpdateTri $ cube s r m

runCommand (Sphere rad div ts tr tm) = do
	RenderState {_fnum = fnum, _varys = vs} <- get
	let 	
		gt = getTransform (seqsVal fnum) vs
		(s,r,m) = (gt ts,gt tr,gt tm)
		(radius,division) = (getValue (seqsVal fnum) vs rad,
				getValue (seqsVal fnum) vs div)
	transformAndUpdateTri $ sphere radius division s r m

runCommand (Transformation mode st) = do
	RenderState {_varys = vs, _fnum = fnum, _currentTransform = cst} <- get
	let s = getTransform (seqsVal fnum) vs st
	modify $ \ss -> ss {_currentTransform = transform cst $ case mode of
		Scale -> scale s
		Rotate -> rotate s
		Move -> move s}

runCommand (Save s) = do
	modify $ \ss@(RenderState {_currentTransform = cst}) ->
		ss {_transformations = ML.insert s cst $ _transformations ss}

runCommand (Restore s) = do
	modify $ \ss -> ss {_currentTransform = fromMaybe (error "transform not found") $
		ML.lookup s $ _transformations ss}

runCommand (AddVar s vv vf) = do
	RenderState {_varys = vs, _fnum = fnum} <- get
	let 
		g = getValue (seqsVal fnum) vs
		f x (a,b) = (x a, x b)
		(vals,frames) = (f g vv,f (floor . g) vf)
	modify $ \ss -> ss {_varys = ML.insertWith (++) s [Anim3D frames vals] vs}

runCommand RenderParallel = do
	RenderState {_renderable = renderable,_out = out,_buffer = buf} <- get
	modify $ \ss -> ss {_buffer = foldr updateVector buf $
				renderParallel out renderable}

runCommand (RenderCyclops e) = do
	RenderState {_varys = vs, _fnum = fnum, 
		_renderable = renderable,_out = out,_buffer = buf} <- get
	let eye = getTransform (seqsVal fnum) vs e
	modify $ \ss -> ss {_buffer = foldr updateVector buf $
				renderCyclops out eye renderable}

runCommand (RenderStereo e1 e2) = do
	RenderState {_varys = vs, _fnum = fnum, _buffer= buf,
		_renderable = renderable,_out = out} <- get
	let eye1 = getTransform (seqsVal fnum) vs e1
	let eye2 = getTransform (seqsVal fnum) vs e2
	modify $ \ss -> ss {_buffer = foldr updateVector buf $
		renderStereo out (eye1,eye2) renderable}

runCommand Display = do
	RenderState {_buffer = buf,_out = out} <- get
	liftIO $ displayVector (toSize out) buf
{-
runCommand (Files s) = do
	RenderState {_fnum = fnum,
		_buffer = buf,
		_out = out} <- get
	liftIO $ writeFrame s fnum out buf
-}
runCommand Unknown = return ()

--Debug
runCommand c = do
	liftIO $ putStrLn $ show c
