module Sequence
(	Sequence(..),
	currentSequence,
	)
where

data Sequence a = Anim3D {_frames :: (Int,Int), _values :: (a,a)}

currentSequence :: Int -> Sequence a -> Bool
currentSequence f (Anim3D (fs,fe) _) = f >= fs && f <= fe
