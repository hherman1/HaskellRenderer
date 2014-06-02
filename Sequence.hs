module Sequence
(	Sequence(..),
	currentSequence,
	seqsVal,
	chooseSeq,
	seqVal,
	)
where

data Sequence a = Anim3D {_frames :: (Int,Int), _values :: (a,a)} deriving (Show,Eq)

currentSequence :: Int -> Sequence a -> Bool
currentSequence f (Anim3D (fs,fe) _) = f >= fs && f <= fe

seqsVal :: (Num a, Fractional a) => Int -> [Sequence a] -> a
seqsVal n = seqVal n . chooseSeq n

chooseSeq :: Int -> [Sequence a] -> Sequence a
chooseSeq _ [] = error "no sequences to select from"
chooseSeq n seqs = case filter (currentSequence n) seqs of
		(s:_) -> s
		[] -> head seqs

seqVal :: (Num a,Fractional a) => Int -> Sequence a -> a
seqVal fnum (Anim3D (fs,fe) (vs,ve)) =
	case f (compare fnum fs) (compare fnum fe) of
		LT -> vs
		GT -> ve
		EQ -> (fromIntegral (fnum - fs) / fromIntegral (fe - fs)) * (ve - vs) + vs
	where
		f LT LT = LT
		f GT GT = GT
		f _ _ = EQ
