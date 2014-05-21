module Strings 
(
	readFloat,
	)
where

readFloat :: String -> Float
readFloat src = read . (\(a:as) -> case a of '.' -> '0':a:as; '-' -> a:'0':as; _ -> a:as) $ src ::Float
