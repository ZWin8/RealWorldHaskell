{-module Parse (
					Parse (..)
				,	ParseState (..)
				,	identity
				,	(==>)
				,	parse
				,	getState
				,	putState
				,	bail
				,	parseByte
			) 
	where-}

module Parse where

import qualified Data.ByteString.Lazy as L
-- import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Int (Int64)
-- import Control.Monad
import Data.Word

data ParseState = ParseState {
							string::L.ByteString
						,	offset::Int64
						}	deriving (Show)


-- Parse :: (ParseState -> Either String (a, ParseState)) -> Parse a
-- Note that Parse takes a function type as its parameter
newtype Parse a = Parse { runParse :: ParseState -> Either String (a, ParseState) } 

--------------------------------------------------------------------------------------
instance Monad Parse where
	return = identity
	(>>=) = (==>)

identity ::  a -> Parse a
identity i = Parse (\s -> Right (i, s))

(==>) :: Parse a -> (a -> Parse b) -> Parse b
infix 0 ==>
(==>) parser f = Parse chainedpar
		where 
			chainedpar initState = 
				case runParse parser initState of
					Left errMsg -> Left errMsg
					Right (result, newState) -> runParse (f result) newState  

instance Functor Parse where
	fmap f parser = parser ==> identity . f 

--------------------------------------------------------------------------------------

-- Parse :: (ParseState -> Either String (a, ParseState)) -> Parse a
-- Do the reverse, however initial ParseState is only a string.
parse :: Parse a -> L.ByteString -> Either String a
parse parser initState = 
	case runParse parser (ParseState initState 0) of
		Left errMsg -> Left errMsg
		Right (result, _) -> Right result

-- getState != return ParseState
getState :: Parse ParseState
getState = Parse (\s -> Right (s,s))

putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))

bail :: String -> Parse a
bail err = Parse $ \s -> Left $
				"byte offset" ++ show (offset s) ++ ":" ++ err


-- consume one byte per time
parseByte :: Parse Word8
parseByte =
	getState ==> \initState ->
	case L.uncons (string initState) of
		Nothing -> 
			bail "no more input"
		Just (byte, remainder) ->
			putState newState ==> \_ -> identity byte
			where 
				newState = initState { string = remainder, offset = newOffset}
				newOffset = offset initState + 1

--------------------------------------------------------------------------------------
{-
main::IO ()
main = do
		 ws <- u st
		 print ws
		where
			st = L.readFile "C:\\users\\z\\desktop\\RealWorld\\1.bmp"
			u = liftM L.unpack
-}