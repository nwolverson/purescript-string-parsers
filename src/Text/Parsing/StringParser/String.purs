module Text.Parsing.StringParser.String where
  
import Data.Either (Either(..))
import Data.String (charAt, length, take, indexOf')
import Text.Parsing.StringParser

eof :: Parser {}
eof = More $ \s -> case s of
  { str = str, pos = i } | i < length str -> { remainingInput: s, next: Left $ ParseError $ "Expected EOF" } 
  _ -> { remainingInput: s, next: Right $ pure {} } 

anyChar :: Parser String
anyChar = More $ \s -> case s of
  { str = str, pos = i } | i < length str -> { remainingInput: { str: str, pos: i + 1 }, next: Right $ pure $ charAt i str }
  { pos = i } -> { remainingInput: s, next: Left $ ParseError $ "Unexpected EOF" }

string :: String -> Parser String
string nt = More $ \s -> case s of
  { str = str, pos = i } | indexOf' nt i str == i -> { remainingInput: { str: str, pos: i + length nt }, next: Right (pure nt) }
  { pos = i } -> { remainingInput: s, next: Left $ ParseError $ "Expected '" ++ nt ++ "'." }