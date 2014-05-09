module Text.Parsing.StringParser where
  
import Data.Either (Either(..))

type Pos = Number

-- 
-- Strings are represented as a string with an index from the
-- start of the string. 
--
-- { str: s, pos: n } is interpreted as the substring of s
-- starting at index n.
--
-- This allows us to avoid repeatedly finding substrings
-- every time we match a character.
--
type PosString = { str :: String, pos :: Pos }

--
-- The type of parsing errors
--
data ParseError = ParseError String

instance showParseError :: Show ParseError where
  show (ParseError msg) = msg

data Parser a 
  = Done a
  | More (PosString -> { remainingInput :: PosString, next :: Either ParseError (Parser a) })

runParser :: forall a. Parser a -> String -> Either ParseError a
runParser p s = go p { str: s, pos: 0 }
  where
  go :: forall a. Parser a -> PosString -> Either ParseError a
  go (Done a) _ = Right a
  go (More f) s = case f s of
                    { next = Left err } -> Left err
                    o@{ next = Right next } -> go next o.remainingInput

--
-- Parser type class instances
--

instance functorParser :: Functor Parser where
  (<$>) f (Done a) = Done (f a)
  (<$>) f (More more) = More $ \s -> let o = more s in { remainingInput: o.remainingInput, next: ((<$>) f) <$> o.next }

instance applyParser :: Apply Parser where
  (<*>) = ap

instance applicativeParser :: Applicative Parser where
  pure = Done

instance bindParser :: Bind Parser where
  (>>=) (Done a) f = f a
  (>>=) (More more) f = More $ \s -> let o = more s in { remainingInput: o.remainingInput, next: (flip (>>=) f) <$> o.next }

instance monadParser :: Monad Parser

instance alternativeParser :: Alternative Parser where
  empty = fail "No alternative"
  (<|>) p1 p2 = mark p1
    where
    mark (Done a) = Done a
    mark (More more) = More $ \s -> go s (more s)
    
    go s o@{ next = Left _ } | o.remainingInput.pos == s.pos = { remainingInput: s, next: Right p2 }
    go s o@{ next = Left _ } = o
    go s o@{ next = Right (Done a) } = { remainingInput: o.remainingInput, next: Right (Done a) }
    go s o@{ next = Right (More more) } = { remainingInput: o.remainingInput, next: Right (More $ \s' -> go s (more s')) }

fail :: forall a. String -> Parser a
fail msg = More $ \s -> { remainingInput: s, next: Left (ParseError msg) }

fix :: forall a. (Parser a -> Parser a) -> Parser a
fix f = More (\s -> { remainingInput: s, next: Right (f (fix f)) })

try :: forall a. Parser a -> Parser a
try (More more) = More $ \s -> 
  let
    go { remainingInput = ri, next = Left msg } = { remainingInput: s, next: Left msg }
    go { remainingInput = ri, next = Right (Done a) } = { remainingInput: ri, next: Right (Done a) }
    go { remainingInput = ri, next = Right (More more) } = { remainingInput: ri, next: Right (More $ \s -> go (more s)) }
  in go (more s)
try other = other