module Text.Parsing.StringParser where

import Data.Maybe (Maybe(..))
import Data.String (charAt, length, take)
import Data.Either (Either(..))

import Control.Monad.Eff

type Pos = Number

foreign import data Parse :: !

foreign import data ParserState :: *

foreign import pushPosition
  "function pushPosition(st) {\
  \  return function() {\
  \    st.stack.push(st.pos);\
  \    return {};\
  \  };\
  \}" :: forall eff. ParserState -> Eff (parse :: Parse | eff) {}

foreign import popPosition
  "function popPosition(st) {\
  \  return function() {\
  \    st.pos = st.stack.push();\
  \    return {};\
  \  };\
  \}" :: forall eff. ParserState -> Eff (parse :: Parse | eff) {}

foreign import getPos
  "function getPos(st) {\
  \  return function () {\
  \    return st.pos;\
  \  };\
  \}" :: forall eff. ParserState -> Eff (parse :: Parse | eff) Pos 

foreign import getInput
  "function getInput(st) {\
  \  return function () {\
  \    return st.str;\
  \  };\
  \}" :: forall eff. ParserState -> Eff (parse :: Parse | eff) String

foreign import incrPos
  "function incrPos(st) {\
  \  return function(n) {\
  \    return function () {\
  \      st.pos += n;\
  \      return {};\
  \    };\
  \  };\
  \}" :: forall eff. ParserState -> Number -> Eff (parse :: Parse | eff) {}

foreign import runParse
  "function runParse(p) {\
  \  return p;\
  \}" :: forall eff a. Eff (parse :: Parse | eff) a -> Eff eff a

foreign import defaultParserState
  "function defaultParserState(s) {\
  \  return function() {\
  \    return {\
  \      str: s,\
  \      pos: 0,\
  \      stack: []\
  \    };\
  \  };\
  \}" :: forall eff. String -> Eff (parse :: Parse | eff) ParserState

data Parser eff a = Parser (ParserState -> Eff (parse :: Parse | eff) (Either String a))

type PureParser a = forall eff. Parser eff a

unParser :: forall eff a. Parser eff a -> ParserState -> Eff (parse :: Parse | eff) (Either String a)
unParser (Parser p) = p

runParser :: forall eff a. Parser eff a -> String -> Eff eff (Either String a)
runParser (Parser p) s = runParse $ do
  st <- defaultParserState s
  p st

runPureParser :: forall a. PureParser a -> String -> Either String a
runPureParser p s = runPure (runParser p s)

--
-- Parser type class instances
--

instance functorParser :: Functor (Parser eff) where
  (<$>) f p = Parser $ \st -> do
    e <- unParser p st
    return (f <$> e)

instance applyParser :: Apply (Parser eff) where
  (<*>) = ap 

instance applicativeParser :: Applicative (Parser eff) where
  pure a = Parser $ \_ -> return (return a)

instance bindParser :: Bind (Parser eff) where
  (>>=) p f = Parser $ \st -> do
    e <- unParser p st
    case e of
      Left err -> return (Left err)
      Right a -> unParser (f a) st  

instance monadParser :: Monad (Parser eff)

instance alternativeParser :: Alternative (Parser eff) where
  empty = fail "No alternative"
  (<|>) p1 p2 = Parser $ \st -> do
    pushPosition st
    e <- unParser p1 st
    case e of
      Left _ -> do
        popPosition st
        unParser p2 st
      r -> return r

--
-- Some elementary parsers
--

fail :: forall eff a. String -> Parser eff a
fail msg = Parser $ \st -> return (Left msg)

eof :: forall eff. Parser eff {}
eof = Parser $ \st -> do
  str <- getInput st
  i <- getPos st
  return $ case {} of
    _ | i < length str -> Left "Expected EOF"
    _ -> Right {}

anyChar :: forall eff. Parser eff String
anyChar = Parser $ \st -> do
  str <- getInput st 
  i <- getPos st
  case {} of 
    _ | i < length str -> do
      incrPos st 1
      return $ Right $ charAt i str
    _ -> return $ Left "Unexpected EOF" 

foreign import indexOf'
  "function indexOf$prime(x) {\
  \  return function(startAt) {\
  \    return function(s) {\
  \      return s.indexOf(x, startAt);\
  \    }; \
  \  }; \
  \}" :: String -> Number -> String -> Number

string :: forall eff. String -> Parser eff String
string nt = Parser $ \st -> do
  str <- getInput st
  i <- getPos st
  case {} of 
    _ | indexOf' nt i str == i -> do
      incrPos st $ length nt
      return $ Right nt
    _ -> return $ Left $ "Expected '" ++ nt ++ "' at position " ++ show i ++ "."

--
-- Parsing Combinators
--

many :: forall eff a. Parser eff a -> Parser eff [a]
many p = many1 p <|> return []

many1 :: forall eff a. Parser eff a -> Parser eff [a]
many1 p = do
  a <- p
  as <- many p
  return (a : as)

(<?>) :: forall eff a. Parser eff a -> String -> Parser eff a
(<?>) p msg = p <|> fail msg

fix :: forall eff a. (Parser eff a -> Parser eff a) -> Parser eff a
fix f = Parser $ \st -> unParser (f (fix f)) st

between :: forall eff a open close. Parser eff open -> Parser eff close -> Parser eff a -> Parser eff a
between open close p = do
  open
  a <- p
  close 
  return a

option :: forall eff a. a -> Parser eff a -> Parser eff a
option a p = p <|> return a

optional :: forall eff a. Parser eff a -> Parser eff {}
optional p = (p >>= \_ -> return {}) <|> return {}

optionMaybe :: forall eff a. Parser eff a -> Parser eff (Maybe a)
optionMaybe p = option Nothing (Just <$> p)

sepBy :: forall eff a sep. Parser eff a -> Parser eff sep -> Parser eff [a]
sepBy p sep = sepBy1 p sep <|> return []

sepBy1 :: forall eff a sep. Parser eff a -> Parser eff sep -> Parser eff [a]
sepBy1 p sep = do
  a <- p
  as <- many $ do
    sep
    p
  return (a : as)

sepEndBy :: forall eff a sep. Parser eff a -> Parser eff sep -> Parser eff [a]
sepEndBy p sep = sepEndBy1 p sep <|> return []

sepEndBy1 :: forall eff a sep. Parser eff a -> Parser eff sep -> Parser eff [a]
sepEndBy1 p sep = do
  a <- p
  (do sep
      as <- sepEndBy p sep
      return (a : as)) <|> return [a]

endBy1 :: forall eff a sep. Parser eff a -> Parser eff sep -> Parser eff [a]
endBy1 p sep = many1 $ do 
  a <- p
  sep
  return a

endBy :: forall eff a sep. Parser eff a -> Parser eff sep -> Parser eff [a]
endBy p sep = many $ do
  a <- p
  sep
  return a

chainr :: forall eff a. Parser eff a -> Parser eff (a -> a -> a) -> a -> Parser eff a
chainr p f a = chainr1 p f <|> return a

chainl :: forall eff a. Parser eff a -> Parser eff (a -> a -> a) -> a -> Parser eff a
chainl p f a = chainl1 p f <|> return a

chainl1 :: forall eff a. Parser eff a -> Parser eff (a -> a -> a) -> Parser eff a
chainl1 p f = do
  a <- p
  chainl1' p f a

chainl1' :: forall eff a. Parser eff a -> Parser eff (a -> a -> a) -> a -> Parser eff a
chainl1' p f a = (do f' <- f
                     a' <- p
                     chainl1' p f (f' a a')) <|> return a

chainr1 :: forall eff a. Parser eff a -> Parser eff (a -> a -> a) -> Parser eff a
chainr1 p f = do
  a <- p
  chainr1' p f a

chainr1' :: forall eff a. Parser eff a -> Parser eff (a -> a -> a) -> a -> Parser eff a
chainr1' p f a = (do f' <- f
                     a' <- chainr1 p f
                     return $ f' a a') <|> return a

choice :: forall eff a. [Parser eff a] -> Parser eff a
choice []   = fail "Nothing to parse"
choice [x]  = x
choice (x:xs) = x <|> choice xs
