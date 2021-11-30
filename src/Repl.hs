module Repl where

import Data.Text
import Prelude hiding (elem, length, lines, takeWhile, words)

data Token = Token 
  { text :: Text
  , paren :: Int
  , bracket :: Int
  , brace :: Int
  , angle :: Int
  }
  deriving stock (Eq, Show)

-- this is telling me I should use attoparsec. later!
isPunct :: Char -> Bool
isPunct t = t `elem` ("~`!@#$%^&*()-_=+[]{}\\|;:',<.>/?" :: Text)

-- okay, an operator is not just a string of punctuation, but it works for v1
isOperator :: Text -> Bool
isOperator t = t == takeWhile isPunct t

mkToken :: Text -> Token
mkToken t = 
  if isOperator t then Token t 0 0 0 0
  else Token t par brk brc ang
  where par = count "(" t - count ")" t
        brk = count "[" t - count "]" t
        brc = count "{" t - count "}" t
        ang = count "<" t - count ">" t

catTokens :: Token -> Token -> Token
catTokens (Token lhs _ _ _ _) (Token rhs _ _ _ _) = mkToken (lhs <> " " <> rhs)

isClosed :: Token -> Bool
isClosed = \case
  Token t _ _ _ _ | 20 < length t -> False
  Token _ 0 0 0 0 -> True
  _ -> False

fusePrefix :: Token -> [Token] -> (Token, [Token])
fusePrefix tHead tTail = case tTail of
  [] -> (tHead, [])
  t:ts | isClosed tHead -> (tHead, tTail)
  t:ts | otherwise -> if isClosed . fst $ fusePrefix (catTokens tHead t) ts
                      then fusePrefix (catTokens tHead t) ts
                      else (tHead, tTail)

fuseLine :: [Token] -> [Token]
fuseLine tokens = fst $ fuseRestOfLine ([], tokens)
  where
    fuseRestOfLine :: ([Token], [Token]) -> ([Token], [Token])
    fuseRestOfLine (fused, []) = (fused, [])
    fuseRestOfLine (fused, u:us) = fuseRestOfLine (fused ++ [prefix], suffix)
      where
        (prefix, suffix) = fusePrefix u us

tokenizeLine :: Text -> [Token]
tokenizeLine t = fuseLine $ mkToken <$> words t

tokenizeCorpus :: Text -> [Token]
tokenizeCorpus t = mconcat $ tokenizeLine <$> lines t