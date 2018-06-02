import Data.Char (isDigit)

data Term
  = TermInt Integer
  | TermOp (Integer -> Integer -> Integer)

evaluate :: String -> [Integer]
evaluate = evalTerms . map mkTerm . words

mkTerm :: String -> Term
mkTerm termStr = case termStr of
  "+" -> TermOp (+)
  "-" -> TermOp (-)
  "*" -> TermOp (*)
  _
    | all isDigit termStr -> TermInt $ read termStr
    | otherwise -> error $ "invalid input `" ++ termStr ++ "'"

evalTerms :: [Term] -> [Integer]
evalTerms = foldl modifyStack []
  where
  modifyStack stack term = case term of
    TermInt n -> n : stack
    TermOp op -> case stack of
      (a:b:_) -> op a b : drop 2 stack
      _ -> error "stack too small for operator application"
