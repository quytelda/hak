module Expression where

data Function = Function String Int Expression
              deriving(Show,Eq)

data Expression = Literal Integer
                | Decimal Float
                | Symbol String
                | Infinity
                | Operation String Expression Expression
                | Application Function [Expression]
                deriving(Show, Eq)

data Result = IntVal Integer
            | DecVal Float
            | NaN
            deriving(Eq)

instance Show Result where
  show (IntVal x) = show x
  show (DecVal x) = show x
  show NaN        = "NaN"

evaluate :: Expression -> Result
evaluate (Literal x)           = IntVal x
evaluate (Decimal x)           = DecVal x
evaluate (Symbol s)            = NaN
evaluate Infinity              = NaN
evaluate (Operation opStr x y) = undefined
evaluate (Application fn args) = undefined

reduce :: Expression -> Expression
reduce expr@(Literal x)      = expr
reduce expr@(Decimal x)      = expr
reduce expr@(Symbol x)       = expr
reduce Infinity              = Infinity
reduce (Application fn args) = undefined
