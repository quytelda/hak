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

reduce :: Expression -> Expression
reduce expr@(Literal x)      = expr
reduce expr@(Decimal x)      = expr
reduce expr@(Symbol x)       = expr
reduce Infinity              = Infinity
reduce (Application fn args) = undefined

evaluate :: Num a => Expression -> Maybe a
evaluate (Literal x)           = Just x
evaluate (Decimal x)           = Just x
evaluate (Symbol s)            = Nothing
evaluate Infinity              = Nothing
evaluate (Application fn args) = undefined
