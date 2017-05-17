module Expression where

data Function = Function String Int Expression
  deriving (Show, Eq)

data Expression = Literal Integer
                | Decimal Float
                | Symbol String
                | Infinity
                | Application Function [Expression]
                deriving(Show, Eq)

data Equation = Equals        Expression Expression
              | Less          Expression Expression
              | Greater       Expression Expression
              | LessEquals    Expression Expression
              | GreaterEquals Expression Expression
              | Approximates  Expression Expression
              deriving(Show, Eq)

lhs :: Equation -> Expression
lhs (Equals x _) = x
lhs (Less x _) = x
lhs (Greater x _) = x
lhs (LessEquals x _) = x
lhs (GreaterEquals x _) = x
lhs (Approximates x _) = x

rhs :: Equation -> Expression
rhs (Equals _ y) = y
rhs (Less _ y) = y
rhs (Greater _ y) = y
rhs (LessEquals _ y) = y
rhs (GreaterEquals _ y) = y
rhs (Approximates _ y) = y
