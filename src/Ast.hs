module Ast where
import Data.Map

type Id = String

data MSVisibility =
    Public
  | Private
  deriving (Show, Eq)

data MSType =
    TInt
  | TString
  | TBool
  | TArray MSType
  | TMapping MSType MSType
  | TVoid
  deriving (Show, Eq, Ord)

data MSValue =
    VNull MSType
  | VInt Int
  | VString String
  | VBool Bool
  | VArray MSType [MSValue]
  | VMapping MSType MSType (Map MSValue MSValue)
  | VVoid
  deriving (Show, Eq, Ord)

data MSBinOp =
    Add   -- +
  | Minus -- -
  | Mul   -- *
  | Div   -- /
  | Mod   -- %
  | And   -- &&
  | Or    -- ||
  | Eq    -- ==
  | Gr    -- >
  | Le    -- <
  | Gre   -- >=
  | Leq   -- <=
  | Neq   -- !=
  | Index -- []
  deriving (Show, Eq)

data MSUnOp =
    UnMinus
  | Negate
  deriving (Show, Eq)

data MSExpr =
    -- Value
    EValue MSValue
    -- Operators
  | EBinOp MSBinOp MSExpr MSExpr
  | EUnOp MSUnOp MSExpr
    -- Function call 
  | ECall Id [MSExpr]
    -- Array operations
  | EArrayInit MSType [MSExpr]
    -- Name access
  | EName Id
  deriving (Show, Eq)

data MSFunction = MSFunction {
  funcVisibility :: MSVisibility,
  argsSignature  :: [(MSType, Id)],
  retType        :: Maybe MSType,
  body           :: MSBlock
} deriving (Show, Eq)

data MSTopLevelStatement =
    STopLevelVarDecl MSType MSVisibility Id (Maybe MSExpr)
  | SConstantDecl MSType MSVisibility Id MSValue
  | SFunctionDecl Id MSFunction
  deriving (Show, Eq)

type MSBlock = [MSStatement]

data MSLocalVarDecl = LocalVarDecl MSType Id MSExpr
  deriving (Show, Eq)

data MSStatement =
    -- Expr
    SExpr MSExpr
    -- Loops
  | SFor MSLocalVarDecl MSExpr MSStatement MSBlock -- for (statement; expr; statement) statement
  | SWhile MSExpr MSBlock -- while (expr) statement
    -- Declarations
  | SLocalVarDecl MSLocalVarDecl
    -- Assign
  | SAssign Id MSExpr
  | SIndexAssign Id [MSExpr] MSExpr
    -- Require
  | SRequire MSExpr MSExpr
    -- If
  | SIf (MSExpr, MSBlock) [(MSExpr, MSBlock)] (Maybe MSBlock)
    -- Jumps
  | SReturn MSExpr
  | SBreak
  | SContinue
    -- Block
  deriving (Show, Eq)

data MSProgram = MSProgram {
  name :: String,
  declarations :: [MSTopLevelStatement]
} deriving (Show, Eq)

