module Errors where
import Ast
import Prelude hiding (lookup)
import Data.Map

data MSJump =
    Break
  | Continue
  | Return MSValue
  deriving (Show)

data MSInterpretationError =
    NameNotExist Id
  | DuplicatedName Id
  | UndefinedName Id
  | MismatchedType MSType MSType
  | UnsupportedBinOp MSBinOp MSType MSType
  | UnsupportedUnOp MSUnOp MSType
  | ZeroDivision
  | IndexOutOfBounds MSValue
  | KeyNotFound MSValue
  | MismatchedArgsCount
  | Jump MSJump
  | RequireFailed String
  deriving (Show)
