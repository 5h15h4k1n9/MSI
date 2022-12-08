module AST where

import Data.Map  (Map)
import Data.Word (Word8)

type Id = String
type Message = String

data AccessModifier =
    Public
  | Private
  deriving Eq

instance (Show AccessModifier) where
  show Public  = "public"
  show Private = "private"

data SType =
    TUInt8                                                       -- uint8
  | TBool                                                        -- bool
  | TString                                                      -- string
  | TMapping SType SType                                         -- mapping(uint => bool)
  | TArray SType Integer                                         -- uint[10]
--  | TFun [SType] AccessModifier (Maybe SType)                    -- function (uint, bool) public returns(uint)
  deriving Eq

instance Show SType where
  show TUInt8             = "uint8"
  show TBool              = "bool"
  show TString            = "string"
  show (TMapping t1 t2  ) = "mapping(" ++ show t1 ++ " => " ++ show t2 ++ ")"
  show (TArray   t  i   ) = show t ++ "[" ++ show i ++ "]"
--  show (TFun     ts a mt) = "function (" ++ show ts ++ ") " ++ show a ++ case mt of
--    Nothing -> ""
--    Just t  -> " returns(" ++ show t ++ ")"


data SValue =
    VUInt8 Word8                                                 -- 1, 2, 3
  | VBool Bool                                                   -- true, false
  | VString String                                               -- "abc"
  | VMapping (Map SValue SValue)                                 -- {1 => true, 2 => false}
  | VArray SType [SValue] Integer                                -- [1, 2, 3]
--  | VFun [(Id, SType)] AccessModifier (Maybe SType) SStmt   -- function (uint x, bool y) public returns(uint) { return x; }
  deriving (Eq, Show)

data SUnOp =
    Negate                                                       -- -
  | Not                                                          -- !
  deriving Eq

instance Show SUnOp where
  show Negate = "-"
  show Not    = "!"
  
data SBinOp =
-- Arithmetic
    Add                                                          -- +  
  | Sub                                                          -- -
  | Mul                                                          -- *
  | Div                                                          -- /
  | Mod                                                          -- %
-- Logical
  | And                                                          -- &&
  | Or                                                           -- ||
-- Comparison
  | Eq                                                           -- ==
  | Neq                                                          -- !=
  | Lt                                                           -- <
  | Gt                                                           -- >
  | Leq                                                          -- <=
  | Geq                                                          -- >=
  deriving Eq
  
instance Show SBinOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Mod = "%"
  show And = "&&"
  show Or  = "||"
  show Eq  = "=="
  show Neq = "!="
  show Lt  = "<"
  show Gt  = ">"
  show Leq = "<="
  show Geq = ">="
  
data SExpr =
    EVar Id                                                      -- x
  | EVal SValue                                                  -- 1, true, "abc"
  | EUnOp SUnOp SExpr                                            -- -x, !x
  | EBinOp SBinOp SExpr SExpr                                    -- x + y, x && y
  | EArrAccess SExpr SExpr                                       -- x[0]
  | EFunCall Id [SExpr]                                          -- foo(x, y)
  deriving (Eq, Show)
  
data SStmt =
    SVarDecl Id AccessModifier                                   -- uint public x;
  | SConstDecl Id AccessModifier SExpr                           -- uint private constant x = 1;
  | SAssign SExpr SExpr                                          -- x = 1;
  | SIf SExpr SStmt                                              -- if (x) { return y; }
  | SFor Id SExpr SExpr SStmt                                    -- for (uint i = 0; i < 10; i++) { return i; }
  | SReturn (Maybe SExpr)                                        -- return x;
  | SRequire SExpr Message                                       -- require(!x, "x is not true");
  | SBlock [SStmt]                                               -- { uint x = 1; return x; }
  deriving (Eq, Show)
  
data SContract = SContract Id SStmt deriving (Eq, Show)          -- contract Foo { uint public x = 1; }
  