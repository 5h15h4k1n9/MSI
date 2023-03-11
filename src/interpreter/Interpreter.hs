module Interpreter where

import AST
import TypeOps
import Errors
import ValOps

evalBinOp :: SBinOp -> SValue -> SValue -> SValue
evalBinOp op v1 v2 = case op of
  -- Arithmetic
  Add -> v1 + v2
  Sub -> v1 - v2
  Mul -> v1 * v2
  Div -> v1 `quot` v2
  Mod -> v1 `rem` v2
  -- Logical
  And -> v1 `sAnd` v2
  Or  -> v1 `sOr` v2
  -- Comparison
  Eq  -> VBool (v1 == v2)
  Neq -> VBool (v1 /= v2)
  Lt  -> VBool (v1 < v2)
  Gt  -> VBool (v1 > v2)
  Leq -> VBool (v1 <= v2)
  Geq -> VBool (v1 >= v2)
  
evalUnOp :: SUnOp -> SValue -> SValue
evalUnOp op v = case op of
  Neg -> -v
  Not -> sNot v
  Inc -> v + VUInt8 1
  Dec -> v - VUInt8 1
