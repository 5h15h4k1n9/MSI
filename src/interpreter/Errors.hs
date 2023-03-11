module Errors where

import AST
import TypeOps

interpretationError :: String 
interpretationError = "Interpretation error"

cannotEvalBinOp :: SBinOp -> SValue -> SValue -> String
cannotEvalBinOp op v1 v2 = "Cannot evaluate operation (" ++ show op ++ ") on " ++ show v1 ++ " and " ++ show v2

cannotEvalUnOp :: SUnOp -> SValue -> String
cannotEvalUnOp op v = "Cannot evaluate operation (" ++ show op ++ ") on " ++ show v