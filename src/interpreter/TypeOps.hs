module TypeOps where

import AST

getType :: SValue -> SType
getType (VUInt8   _      ) = TUInt8
getType (VBool    _      ) = TBool
getType (VString  _      ) = TString
getType (VMapping t1 t2 _) = TMapping t1 t2
getType (VArray   t  _  s) = TArray t s

checkType :: SValue -> SType -> Bool
checkType v t = getType v == t

compareTypes :: SValue -> SValue -> Bool
compareTypes v1 v2 = getType v1 == getType v2