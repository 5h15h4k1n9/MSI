module ValOps where

import AST
import Errors

instance (Num SValue) where
  (VUInt8 i1) + (VUInt8 i2) = VUInt8 (i1 + i2)
  _           + _           = error interpretationError
  (VUInt8 i1) - (VUInt8 i2) = VUInt8 (i1 - i2)
  _          - _           = error interpretationError
  (VUInt8 i1) * (VUInt8 i2) = VUInt8 (i1 * i2)
  _           * _           = error interpretationError
  abs (VUInt8 i) = VUInt8 (abs i)
  abs _          = error interpretationError
  signum (VUInt8 i) = VUInt8 (signum i)
  signum _          = error interpretationError
  fromInteger i = VUInt8 (fromInteger i)
  
instance (Eq SValue) where
  (VUInt8   i1      ) == (VUInt8   i2        ) = i1 == i2
  (VBool    b1      ) == (VBool    b2        ) = b1 == b2
  (VString  s1      ) == (VString  s2        ) = s1 == s2
  (VMapping t1 t2 m1) == (VMapping t1' t2' m2) = t1 == t1' && t2 == t2' && m1 == m2
  (VArray   t  v1 s1) == (VArray   t' v2 s2  ) = t == t' && v1 == v2 && s1 == s2
  _                   == _                     = error interpretationError
  
instance (Ord SValue) where
  compare (VUInt8  i1) (VUInt8  i2) = compare i1 i2
  compare (VBool   b1) (VBool   b2) = compare b1 b2
  compare (VString s1) (VString s2) = compare s1 s2
  compare _            _            = error interpretationError
  
instance (Enum SValue) where
  toEnum i = VUInt8 (toEnum i)
  fromEnum (VUInt8 i) = fromEnum i
  fromEnum _          = error interpretationError
  
instance (Real SValue) where
  toRational (VUInt8 i) = toRational i
  toRational _          = error interpretationError
  
instance (Integral SValue) where
  toInteger (VUInt8 i) = toInteger i
  toInteger _          = error interpretationError
  quotRem (VUInt8 i1) (VUInt8 i2) = (VUInt8 i1', VUInt8 i2')
    where (i1', i2') = quotRem i1 i2
  quotRem _           _           = error interpretationError

sAnd :: SValue -> SValue -> SValue
sAnd (VBool b1) (VBool b2) = VBool (b1 && b2)
sAnd _          _          = error interpretationError

sOr :: SValue -> SValue -> SValue
sOr (VBool b1) (VBool b2) = VBool (b1 || b2)
sOr _          _          = error interpretationError

sNot :: SValue -> SValue
sNot (VBool b) = VBool (not b)
sNot _         = error interpretationError