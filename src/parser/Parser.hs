{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Parser (parseProgram, parseReplStatement) where

import Lexer
import Ast

import Data.Maybe
import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>))

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Debug.Trace
-- visibility

msVisibility :: Parser MSVisibility
msVisibility =
  try msPublic
  <|> msPrivate
  where
    msPublic  = Public  <$ reserved "public"
    msPrivate = Private <$ reserved "private"

-- types

msType :: Parser MSType
msType =
  try tarray
  <|> tNonArray

tNonArray :: Parser MSType 
tNonArray =
  try tint
  <|> try tstring
  <|> try tbool
  <|> tmapping

tint :: Parser MSType
tint = TInt <$ reserved "int"

tstring :: Parser MSType
tstring = TString <$ reserved "string"

tbool :: Parser MSType
tbool = TBool <$ reserved "bool"

tarray :: Parser MSType
tarray = do
  innerType <- tNonArray
  br <- many1 (brackets (sym ""))
  let inheritanceSize = length br
  return $ makeArrayT inheritanceSize innerType
  where
    makeArrayT 1 t = TArray t
    makeArrayT x t = TArray $ makeArrayT (x - 1) t

tmapping :: Parser MSType
tmapping = do
  reserved "mapping"
  (kType, vType) <- parens types
  return $ TMapping kType vType
  where 
    types = do
      kType <- msType
      reserved "=>"
      vType <- msType
      return (kType, vType)

-- values 
msValue :: Parser MSValue
msValue =
  try vint
  <|> try vbool
  <|> vstring

vint :: Parser MSValue
vint = toPVInt <$> integer
  where
    toPVInt x = VInt $ fromInteger x

vstring :: Parser MSValue
vstring = do
  syms <- between (sym "\"") (sym "\"") (many (letter <|> digit <|> oneOf " .,;:!?()[]{}"))
  return $ VString syms

vbool :: Parser MSValue
vbool =
  try vtrue
  <|> vfalse
  where
    vtrue = VBool True <$ reserved "true"
    vfalse = VBool False <$ reserved "false"


-- expressions
msExpr :: Parser MSExpr
msExpr = Ex.buildExpressionParser table msTerm

msTerm :: Parser MSExpr
msTerm =
--  eCall
  try (parens msExpr)
  <|> try eArrayInit
  <|> try eCall
  <|> try eIndex
  <|> try eValue
  <|> eName

table = [ [ unIntOp "-" UnMinus,
            unBoolOp "!" Negate]
        , [ leftAsocBinOp "*" Mul,
            leftAsocBinOp "/" Div,
            leftAsocBinOp "%" Mod]
        , [ leftAsocBinOp "+" Add,
            leftAsocBinOp "-" Minus]
        , [ compOp "==" Eq,
            compOp ">" Gr,
            compOp "<" Le,
            compOp ">=" Gre,
            compOp "<=" Leq,
            compOp "!=" Neq]
        , [ leftAsocBinOp "&&" And ]
        , [ leftAsocBinOp "||" Or ]
        ]

binary name fun = Ex.Infix   ( do { reservedOp name; return fun } )
prefix name fun = Ex.Prefix  ( do { reservedOp name; return fun } )
postfix name fun = Ex.Postfix ( do { reservedOp name; return fun } )

leftAsocBinOp s op = binary s (EBinOp op) Ex.AssocLeft
unIntOp  s op = prefix s (EUnOp op)

compOp s op = binary s (EBinOp op) Ex.AssocNone

unBoolOp s op = prefix s (EUnOp op)

-- stack build --profile && stack exec --profile -- MSI-exe ./tests/Operations/contract.sol +RTS -p

eValue :: Parser MSExpr
eValue = EValue <$> msValue

eName :: Parser MSExpr
eName = EName <$> try identifier

eCall :: Parser MSExpr
eCall = do
    id <- try identifier
    args <- parens $ commaSep msExpr
    return $ ECall id args

eIndex :: Parser MSExpr
eIndex = do
  id <- try identifier
  index <- many $ brackets msExpr
  return $ buildExpr $ reverse $ EName id:index
  where
    buildExpr [x]    = x
    buildExpr (x:xs) = EBinOp Index x (buildExpr xs)
    buildExpr []     = undefined

eArrayInit :: Parser MSExpr
eArrayInit = do
  t <- msType
  case t of
    TArray t -> do
      vs <- braces $ commaSep msExpr
      return $ EArrayInit t vs
    _ -> fail "Array initialization requires an array type" -- TODO: better error handling

-- top level statements

msTopLevelStatement :: Parser MSTopLevelStatement
msTopLevelStatement =
  try sTopLevelVarDecl
  <|> try sConstantDecl
  <|> sFunctionDecl

sTopLevelVarDecl :: Parser MSTopLevelStatement
sTopLevelVarDecl = do
  t <- msType
  visibility <- msVisibility
  id <- identifier
  init <- optionMaybe varInit
  semi
  return $ STopLevelVarDecl t visibility id init
  where
    varInit = reserved "=" *> msExpr 

sConstantDecl :: Parser MSTopLevelStatement
sConstantDecl = do
  t <- msType
  visibility <- msVisibility
  id <- identifier
  v <- msValue
  semi
  return $ SConstantDecl t visibility id v

sFunctionDecl :: Parser MSTopLevelStatement
sFunctionDecl = do
  reserved "function"
  id <- identifier
  args <- parens $ commaSep arg
  visibility <- msVisibility
  t <- optionMaybe retType
  body <- braces $ many msBlockStatement
  let f = MSFunction { funcVisibility = visibility, argsSignature = args, retType = t, body = body}
  return $ SFunctionDecl id f
  where
    arg = do
      t <- msType
      id <- identifier
      return (t, id)
    retType = do
      reserved "returns"
      parens msType

sBreak :: Parser MSStatement
sBreak = SBreak <$ reserved "break"

sContinue :: Parser MSStatement
sContinue = SContinue <$ reserved "continue"

-- statements

msStatement :: Parser MSStatement
msStatement =
  try sFor
  <|> try sWhile
  <|> try sLocalVarDecl
  <|> try sAssign
  <|> try sIndexAssign
  <|> try sRequire
  <|> try sIf
  <|> try sBreak
  <|> try sContinue
  <|> try psReturn
  <|> sExpr

msBlockStatement :: Parser MSStatement
msBlockStatement =
  try sFor
  <|> try sWhile
  <|> try (sLocalVarDecl <* semi)
  <|> try (sAssign <* semi)
  <|> try (sIndexAssign <* semi)
  <|> try (sRequire <* semi)
  <|> try sIf
  <|> try (sBreak <* semi)
  <|> try (sContinue <* semi)
  <|> try (psReturn <* semi)
  <|> (sExpr <* semi)

sExpr :: Parser MSStatement
sExpr = SExpr <$> msExpr

sFor :: Parser MSStatement
sFor = do
  reserved "for"
  skip "("
  init <- localVarDecl
  semi
  condition <- msExpr
  semi
  delta <- msStatement
  skip ")"
  body <- braces $ many msBlockStatement
  return $ SFor init condition delta body

sWhile :: Parser MSStatement
sWhile = do
  reserved "while"
  skip "("
  condition <- msExpr
  skip ")"
  body <- braces $ many msBlockStatement
  return $ SWhile condition body

localVarDecl :: Parser MSLocalVarDecl
localVarDecl = do
  t <- msType
  id <- identifier
  sym "="
  e <- msExpr
  return $ LocalVarDecl t id e

sLocalVarDecl :: Parser MSStatement
sLocalVarDecl = SLocalVarDecl <$> localVarDecl

sAssign :: Parser MSStatement
sAssign = do
  i <- try identifier
  sym "="
  e <- msExpr
  return $ SAssign i e

sIndexAssign :: Parser MSStatement
sIndexAssign = do
  i <- try identifier
  is <- many $ brackets msExpr
  sym "="
  e <- msExpr
  return $ SIndexAssign i is e

sRequire :: Parser MSStatement
sRequire = do
  reserved "require"
  skip "("
  condition <- msExpr
  _ <- comma
  message <-msExpr
  skip ")"
  return $ SRequire condition message

sIf :: Parser MSStatement
sIf = do
  ifB <- ifBranch
  elifBs <- many $ try elifBranch
  elseB <- optionMaybe elseBranch
  return $ SIf ifB elifBs elseB
  where 
    ifBranch = do
      reserved "if"
      condition <- parens msExpr
      body <- braces $ many msBlockStatement
      return (condition, body)
    elifBranch = do
      reserved "else"
      reserved "if"
      condition <- parens msExpr
      body <- braces $ many msBlockStatement
      return (condition, body)
    elseBranch = do
      reserved "else"
      braces $ many msBlockStatement

psReturn :: Parser MSStatement
psReturn = do
  reserved "return"
  SReturn <$> msExpr

-- helpers
skip :: String -> Parser ()
skip x = do
  _ <- sym x
  return ()

-- program
program :: Parser MSProgram
program = do
  reserved "contract"
  name <- identifier
  declarations <- braces $ many msTopLevelStatement
  eof
  return $ MSProgram { name = name, declarations = declarations }

parseProgram :: String -> Either ParseError MSProgram
parseProgram = parse program ""

parseReplStatement :: String -> Either ParseError MSStatement
parseReplStatement = parse p ""
  where
    p =
      try (sAssign <* eof)
      <|> sExpr <* eof
