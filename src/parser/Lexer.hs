module Lexer where

import Ast

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Prim (many)

import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = [ "+", "*", "-", "/", "%"
          , "<", ">", ">=", "<=", "==", "!=" ]
    names = [
      "public",
      "private",
      "true", 
      "false", 
      "function",
      "require",
      "const",
      "return",
      "returns",
      "int", 
      "bool",
      "mapping",
      "if",
      "else",
      "=>",
      "for",
      "while",
      "break",
      "continue"
      ]
    style = emptyDef {
               Tok.commentLine = "//"
             , Tok.commentStart = "/*"
             , Tok.commentEnd = "*/"
             , Tok.caseSensitive = True
             , Tok.reservedOpNames = ops
             , Tok.reservedNames = names
             }

integer    = Tok.integer lexer
float      = Tok.float lexer
parens     = Tok.parens lexer
braces     = Tok.braces lexer
brackets   = Tok.brackets lexer
commaSep   = Tok.commaSep lexer
semiSep    = Tok.semiSep lexer
identifier = Tok.identifier lexer
whitespace = Tok.whiteSpace lexer
reserved   = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
semi       = Tok.semi lexer
sym        = Tok.symbol lexer
comma      = Tok.comma lexer


