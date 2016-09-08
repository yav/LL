{
{-# LANGUAGE TemplateHaskell #-}
module Lexer where

import AlexTools
import Data.Text(Text)
}

$upper      = [A-Z]
$lower      = [a-z_]
$ident_next = [A-Za-z0-9_]

tokens :-
  "="                 { lexeme KW_eq }
  "->"                { lexeme KW_arr }
  "{"                 { lexeme KW_open_brace }
  "}"                 { lexeme KW_close_brace }
  "("                 { lexeme KW_open_paren }
  ")"                 { lexeme KW_close_paren }
  ";"                 { lexeme KW_semi }
  ":"                 { lexeme KW_colon }
  "data"              { lexeme KW_data }
  "|"                 { lexeme KW_bar }
  "^"                 { lexeme KW_hat }

  $upper $ident_next* { lexeme CON }
  $lower $ident_next* { lexeme VAR }
  "$" $ident_next*    { lexeme EVAR }

  "--" .*             { return [] }
  $white+             { return [] }

  .                   { lexeme BAD }

{
alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte = makeAlexGetByte $ \c -> fromIntegral (min 127 (fromEnum c))

lexer :: Text -> [Lexeme Token]
lexer txt = $makeLexer simpleLexer (initialInput txt)

data Token =
    KW_eq
  | KW_arr
  | KW_open_brace
  | KW_close_brace
  | KW_open_paren
  | KW_close_paren
  | KW_semi
  | KW_colon
  | KW_data
  | KW_bar
  | KW_hat

  | VAR
  | CON
  | EVAR

  | BAD
}

