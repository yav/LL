{
{-# LANGUAGE TemplateHaskell #-}
module Lexer where

import AlexTools
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
  ","                 { lexeme KW_comma }
  "data"              { lexeme KW_data }

  $upper $ident_next* { lexeme VAR }
  $lower $ident_next* { lexeme CON }

  "--" .*             { return [] }
  $white+             { return [] }

  .                   { lexeme BAD }

{
alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte = makeAlexGetByte $ \c -> fromIntegral (min 127 (fromEnum c))

lexer :: Input -> [Lexeme Token]
lexer = $makeLexer simpleLexer

data Token =
    KW_eq
  | KW_arr
  | KW_open_brace
  | KW_close_brace
  | KW_open_paren
  | KW_close_paren
  | KW_semi
  | KW_colon
  | KW_comma
  | KW_data

  | VAR
  | CON

  | BAD
}

