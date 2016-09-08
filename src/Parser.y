{
module Parser
  ( parseExpr
  , parseType
  , parseDecls
  ) where

import AlexTools
import Lexer
import Syntax
}


%token
  '='     { Lexeme { lexemeToken = KW_eq           } }
  '->'    { Lexeme { lexemeToken = KW_arr          } }
  '{'     { Lexeme { lexemeToken = KW_open_brace   } }
  '}'     { Lexeme { lexemeToken = KW_close_brace  } }
  '('     { Lexeme { lexemeToken = KW_open_paren   } }
  ')'     { Lexeme { lexemeToken = KW_close_paren  } }
  ';'     { Lexeme { lexemeToken = KW_semi         } }
  ':'     { Lexeme { lexemeToken = KW_colon        } }
  '|'     { Lexeme { lexemeToken = KW_bar          } }
  '^'     { Lexeme { lexemeToken = KW_hat          } }
  'data'  { Lexeme { lexemeToken = KW_data         } }

  VAR     { Lexeme { lexemeToken = VAR             } }
  CON     { Lexeme { lexemeToken = CON             } }
  EVAR    { Lexeme { lexemeToken = EVAR            } }

%name parseExpr  expr
%name parseType  type
%name parseDecls decls

%tokentype  { Lexeme Token }
%error      { parseError   }
%monad      { Either String }


%%
expr                                     :: { Expr }
  : evar_name                               { ENamed $1 }
  | var '=' var                             { EId  $1 $3 }
  | var '=' con_name vars                   { ECon $1 $3 $4 }
  | var '=' con_name '{' vars '}'           { ECase0 $1 $3 $5 }
  | var '=' '{' alts1 '}'                   { ECase $1 $4 }
  | '{' var '->' expr ';'
        var '->' expr '}'                   { ECut $2 $4 $6 $8 }

alts1                                    :: { [(Pat,Expr)] }
  : alt                                     { [$1]    }
  | alt ';' alts1                           { $1 : $3 }

alt                                      :: { (Pat,Expr) }
  : con_name vars '->' expr                 { (PCon $1 $2, $4) }

var_name                                 :: { Name }
  : VAR                                     { Name (lexemeText $1) }

var_names                                :: { [Name] }
  :                                         { [] }
  | var_names1                              { $1 }

var_names1                               :: { [Name] }
  : var_name                                { [$1] }
  | var_name var_names1                     { $1 : $2 }

con_name                                 :: { Name }
  : CON                                     { Name (lexemeText $1) }

evar_name                                :: { Name }
  : EVAR                                    { Name (lexemeText $1) }

var                                      :: { TypedName }
  : var_name                                { TypedName $1 Nothing }
  | '(' var_name ':' type ')'               { TypedName $2 (Just $4) }

vars                                     :: { [TypedName] }
  :                                         { [] }
  | vars1                                   { $1 }

vars1                                    :: { [TypedName] }
  : var                                     { [$1]  }
  | var vars1                               { $1 : $2 }

type                                     :: { Type }
  : atype                                   { $1 }
  | con_name atypes1                        { TCon $1 $2 }

atypes1                                  :: { [Type] }
  : atype atypes1                           { $1 : $2 }
  | atype                                   { [$1] }

atypes                                   :: { [Type] }
  :                                         { [] }
  | atypes1                                 { $1 }

atype                                    :: { Type }
  : con_name                                { TCon $1 [] }
  | var_name                                { TVar $1 }
  | '(' type ')'                            { $2 }
  | atype '^'                               { TNot $1 }

decl                                     :: { Decl }
  : data_head                               { $1 [] }
  | data_head '=' data_cons1                { $1 $3 }
  | evar_name '=' expr                      { DNamed $1 $3 }

decls                                    :: { [Decl] }
  :                                         { [] }
  | decls1                                  { $1 }

decls1
  : decl                                    { [$1] }
  | decl ';' decls1                         { $1 : $3 }


data_head                                :: { [DataCon] -> Decl }
  : 'data' con_name var_names               { DData $2 $3 }

data_con                                 :: { DataCon }
  : con_name atypes                         { DataCon $1 $2 }

data_cons1                               :: { [DataCon] }
  : data_con                                { [$1] }
  | data_con '|' data_cons1                 { $1 : $3 }


{
parseError :: [Lexeme Token] -> Either String a
parseError x = Left ("Parse error at " ++ loc)
  where
  loc = case x of
          []    -> "end of file."
          l : _ -> prettySourceRange (lexemeRange l)
}
