module Syntax where

import Data.Text (Text)

data Decl       = DData Name [Name] [DataCon]
                | DNamed Name Expr
                  deriving Show

data Name       = Name Text
                  deriving (Eq,Ord,Show)

data Expr       = EId     TypedName TypedName
                | ECon    TypedName Name [TypedName]
                | ECase0  TypedName Name [ TypedName ]
                | ECase   TypedName [ (Pat,Expr) ]
                | ECut    TypedName Expr TypedName Expr
                | ENamed  Name
                  deriving Show

data TypedName  = TypedName { name :: Name, signature :: Maybe Type }
                  deriving Show

data Pat        = PCon Name [TypedName]
                  deriving Show

data Type       = TVar Name
                | TCon Name [Type]
                | TNot Type
                  deriving Show

data DataCon    = DataCon Name [Type]
                  deriving Show



