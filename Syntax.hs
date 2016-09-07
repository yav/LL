module Syntax where

import Data.Text (Text)

data Name       = Name Text

data TypedName  = TypedName { name :: Name, signature :: Maybe Type }

data Expr       = EId     TypedName TypedName
                | ECon    TypedName Name [TypedName]
                | ECase0  TypedName [TypedName]
                | ECase   TypedName [ (Pat,Expr) ]
                | ECut    TypedName Expr TypedName Expr

data Pat        = PCon Name [TypedName]

data Type       = TVar Name
                | TCon Name [Type]



