
module Data.Aeson.Jq.Expr
  ( Expr(..)
  , Term(..)
  , Pattern(..)
  ) where

import Data.Scientific (Scientific)
import Data.Text (Text)

{-

The jq grammar: https://github.com/stedolan/jq/blob/master/src/parser.y
A derived BNF: https://github.com/fadado/JBOL/blob/master/doc/JQ-language-grammar.md

Initially, only look at expressions and skip jq modules, function defs and
imperative constructs. You know, I didn't even know that jq had support for
modules until now. I'm learning!

Addressed:

$ jq '.[.bar]' <<< '{ "foo": 42, "bar": "foo" }'
42

$ jq '.foo[.bar]' <<< '{ "foo": [3,6,9], "bar": 2 }'
9

Not addressed yet:

$ jq '.foo[.bar + 1]' <<< '{ "foo": [3,6,9], "bar": 0 }' # arithmetic
6

$ jq '.foo.bar'  <<< '{ "foo": { "bar": 42 } }' # syntax sugar for '.foo | .bar'
42

-}

data Expr = Term !Term
          | As !Pattern
          | ListLit ![Expr]
          | StrLit !Text
          | NumLit !Scientific
          | BoolLit !Bool
          | NullLit
          deriving (Eq, Show)

-- TODO: Perhaps keep a parameterized AST so that we can represent the
-- syntax tree exactly as it was parsed, i.e. distinguish between the two:
-- .foo and .["foo"], in one form of the tree, and in another form make such
-- syntax sugar unexpressable. The same goes for .foo.bar -> .foo | .bar.

data Term = Identity   -- .
          | Index !Expr -- .foo, .["foo"], .[1], .[.foo]
          deriving (Eq, Show)

data Pattern = Ident !Text -- '... as $var'
             deriving (Eq, Show)
