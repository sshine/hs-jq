
module Data.Aeson.Jq.Expr
  ( Expr(..)
  , Term(..)
  , ObjElem(..)
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

type Ident = Text

data Expr = Term !Term
          | As !Pattern
          | Obj ![ObjElem]
          | List ![Expr]
          | StrLit !Text
          | NumLit !Scientific
          | BoolLit !Bool
          | NullLit
          deriving (Eq, Show)

-- TODO: After asking Athas, I've decided to make two ASTs so that we can
-- represent the syntax tree exactly as it was parsed, i.e. distinguish
-- between the two: .foo and .["foo"], in one form of the tree, and in
-- another form make such syntax sugar unexpressable. The same goes for
-- .foo.bar -> .foo | .bar.

-- TODO: Find out why Var is a Term and not an Expr.

data Term = Identity   -- .
          | Index !Expr -- .foo, .["foo"], .[1], .[.foo]
          | Var Ident
          deriving (Eq, Show)

-- This is what JBOL's BNF calls MkDictPair.
--
-- I don't quite understand this part of the grammar:
--   Keyword ':' ExpD
--
-- Explanation:
--
-- jq supports {foo: "bar"} as shorthand for {"foo": "bar"}, and this
-- naturally extends to keywords. So the Keyword part of the BNF makes this
-- explicit.
--
-- The grammar only allows a subset of expressions, ExpD, to avoid ambiguity
-- in at least ':' and ',': Both of these can occur in expressions, but they
-- also occur as part of dictionary syntax.
--
-- For now, simplify things and deal with ambiguities in the parser.
data ObjElem = ObjPair Expr (Maybe Expr)
             deriving (Eq, Show)

-- TODO: Split this into ObjExprPair, ObjIdentPair, singles, etc.

data Pattern = Ident !Text -- '... as $var'
             deriving (Eq, Show)
