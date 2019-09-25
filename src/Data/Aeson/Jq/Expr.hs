
module Data.Aeson.Jq.Expr where

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

Note: 'as' is only available in subsequent expressions:

$ jq -n '[ 1 as $answer | 2 ] | $answer' # fails

TODO: Catch this in type-checking phase.

Not addressed yet:

$ jq '.foo[.bar + 1]' <<< '{ "foo": [3,6,9], "bar": 0 }' # arithmetic
6

$ jq '.foo.bar'  <<< '{ "foo": { "bar": 42 } }' # syntax sugar for '.foo | .bar'
42

-}

type Ident = Text

data Expr
      -- Function definitions
    = FuncDef !Ident !(Maybe Params) Expr

      -- Control structures
    | As !Expr !Pattern
    | Reduce !Expr !Pattern !Expr !Expr
    | Foreach !Expr !Pattern !Expr !Expr !(Maybe Expr)
    | If ![(Expr, Expr)] !Expr
    | TryCatch !Expr !(Maybe Expr)
    | Label !Ident
    | Break Ident                    -- 'break' $ IDENT (JBOL Term)

      -- Operators
    | Assign !Expr !Expr             -- '='
    | Or !Expr !Expr                 -- 'or'
    | And !Expr !Expr                -- 'and'
    | Alternative !Expr !Expr        -- '//'
    | AlternativeAssign !Expr !Expr  -- '//='
    | UpdateAssign !Expr !Expr       -- '|='
    | Pipe !Expr !Expr               -- '|'
    | Comma !Expr !Expr              -- ','
    | Plus !Expr !Expr               -- '+'
    | PlusAssign !Expr !Expr         -- '+='
    | Minus !Expr !Expr              -- '-'
    | MinusAssign !Expr !Expr        -- '-='
    | Mult !Expr !Expr               -- '*'
    | MultAssign !Expr !Expr         -- '*='
    | Div !Expr !Expr                -- '/'
    | DivAssign !Expr !Expr          -- '/='
    | Mod !Expr !Expr                -- '%'
    | ModAssign !Expr !Expr          -- '%='
    | Eq !Expr !Expr                 -- '=='
    | Neq !Expr !Expr                -- '!='
    | Lt !Expr !Expr                 -- '<'
    | Gt !Expr !Expr                 -- '>'
    | Leq !Expr !Expr                -- '<='
    | Geq !Expr !Expr                -- '>='

      -- Terms

      -- Prefix/postfix
    | Optional !Expr                 -- suffix '?'
    | Neg !Expr                      -- unary '-'
    | DotFieldAfter !Expr !Ident     -- suffix '.foo'
    | DotStrAfter !Expr !Text        -- suffix '."foo"'
 -- | DotExp !Exp !Exp               -- suffix '.[e]' not supported by jq,
                                     --   e.g. jq -n '{"foo":42}.["foo"]'
      -- Begins with '.'
    | Identity                       -- '.'
    | RecursiveDescent               -- '..'
    | DotField !Ident                -- '.foo'
    | DotStr !Text                   -- '."foo"'
    | DotExpr !Expr                  -- '.[e]'

      -- Postfix indexing with []
    | IndexAfter !(Maybe Expr) !Expr -- 'x[y]'
    | IndexRangeAfter !Expr !(Maybe Expr) !(Maybe Expr)
                                     -- postfix 'e[x:y]', 'e[x:]', 'e[:y]', 'e[:]'

      -- Literals, variables
    | Var Ident                      -- '$var' ([a-zA-Z_][a-zA-Z_0-9]*::)*[a-zA-Z_][a-zA-Z_0-9]*
    | Obj ![(ObjKey, Maybe Expr)]      -- This is what JBOL's grammar calls MkDictPair
    | List ![Expr]
    | StrLit !Text
    | NumLit !Scientific
    | BoolLit !Bool
    | NullLit

      -- Explicit parentheses
    | Paren !Expr
    deriving (Eq, Show)

data ObjKey = FieldKey !Ident
            | FieldExpr !Expr
            deriving (Eq, Show)

data Params = IdentParam !Ident      -- '$' ident
            | FilterParam !Ident     -- e.g. 'def foo(f): f | f'
            deriving (Show, Eq)

data Pattern = Ident !Text -- '... as $var'
             deriving (Eq, Show)
