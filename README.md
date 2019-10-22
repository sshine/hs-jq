# hs-jq

The purpose of this project is to create a competing implementation of
[`jq`][1], the command-line JSON processor.

## Check it out

There is currently no executable. To see the tests that pass you can:

```
$ git clone https://github.com/sshine/hs-jq.git
$ cd hs-jq
$ stack test
```

You can also experiment with the parser manually using GHCi:

```
$ stack ghci
> parseExpr "def map(f): [ .[] | f ]; map(.foo)"
Right (FuncDef "map" [FilterParam "f"]
        (List [Pipe (ValueIterator Identity) (FilterCall "f" Nothing)])
        (FilterCall "map" (Just [DotField "foo"])))
```

## Why?

 - I think `jq` is a really cool domain-specific language, but I'm really bad
   at it.

   Maybe if I write an interpreter for it, I will become better at it.
 - This implementation can
    - confirm that the original `jq` implementation works as intended.
    - document the original `jq` syntax [as documented][8] in [JBOL][7].
 - Writing property-based tests in Haskell is a lot more convenient than
   doing so in C. This will give me an opportunity to experiment with
   [hedgehog][2]. For example, I might like to write a generator of JSON
   values where lists of objects share the same structure (reflecting JSON
   documents that `jq` is suitable to transform), and given such a JSON
   value, I might like to write a generator of `jq` expressions that validly
   address such a value.
 - It would be really cool if you could transform [Aeson][3] `Value`s using a
   [quasi-quoter][4]. This would give me a chance to explore how quasi-quoters
   work in Haskell.

   ```haskell
   slugs :: Value -> Either JqError Value
   slugs = [jq| [ .exercises[] | .slug ] |]
   ```

## Comparable projects

 - [`stedolan/jq`][1]: "Command-line JSON processor"
 - [`chrisdone/jl`][5]: "Functional sed for JSON"
 - [`haskell-works/hw-jq`][6]: From 2017, appears very similar to this project.

## Status

Parser: 181 out of 580 tests failed.

## Contributing

If you like this project, or if you're looking for an opportunity to
practice writing open source Haskell, feel free to open an issue or a PR, or
send me an email. A good starting point could be to look through issues
labelled "[help wanted][9]".

[1]: https://github.com/stedolan/jq
[2]: http://hackage.haskell.org/package/hedgehog
[3]: http://hackage.haskell.org/package/aeson
[4]: https://wiki.haskell.org/Quasiquotation
[5]: https://github.com/chrisdone/jl
[6]: https://github.com/haskell-works/hw-jq
[7]: https://github.com/fadado/JBOL
[8]: https://github.com/fadado/JBOL/blob/master/doc/JQ-language-grammar.md
[9]: https://github.com/sshine/hs-jq/issues?q=is%3Aopen+is%3Aissue+label%3A%22help+wanted%22
