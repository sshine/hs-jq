# hs-jq

[jq][1] is a command-line JSON processor. It is also a domain-specific
combinator language, which makes it easy to analyse and optimize. This
project aims to reimplement jq in Haskell, mostly because it's fun.

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

 - I think jq is a really cool domain-specific language, but I'm really bad
   at it.

   Maybe if I write an interpreter for it, I will become better at it.
 - This implementation will
    - confirm that the original jq implementation works as intended.
    - document the original jq syntax [as documented][8] in [JBOL][7].
    - prototype improvements to jq, such as syntax-preserving arithmetic,
      arbitrary-size numbers, fusion of combinators, and linting.
 - Writing property-based tests in Haskell is a lot more convenient than
   doing so in C. This will give me an opportunity to experiment with
   [Hedgehog][2].
 - It would be really cool if you could transform [Aeson][3] `Value`s using a
   [quasi-quoter][4]. This would give me a chance to explore how quasi-quoters
   work in Haskell.

   ```haskell
   slugs :: Value -> Either JqError Value
   slugs = [jq| .exercises | map(.slug) |]
   ```

## Comparable projects

 - [`stedolan/jq`][1]: "Command-line JSON processor"
 - [`chrisdone/jl`][5]: "Functional sed for JSON"
 - [`haskell-works/hw-jq`][6]: From 2017, appears very similar to this project.

## Status

Parser: 72 out of 653 tests failed.

## Contributing

If you like this project, or if you're looking for an opportunity to practice
writing open source Haskell, feel free to open an issue or a PR, or send me an
email. A good starting point could be to look through issues labelled "[help
wanted][9]".

In connection to Hacktoberfest 2019 I received 9 PRs. Thanks a bunch, guys!

[1]: https://github.com/stedolan/jq
[2]: http://hackage.haskell.org/package/hedgehog
[3]: http://hackage.haskell.org/package/aeson
[4]: https://wiki.haskell.org/Quasiquotation
[5]: https://github.com/chrisdone/jl
[6]: https://github.com/haskell-works/hw-jq
[7]: https://github.com/fadado/JBOL
[8]: https://github.com/fadado/JBOL/blob/master/doc/JQ-language-grammar.md
[9]: https://github.com/sshine/hs-jq/issues?q=is%3Aopen+is%3Aissue+label%3A%22help+wanted%22
