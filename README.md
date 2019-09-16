# hs-jq

The purpose of this project is to create a competing implementation of
[`jq`][1], the command-line JSON processor.

Why?

 - I think `jq` is a really cool domain-specific language, but I'm really bad
   at it.

   Maybe if I write an interpreter for it, I will become better at it.
 - It may help the original `jq` project by asserting equivalent behavior.  So
   while nobody may use this implementation, its output can confirm that the
   original implementation works.
 - Writing property-based tests in Haskell is a lot more convenient than doing
   so in C. This will give me a chance to experiment with [hedgehog][2] and
   write some non-trivial generators and properties. For example, I might like
   to write a generator of JSON values where lists of objects share the same
   structure (reflecting JSON documents that `jq` is suitable to transform),
   and given such a JSON value, I might like to write a generator of `jq`
   expressions that validly address such a value.
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

Conceptual.

[1]: https://github.com/stedolan/jq
[2]: http://hackage.haskell.org/package/hedgehog
[3]: http://hackage.haskell.org/package/aeson
[4]: https://wiki.haskell.org/Quasiquotation
[5]: https://github.com/chrisdone/jl
[6]: https://github.com/haskell-works/hw-jq
