# My 'learning [OCaml](https://ocaml.org/)' repo!

Nothing to see here... Move along.

## Opchecker

given a pair of characters `fst` and `snd`, checks that
provided text conforms to the following two conditions:

1. The number of appearances of both `fst` and `snd` must be equal.
2. The number of `snd` read never exceeds the number of `fst` read when scanning from left to right.

Defaults:
- `fst = (`
- `snd = )`

Use with defaults:
- `ocaml opchecker.ml "text with spaces"`
- `ocaml opchecker.ml text_with_no_spaces`

Use with custom character pair (*braces*): 
- `ocaml opchecker.ml { } "some text here"`
