# puricaml

`puricaml` is a tiny OCaml compiler.

# build

## depends

- ocaml
- opam
- dune
- menhir

## run

```sh
git clone https://github.com/puripuri2100/puricaml.git
cd puricaml
dune build
```

# run

```sh
dune exec -- puricaml example/quicksort.mml
dune exec -- puricaml example/quicksort.zam -r -z
dune exec -- puricaml example/quicksort.cam -r -c
```

# References

- [[S-8] 関数プログラミング](https://www.logic.cs.tsukuba.ac.jp/jikken/index.html)

---

LICENSE: MIT
(c) 2025 puripuri2100