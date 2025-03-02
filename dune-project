(lang dune 3.15)

(using menhir 2.1)

(name puricaml)

(license MIT)

(source (github puripuri2100/puricaml))

(package
 (name puricaml)
 (depends
  (dune (>= 3.15))
  (ocaml (>= 4.14.2))
  (menhir (>= 20231231))
  (menhirLib (>= 20231231))
 )
)

