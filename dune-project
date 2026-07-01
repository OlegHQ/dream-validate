(lang dune 3.22)

(name dream-validate)

(generate_opam_files true)

(package
 (name dream-validate)
 (synopsis "Typed Dream request decoding and declarative DTO validation")
 (description
  "Small OCaml library and PPX deriver for typed Dream form/query/JSON DTO decoding with declarative validation rules.")
 (homepage "https://github.com/OlegHQ/dream-validate")
 (bug_reports "https://github.com/OlegHQ/dream-validate/issues")
 (license MIT)
 (authors OlegHQ)
 (maintainers OlegHQ)
 (depends
  (ocaml (>= 5.2))
  (dune (>= 3.22))
  dream
  yojson
  ppxlib
  (alcotest :with-test)))
