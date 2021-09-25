(lang dune 2.8)

(name rtp)

(generate_opam_files true)

(source
 (github patricoferris/srtp))

(license MIT)

(authors "Patrick Ferris")

(maintainers "pf341@patricoferris.com")

(package
 (name rtp)
 (synopsis "Pure OCaml Real-time Transport Protocol")
 (description
  "A pure OCaml implementation of the Real-time Transport Protocol")
 (depends
  (alcotest :with-test)
  cstruct))

(package
 (name rtcp)
 (synopsis "Pure OCaml Real-time Transport Control Protocol")
 (description
  "A pure OCaml implementation of the Real-time Control Transport Protocol")
 (depends
  (alcotest :with-test)
  (rtp
   (= :version))
  cstruct))
