(lang dune 3.4)

(name necroml)
(version 2.8.0.1)

(generate_opam_files true)

(authors "Enzo Crance" "Nathanaëlle Courant" "Alan Schmitt" "Louis Noizet")
(maintainers "Louis Noizet")

(bug_reports https://gitlab.inria.fr/skeletons/necro/-/issues)
(homepage https://skeletons.gitlabpages.inria.fr/necro-website)
(documentation https://skeletons.gitlabpages.inria.fr/necro-website/manual.html)

(using menhir 2.0)

(package
   (name necroml)
   (synopsis "Generate Interpreters from Skeletal Semantics")
   (description "Necro ML allows to generate an OCaml interpreter from a \
                skeletal semantics.")
   (depends ocamlfind menhir (ocaml (>= 4.10.0))
            (necrolib (and (>= 0.10) (<= 0.11))))
   )

