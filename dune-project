(lang dune 3.4)

(name necroml)
(version 2.9)

(generate_opam_files true)

(authors "Alan Schmitt" "Louis Noizet")
(maintainers "Louis Noizet")

(bug_reports https://gitlab.inria.fr/skeletons/necro-ml/-/issues)
(homepage https://skeletons.gitlabpages.inria.fr/necro-website)
(documentation https://skeletons.gitlabpages.inria.fr/necro-website/manual.html)

(package
   (name necroml)
   (synopsis "Generate Interpreters from Skeletal Semantics")
   (description "Necro ML allows to generate an OCaml interpreter from a \
                skeletal semantics.")
   (depends (ocaml (>= 4.10.0))
            (necrolib (and (>= 0.11) (< 0.12)))
            dune-build-info)
   )

