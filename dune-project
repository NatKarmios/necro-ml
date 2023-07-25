(lang dune 3.4)

(name necroml)
(version 0.2.11.4.1)

(generate_opam_files true)

(authors "Alan Schmitt" "Victoire Noizet")
(maintainers "Victoire Noizet")

(bug_reports https://gitlab.inria.fr/skeletons/necro-ml/-/issues)
(homepage https://skeletons.gitlabpages.inria.fr/necro-website)
(documentation https://skeletons.gitlabpages.inria.fr/necro-website/manual.html)

(package
   (name necroml)
   (synopsis "Generate Interpreters from Skeletal Semantics")
   (description "Necro ML allows to generate an OCaml interpreter from a \
                skeletal semantics.")
   (depends (ocaml (>= 4.10.0))
            (necrolib (and (>= 0.14.5) (< 0.15)))
            (necrotrans (>= 0.1))
            dune-build-info)
   )

