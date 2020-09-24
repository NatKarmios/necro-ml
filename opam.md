# Package OPAM necroml
At this point the necroml package is not released in the official OPAM repository.

## Installation and test with OPAM
```
opam switch create necro 4.09.0
eval $(opam env)

opam repository add necro https://gitlab.inria.fr/skeletons/opam-repository.git#necro

opam install -y necroml
```

## Maintenance
WARNING : The _opam-repository_ should be in the parent folder of this one (_../opam-repository_).
Here is the worklow to update the tarball:
1. Commit and push your change.
2. Run `make git_tag` to update or create the tag for the current version
3. Run `make clean; autoconf; ./configure; make opam_file_update`
4. Commit and push _opam-repository/package/necroml/necroml.VERSION/opam_
5. Test deployment using the _Installation and test with OPAM_ script.

## New version

To create a new version, simply type `make new_version`, the current version will be
display, and will be asked for the new version number
