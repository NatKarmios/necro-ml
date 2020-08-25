# Package OPAM necro-ml
At this point the necro-tex package is not released in the official OPAM repository.

## Installation and test with OPAM
```
opam switch create necro 4.09.0
eval $(opam env)

opam repository add necro https://gitlab.inria.fr/skeletons/opam-repository.git#multirep

opam install -y necro-ml
```

## Maintenance
WARNING : The _opam-repository_ should be in the parent folder of this one (_../opam-repository_).
Here is the worklow to update the tarball:
1. Commit and push your change.
2. Delete the _VERSION_ tag, and create it again from _multirep_ branch (or _master_ when merged).
3. Run ```make clean; autoconf; ./configure; make opam_file_update```
4. Commit and push _opam-repository/package/necrolib/necrolib.VERSION/opam_
5. Test deployment using the _Installation and test with OPAM_ script.
