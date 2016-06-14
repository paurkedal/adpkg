# Adpkg - Transitory Additions to Topkg

Adpkg provides additions to the [Topkg](http://erratique.ch/software/topkg)
package system for OCaml.  In particular it adds functionality to work with
module lists, to avoid duplicating entries across `*.mllib` and `*.odocl`
files and the `~api` argument of `Topkg.mllib`.

As Topkg evolves, this project should no longer be needed.  And yes, that
makes this package transitory to the second order, as topkg is already said
to be transitory.
