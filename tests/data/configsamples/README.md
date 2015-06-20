Configuration samples
=====================

This directory contains text files. Each describes a configuration,
all of its contained graphs (one representative per isomorphic group),
a set of known induced subgraphs and all automorphisms (permutations
of the configurations' nodes which generate the same representatives).
The automorphisms cover only the case that optional edges are mapped
to optional edges.

The sample files were generated using the old implementation and are
therefore considered valid.

Structure of a sample:
----------------------

{number of nodes} [name] n0 - n1; n2 = n3; n4 - n5 ...  
-- contains:
([n0, n1, ...], [{n0,n1}, {nk,nz}, ...])  
...  
([n2, n3, ...], [{n2,n3}, ...])  
-- induces:
([n5, n6, ...], [{n5,n6}, {ni,nj}, ...])  
...  
([n7, ...], [{...}, ...])
-- automorphisms:
[n4, n8, n1, ...]
...
[n0, n7, n6, ...]

where

  - *n0, n1, ...* are the nodes of the graphs
  - *n0 - n1* denotes a normal edge
  - *n2 = n3* denotes an optional edge.

The string *"contains"* is used as a separator between the
configuration and its representatives and *"induces"* between
the representatives and the induced subgraphs.
Both strings must be included.
