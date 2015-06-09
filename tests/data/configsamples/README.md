Configuration samples
=====================

This directory contains text files. Each describes a configuration,
all of its contained graphs (one representative per isomorphic group)
and a set of known induced subgraphs.

The sample files were generated using the old implementation and are
therefore considered valid.

Structure of a sample:
----------------------

{number of nodes} [name] n0 - n1; n2 = n3; n4 - n5 ...  
contains  
([n0, n1, ...], [{n0,n1}, {nk,nz}, ...])  
...  
([n2, n3, ...], [{n2,n3}, ...])  
induces
([n5, n6, ...], [{n5,n6}, {ni,nj}, ...])  
...  
([n7, ...], [{...}, ...])

where

  - *n0, n1, ...* are the nodes of the graphs
  - *n0 - n1* denotes a normal edge
  - *n2 = n3* denotes an optional edge.

The string *"contains"* is used as a separator between the
configuration and its representatives and *"induces"* between
the representatives and the induced subgraphs.
Both strings must be included.
