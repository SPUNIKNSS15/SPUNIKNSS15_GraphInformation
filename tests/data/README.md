Configuration samples
=====================

This directory contains text files. Each describes a configuration and
all of its contained graphs (one representative per isomorphic group).

Two subdirectories exist:

  - *oldimpl*: files which were generated using the old
                 implementation. **Samples from here are considered valid.**
  - *newimpl*: files which were generated using the new implementation.
                 Should be used only to compare the results with the old
                 implementation.

Structure of a sample:
----------------------

{number of nodes} [name] n0 - n1; n2 = n3; n4 - n5 ...  
All contained graphs:  
([n0, n1, ...], [{n0,n1}, {nk,nz}, ...])  
...  
([n2, n3, ...], [{n2,n3}, ...])  

where

  - *n0, n1, ...* are the nodes of the graphs
  - *n0 - n1* denotes a normal edge
  - *n2 = n3* denotes an optional edge.

The string *"All contained graphs:"* is used as a separator between the
configuration and its representatives and must be included.
