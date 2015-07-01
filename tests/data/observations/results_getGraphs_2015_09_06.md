Result of the getGraphs() unit test on current implementation_inheritance
=========================================================================

```
--- Testing getGraphs() ---
Some samples could not be matched.

           Sample no      In Sample only  Internal only           Both
                   0                   8              8              0
                   1                   6              6              0
                   2                   7              7              5
                   3                  27             27              7
                   4                   2              2              2
                   5                  12             12              0
                   6                   4              4              0
                   7                  13             13              0
                   9                   8              8              0
                  10                  12             12              0
                  11                  33             33              1
                  12                   8              8              0
                  13                   2              2              2
                  14                   4              4              0
                  15                   6              6              5
                  16                   6              6              0
                  17                  10             10              1
                  18                  88             88              0
                  19                   9              9              3
                  20                  18             18              0
                  21                  10             10              0
                  22                   7              7              1
                  23                  13             13              0
                  24                   8              8              0
                  25                   2              2              5
                  27                   8              8              0
                  28                   6              6              0
                  29                   7              7              4
                  30                  86             86              2
                  31                   6              6              0
                  32                   6              6              0
                  33                   8              8              0
                  34                  13             13              0
                  35                   1              1              1
                  36                   5              5              1
                  37                   8              8              0
                  38                   1              1              1
                  39                   8              8              2
                  40                   4              4              0
                  41                   7              7              1
                  42                  16             16              0
                  43                  12             12              0
                  44                  10             10              1
                  45                   8              8              0
                  46                   4              4              0
                  47                  16             16              0
                  49                  15             15              3
                  50                   8              8              2
                  51                  10             10              3
                  53                  12             12              0

    Total:        50                 608            608             53
```

=> Its reasonable to assume that the current Graph.isIsomorphic() implementation
goes wrong as soon as the edge order changes. Needs investigation!
