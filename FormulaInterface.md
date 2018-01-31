An exploration of the formula interface
================
Pavan Gurazada
January 2018

``` r
library(tidyverse)

data("mpg")
data("iris")

attach(mpg)

class(fo <- hwy ~ class * displ)
```

    ## [1] "formula"

``` r
typeof(fo)
```

    ## [1] "language"

``` r
terms(fo)
```

    ## hwy ~ class * displ
    ## attr(,"variables")
    ## list(hwy, class, displ)
    ## attr(,"factors")
    ##       class displ class:displ
    ## hwy       0     0           0
    ## class     1     0           1
    ## displ     0     1           1
    ## attr(,"term.labels")
    ## [1] "class"       "displ"       "class:displ"
    ## attr(,"order")
    ## [1] 1 1 2
    ## attr(,"intercept")
    ## [1] 1
    ## attr(,"response")
    ## [1] 1
    ## attr(,".Environment")
    ## <environment: R_GlobalEnv>

``` r
model.frame(fo)
```

    ##     hwy      class displ
    ## 1    29    compact   1.8
    ## 2    29    compact   1.8
    ## 3    31    compact   2.0
    ## 4    30    compact   2.0
    ## 5    26    compact   2.8
    ## 6    26    compact   2.8
    ## 7    27    compact   3.1
    ## 8    26    compact   1.8
    ## 9    25    compact   1.8
    ## 10   28    compact   2.0
    ## 11   27    compact   2.0
    ## 12   25    compact   2.8
    ## 13   25    compact   2.8
    ## 14   25    compact   3.1
    ## 15   25    compact   3.1
    ## 16   24    midsize   2.8
    ## 17   25    midsize   3.1
    ## 18   23    midsize   4.2
    ## 19   20        suv   5.3
    ## 20   15        suv   5.3
    ## 21   20        suv   5.3
    ## 22   17        suv   5.7
    ## 23   17        suv   6.0
    ## 24   26    2seater   5.7
    ## 25   23    2seater   5.7
    ## 26   26    2seater   6.2
    ## 27   25    2seater   6.2
    ## 28   24    2seater   7.0
    ## 29   19        suv   5.3
    ## 30   14        suv   5.3
    ## 31   15        suv   5.7
    ## 32   17        suv   6.5
    ## 33   27    midsize   2.4
    ## 34   30    midsize   2.4
    ## 35   26    midsize   3.1
    ## 36   29    midsize   3.5
    ## 37   26    midsize   3.6
    ## 38   24    minivan   2.4
    ## 39   24    minivan   3.0
    ## 40   22    minivan   3.3
    ## 41   22    minivan   3.3
    ## 42   24    minivan   3.3
    ## 43   24    minivan   3.3
    ## 44   17    minivan   3.3
    ## 45   22    minivan   3.8
    ## 46   21    minivan   3.8
    ## 47   23    minivan   3.8
    ## 48   23    minivan   4.0
    ## 49   19     pickup   3.7
    ## 50   18     pickup   3.7
    ## 51   17     pickup   3.9
    ## 52   17     pickup   3.9
    ## 53   19     pickup   4.7
    ## 54   19     pickup   4.7
    ## 55   12     pickup   4.7
    ## 56   17     pickup   5.2
    ## 57   15     pickup   5.2
    ## 58   17        suv   3.9
    ## 59   17        suv   4.7
    ## 60   12        suv   4.7
    ## 61   17        suv   4.7
    ## 62   16        suv   5.2
    ## 63   18        suv   5.7
    ## 64   15        suv   5.9
    ## 65   16     pickup   4.7
    ## 66   12     pickup   4.7
    ## 67   17     pickup   4.7
    ## 68   17     pickup   4.7
    ## 69   16     pickup   4.7
    ## 70   12     pickup   4.7
    ## 71   15     pickup   5.2
    ## 72   16     pickup   5.2
    ## 73   17     pickup   5.7
    ## 74   15     pickup   5.9
    ## 75   17        suv   4.6
    ## 76   17        suv   5.4
    ## 77   18        suv   5.4
    ## 78   17        suv   4.0
    ## 79   19        suv   4.0
    ## 80   17        suv   4.0
    ## 81   19        suv   4.0
    ## 82   19        suv   4.6
    ## 83   17        suv   5.0
    ## 84   17     pickup   4.2
    ## 85   17     pickup   4.2
    ## 86   16     pickup   4.6
    ## 87   16     pickup   4.6
    ## 88   17     pickup   4.6
    ## 89   15     pickup   5.4
    ## 90   17     pickup   5.4
    ## 91   26 subcompact   3.8
    ## 92   25 subcompact   3.8
    ## 93   26 subcompact   4.0
    ## 94   24 subcompact   4.0
    ## 95   21 subcompact   4.6
    ## 96   22 subcompact   4.6
    ## 97   23 subcompact   4.6
    ## 98   22 subcompact   4.6
    ## 99   20 subcompact   5.4
    ## 100  33 subcompact   1.6
    ## 101  32 subcompact   1.6
    ## 102  32 subcompact   1.6
    ## 103  29 subcompact   1.6
    ## 104  32 subcompact   1.6
    ## 105  34 subcompact   1.8
    ## 106  36 subcompact   1.8
    ## 107  36 subcompact   1.8
    ## 108  29 subcompact   2.0
    ## 109  26    midsize   2.4
    ## 110  27    midsize   2.4
    ## 111  30    midsize   2.4
    ## 112  31    midsize   2.4
    ## 113  26    midsize   2.5
    ## 114  26    midsize   2.5
    ## 115  28    midsize   3.3
    ## 116  26 subcompact   2.0
    ## 117  29 subcompact   2.0
    ## 118  28 subcompact   2.0
    ## 119  27 subcompact   2.0
    ## 120  24 subcompact   2.7
    ## 121  24 subcompact   2.7
    ## 122  24 subcompact   2.7
    ## 123  22        suv   3.0
    ## 124  19        suv   3.7
    ## 125  20        suv   4.0
    ## 126  17        suv   4.7
    ## 127  12        suv   4.7
    ## 128  19        suv   4.7
    ## 129  18        suv   5.7
    ## 130  14        suv   6.1
    ## 131  15        suv   4.0
    ## 132  18        suv   4.2
    ## 133  18        suv   4.4
    ## 134  15        suv   4.6
    ## 135  17        suv   5.4
    ## 136  16        suv   5.4
    ## 137  18        suv   5.4
    ## 138  17        suv   4.0
    ## 139  19        suv   4.0
    ## 140  19        suv   4.6
    ## 141  17        suv   5.0
    ## 142  29    compact   2.4
    ## 143  27    compact   2.4
    ## 144  31    midsize   2.5
    ## 145  32    midsize   2.5
    ## 146  27    midsize   3.5
    ## 147  26    midsize   3.5
    ## 148  26    midsize   3.0
    ## 149  25    midsize   3.0
    ## 150  25    midsize   3.5
    ## 151  17        suv   3.3
    ## 152  17        suv   3.3
    ## 153  20        suv   4.0
    ## 154  18        suv   5.6
    ## 155  26    midsize   3.1
    ## 156  26    midsize   3.8
    ## 157  27    midsize   3.8
    ## 158  28    midsize   3.8
    ## 159  25    midsize   5.3
    ## 160  25        suv   2.5
    ## 161  24        suv   2.5
    ## 162  27        suv   2.5
    ## 163  25        suv   2.5
    ## 164  26        suv   2.5
    ## 165  23        suv   2.5
    ## 166  26 subcompact   2.2
    ## 167  26 subcompact   2.2
    ## 168  26 subcompact   2.5
    ## 169  26 subcompact   2.5
    ## 170  25    compact   2.5
    ## 171  27    compact   2.5
    ## 172  25    compact   2.5
    ## 173  27    compact   2.5
    ## 174  20        suv   2.7
    ## 175  20        suv   2.7
    ## 176  19        suv   3.4
    ## 177  17        suv   3.4
    ## 178  20        suv   4.0
    ## 179  17        suv   4.7
    ## 180  29    midsize   2.2
    ## 181  27    midsize   2.2
    ## 182  31    midsize   2.4
    ## 183  31    midsize   2.4
    ## 184  26    midsize   3.0
    ## 185  26    midsize   3.0
    ## 186  28    midsize   3.5
    ## 187  27    compact   2.2
    ## 188  29    compact   2.2
    ## 189  31    compact   2.4
    ## 190  31    compact   2.4
    ## 191  26    compact   3.0
    ## 192  26    compact   3.0
    ## 193  27    compact   3.3
    ## 194  30    compact   1.8
    ## 195  33    compact   1.8
    ## 196  35    compact   1.8
    ## 197  37    compact   1.8
    ## 198  35    compact   1.8
    ## 199  15        suv   4.7
    ## 200  18        suv   5.7
    ## 201  20     pickup   2.7
    ## 202  20     pickup   2.7
    ## 203  22     pickup   2.7
    ## 204  17     pickup   3.4
    ## 205  19     pickup   3.4
    ## 206  18     pickup   4.0
    ## 207  20     pickup   4.0
    ## 208  29    compact   2.0
    ## 209  26    compact   2.0
    ## 210  29    compact   2.0
    ## 211  29    compact   2.0
    ## 212  24    compact   2.8
    ## 213  44    compact   1.9
    ## 214  29    compact   2.0
    ## 215  26    compact   2.0
    ## 216  29    compact   2.0
    ## 217  29    compact   2.0
    ## 218  29    compact   2.5
    ## 219  29    compact   2.5
    ## 220  23    compact   2.8
    ## 221  24    compact   2.8
    ## 222  44 subcompact   1.9
    ## 223  41 subcompact   1.9
    ## 224  29 subcompact   2.0
    ## 225  26 subcompact   2.0
    ## 226  28 subcompact   2.5
    ## 227  29 subcompact   2.5
    ## 228  29    midsize   1.8
    ## 229  29    midsize   1.8
    ## 230  28    midsize   2.0
    ## 231  29    midsize   2.0
    ## 232  26    midsize   2.8
    ## 233  26    midsize   2.8
    ## 234  26    midsize   3.6

``` r
model.matrix(fo)
```

    ##     (Intercept) classcompact classmidsize classminivan classpickup
    ## 1             1            1            0            0           0
    ## 2             1            1            0            0           0
    ## 3             1            1            0            0           0
    ## 4             1            1            0            0           0
    ## 5             1            1            0            0           0
    ## 6             1            1            0            0           0
    ## 7             1            1            0            0           0
    ## 8             1            1            0            0           0
    ## 9             1            1            0            0           0
    ## 10            1            1            0            0           0
    ## 11            1            1            0            0           0
    ## 12            1            1            0            0           0
    ## 13            1            1            0            0           0
    ## 14            1            1            0            0           0
    ## 15            1            1            0            0           0
    ## 16            1            0            1            0           0
    ## 17            1            0            1            0           0
    ## 18            1            0            1            0           0
    ## 19            1            0            0            0           0
    ## 20            1            0            0            0           0
    ## 21            1            0            0            0           0
    ## 22            1            0            0            0           0
    ## 23            1            0            0            0           0
    ## 24            1            0            0            0           0
    ## 25            1            0            0            0           0
    ## 26            1            0            0            0           0
    ## 27            1            0            0            0           0
    ## 28            1            0            0            0           0
    ## 29            1            0            0            0           0
    ## 30            1            0            0            0           0
    ## 31            1            0            0            0           0
    ## 32            1            0            0            0           0
    ## 33            1            0            1            0           0
    ## 34            1            0            1            0           0
    ## 35            1            0            1            0           0
    ## 36            1            0            1            0           0
    ## 37            1            0            1            0           0
    ## 38            1            0            0            1           0
    ## 39            1            0            0            1           0
    ## 40            1            0            0            1           0
    ## 41            1            0            0            1           0
    ## 42            1            0            0            1           0
    ## 43            1            0            0            1           0
    ## 44            1            0            0            1           0
    ## 45            1            0            0            1           0
    ## 46            1            0            0            1           0
    ## 47            1            0            0            1           0
    ## 48            1            0            0            1           0
    ## 49            1            0            0            0           1
    ## 50            1            0            0            0           1
    ## 51            1            0            0            0           1
    ## 52            1            0            0            0           1
    ## 53            1            0            0            0           1
    ## 54            1            0            0            0           1
    ## 55            1            0            0            0           1
    ## 56            1            0            0            0           1
    ## 57            1            0            0            0           1
    ## 58            1            0            0            0           0
    ## 59            1            0            0            0           0
    ## 60            1            0            0            0           0
    ## 61            1            0            0            0           0
    ## 62            1            0            0            0           0
    ## 63            1            0            0            0           0
    ## 64            1            0            0            0           0
    ## 65            1            0            0            0           1
    ## 66            1            0            0            0           1
    ## 67            1            0            0            0           1
    ## 68            1            0            0            0           1
    ## 69            1            0            0            0           1
    ## 70            1            0            0            0           1
    ## 71            1            0            0            0           1
    ##     classsubcompact classsuv displ classcompact:displ classmidsize:displ
    ## 1                 0        0   1.8                1.8                0.0
    ## 2                 0        0   1.8                1.8                0.0
    ## 3                 0        0   2.0                2.0                0.0
    ## 4                 0        0   2.0                2.0                0.0
    ## 5                 0        0   2.8                2.8                0.0
    ## 6                 0        0   2.8                2.8                0.0
    ## 7                 0        0   3.1                3.1                0.0
    ## 8                 0        0   1.8                1.8                0.0
    ## 9                 0        0   1.8                1.8                0.0
    ## 10                0        0   2.0                2.0                0.0
    ## 11                0        0   2.0                2.0                0.0
    ## 12                0        0   2.8                2.8                0.0
    ## 13                0        0   2.8                2.8                0.0
    ## 14                0        0   3.1                3.1                0.0
    ## 15                0        0   3.1                3.1                0.0
    ## 16                0        0   2.8                0.0                2.8
    ## 17                0        0   3.1                0.0                3.1
    ## 18                0        0   4.2                0.0                4.2
    ## 19                0        1   5.3                0.0                0.0
    ## 20                0        1   5.3                0.0                0.0
    ## 21                0        1   5.3                0.0                0.0
    ## 22                0        1   5.7                0.0                0.0
    ## 23                0        1   6.0                0.0                0.0
    ## 24                0        0   5.7                0.0                0.0
    ## 25                0        0   5.7                0.0                0.0
    ## 26                0        0   6.2                0.0                0.0
    ## 27                0        0   6.2                0.0                0.0
    ## 28                0        0   7.0                0.0                0.0
    ## 29                0        1   5.3                0.0                0.0
    ## 30                0        1   5.3                0.0                0.0
    ## 31                0        1   5.7                0.0                0.0
    ## 32                0        1   6.5                0.0                0.0
    ## 33                0        0   2.4                0.0                2.4
    ## 34                0        0   2.4                0.0                2.4
    ## 35                0        0   3.1                0.0                3.1
    ## 36                0        0   3.5                0.0                3.5
    ## 37                0        0   3.6                0.0                3.6
    ## 38                0        0   2.4                0.0                0.0
    ## 39                0        0   3.0                0.0                0.0
    ## 40                0        0   3.3                0.0                0.0
    ## 41                0        0   3.3                0.0                0.0
    ## 42                0        0   3.3                0.0                0.0
    ## 43                0        0   3.3                0.0                0.0
    ## 44                0        0   3.3                0.0                0.0
    ## 45                0        0   3.8                0.0                0.0
    ## 46                0        0   3.8                0.0                0.0
    ## 47                0        0   3.8                0.0                0.0
    ## 48                0        0   4.0                0.0                0.0
    ## 49                0        0   3.7                0.0                0.0
    ## 50                0        0   3.7                0.0                0.0
    ## 51                0        0   3.9                0.0                0.0
    ## 52                0        0   3.9                0.0                0.0
    ## 53                0        0   4.7                0.0                0.0
    ## 54                0        0   4.7                0.0                0.0
    ## 55                0        0   4.7                0.0                0.0
    ## 56                0        0   5.2                0.0                0.0
    ## 57                0        0   5.2                0.0                0.0
    ## 58                0        1   3.9                0.0                0.0
    ## 59                0        1   4.7                0.0                0.0
    ## 60                0        1   4.7                0.0                0.0
    ## 61                0        1   4.7                0.0                0.0
    ## 62                0        1   5.2                0.0                0.0
    ## 63                0        1   5.7                0.0                0.0
    ## 64                0        1   5.9                0.0                0.0
    ## 65                0        0   4.7                0.0                0.0
    ## 66                0        0   4.7                0.0                0.0
    ## 67                0        0   4.7                0.0                0.0
    ## 68                0        0   4.7                0.0                0.0
    ## 69                0        0   4.7                0.0                0.0
    ## 70                0        0   4.7                0.0                0.0
    ## 71                0        0   5.2                0.0                0.0
    ##     classminivan:displ classpickup:displ classsubcompact:displ
    ## 1                  0.0               0.0                   0.0
    ## 2                  0.0               0.0                   0.0
    ## 3                  0.0               0.0                   0.0
    ## 4                  0.0               0.0                   0.0
    ## 5                  0.0               0.0                   0.0
    ## 6                  0.0               0.0                   0.0
    ## 7                  0.0               0.0                   0.0
    ## 8                  0.0               0.0                   0.0
    ## 9                  0.0               0.0                   0.0
    ## 10                 0.0               0.0                   0.0
    ## 11                 0.0               0.0                   0.0
    ## 12                 0.0               0.0                   0.0
    ## 13                 0.0               0.0                   0.0
    ## 14                 0.0               0.0                   0.0
    ## 15                 0.0               0.0                   0.0
    ## 16                 0.0               0.0                   0.0
    ## 17                 0.0               0.0                   0.0
    ## 18                 0.0               0.0                   0.0
    ## 19                 0.0               0.0                   0.0
    ## 20                 0.0               0.0                   0.0
    ## 21                 0.0               0.0                   0.0
    ## 22                 0.0               0.0                   0.0
    ## 23                 0.0               0.0                   0.0
    ## 24                 0.0               0.0                   0.0
    ## 25                 0.0               0.0                   0.0
    ## 26                 0.0               0.0                   0.0
    ## 27                 0.0               0.0                   0.0
    ## 28                 0.0               0.0                   0.0
    ## 29                 0.0               0.0                   0.0
    ## 30                 0.0               0.0                   0.0
    ## 31                 0.0               0.0                   0.0
    ## 32                 0.0               0.0                   0.0
    ## 33                 0.0               0.0                   0.0
    ## 34                 0.0               0.0                   0.0
    ## 35                 0.0               0.0                   0.0
    ## 36                 0.0               0.0                   0.0
    ## 37                 0.0               0.0                   0.0
    ## 38                 2.4               0.0                   0.0
    ## 39                 3.0               0.0                   0.0
    ## 40                 3.3               0.0                   0.0
    ## 41                 3.3               0.0                   0.0
    ## 42                 3.3               0.0                   0.0
    ## 43                 3.3               0.0                   0.0
    ## 44                 3.3               0.0                   0.0
    ## 45                 3.8               0.0                   0.0
    ## 46                 3.8               0.0                   0.0
    ## 47                 3.8               0.0                   0.0
    ## 48                 4.0               0.0                   0.0
    ## 49                 0.0               3.7                   0.0
    ## 50                 0.0               3.7                   0.0
    ## 51                 0.0               3.9                   0.0
    ## 52                 0.0               3.9                   0.0
    ## 53                 0.0               4.7                   0.0
    ## 54                 0.0               4.7                   0.0
    ## 55                 0.0               4.7                   0.0
    ## 56                 0.0               5.2                   0.0
    ## 57                 0.0               5.2                   0.0
    ## 58                 0.0               0.0                   0.0
    ## 59                 0.0               0.0                   0.0
    ## 60                 0.0               0.0                   0.0
    ## 61                 0.0               0.0                   0.0
    ## 62                 0.0               0.0                   0.0
    ## 63                 0.0               0.0                   0.0
    ## 64                 0.0               0.0                   0.0
    ## 65                 0.0               4.7                   0.0
    ## 66                 0.0               4.7                   0.0
    ## 67                 0.0               4.7                   0.0
    ## 68                 0.0               4.7                   0.0
    ## 69                 0.0               4.7                   0.0
    ## 70                 0.0               4.7                   0.0
    ## 71                 0.0               5.2                   0.0
    ##     classsuv:displ
    ## 1              0.0
    ## 2              0.0
    ## 3              0.0
    ## 4              0.0
    ## 5              0.0
    ## 6              0.0
    ## 7              0.0
    ## 8              0.0
    ## 9              0.0
    ## 10             0.0
    ## 11             0.0
    ## 12             0.0
    ## 13             0.0
    ## 14             0.0
    ## 15             0.0
    ## 16             0.0
    ## 17             0.0
    ## 18             0.0
    ## 19             5.3
    ## 20             5.3
    ## 21             5.3
    ## 22             5.7
    ## 23             6.0
    ## 24             0.0
    ## 25             0.0
    ## 26             0.0
    ## 27             0.0
    ## 28             0.0
    ## 29             5.3
    ## 30             5.3
    ## 31             5.7
    ## 32             6.5
    ## 33             0.0
    ## 34             0.0
    ## 35             0.0
    ## 36             0.0
    ## 37             0.0
    ## 38             0.0
    ## 39             0.0
    ## 40             0.0
    ## 41             0.0
    ## 42             0.0
    ## 43             0.0
    ## 44             0.0
    ## 45             0.0
    ## 46             0.0
    ## 47             0.0
    ## 48             0.0
    ## 49             0.0
    ## 50             0.0
    ## 51             0.0
    ## 52             0.0
    ## 53             0.0
    ## 54             0.0
    ## 55             0.0
    ## 56             0.0
    ## 57             0.0
    ## 58             3.9
    ## 59             4.7
    ## 60             4.7
    ## 61             4.7
    ## 62             5.2
    ## 63             5.7
    ## 64             5.9
    ## 65             0.0
    ## 66             0.0
    ## 67             0.0
    ## 68             0.0
    ## 69             0.0
    ## 70             0.0
    ## 71             0.0
    ##  [ reached getOption("max.print") -- omitted 163 rows ]
    ## attr(,"assign")
    ##  [1] 0 1 1 1 1 1 1 2 3 3 3 3 3 3
    ## attr(,"contrasts")
    ## attr(,"contrasts")$class
    ## [1] "contr.treatment"

``` r
detach(mpg)
```

Heavily annotated from the following sources <https://rviews.rstudio.com/2017/02/01/the-r-formula-method-the-good-parts/> <https://cran.r-project.org/web/packages/lazyeval/vignettes/lazyeval.html> <http://rmhogervorst.nl/cleancode/blog/2016/06/13/NSE_standard_evaluation_dplyr.html>

``` r
mod1 <- lm(Sepal.Width ~ Petal.Width + log(Petal.Length) + Species,
           data = iris,
           subset = Sepal.Length > 4.6)
```

The following code extracts the relevant columns and rows specified in the formula and places them in a dataframe

``` r
model.frame(mod1) %>% head()
```

    ##   Sepal.Width Petal.Width log(Petal.Length) Species
    ## 1         3.5         0.2         0.3364722  setosa
    ## 2         3.0         0.2         0.3364722  setosa
    ## 3         3.2         0.2         0.2623643  setosa
    ## 5         3.6         0.2         0.3364722  setosa
    ## 6         3.9         0.4         0.5306283  setosa
    ## 8         3.4         0.2         0.4054651  setosa

The formula interface represents the symbolic model, as well as the design ' matrix (`X`). Here, `y = Sepal.Width` and `X` has the columns `Petal.Width`, `log(Petal.Length)` and `Species`. The rows from the data set to be used is also specified.

``` r
str(iris$Species)
```

    ##  Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...

``` r
model.matrix(mod1) %>% head() 
```

    ##   (Intercept) Petal.Width log(Petal.Length) Speciesversicolor
    ## 1           1         0.2         0.3364722                 0
    ## 2           1         0.2         0.3364722                 0
    ## 3           1         0.2         0.2623643                 0
    ## 5           1         0.2         0.3364722                 0
    ## 6           1         0.4         0.5306283                 0
    ## 8           1         0.2         0.4054651                 0
    ##   Speciesvirginica
    ## 1                0
    ## 2                0
    ## 3                0
    ## 5                0
    ## 6                0
    ## 8                0

As can be seen above, there are three factor levels, so 2 dummy variables need to be automagically created. The underlying matrix is always accessible. Note the absence of the outcome variable ' The model matrix approach is widely used within the scikit-learn family, but R has this rather convenient formula interface which allows a symbolic model to be expressed directly in code. Formulas are employed in R beyond statistical model specifications (e.g., in `dplyr`)

There are three parts to a formula - the tilde(`~`), expression to the left of `~` (usually the response) and the expression to the right of `~` (usually the predictors).

`~` is an operator and hence is a shortcut to a function just as `+`, `-`

So, the following code is valid:

``` r
`~`(lhs, rhs)
```

    ## lhs ~ rhs

``` r
#`+`(lhs, rhs) does not work
`+`(1, 2)
```

    ## [1] 3

These three function evaluations exemplify the nature of `~`. It parses the arguments but does not evaluate them, unlike `+`. This is an entry point to the world of metaprogramming, where the unevaluated code is processed in a specific way before evaluation so that the user can enjoy the benefit of a human-parseable interface. In sum, formulas capture an enevaluated expression and the context in which the expression was created.

These facets of the formula object are explored below:

``` r
myFormula <- Sepal.Width ~ Petal.Width + log(Petal.Length) + Species
str(myFormula)
```

    ## Class 'formula'  language Sepal.Width ~ Petal.Width + log(Petal.Length) + Species
    ##   ..- attr(*, ".Environment")=<environment: R_GlobalEnv>

``` r
myFormula[[1]] # ~
```

    ## `~`

``` r
myFormula[[2]] # lhs
```

    ## Sepal.Width

``` r
myFormula[[3]] # rhs
```

    ## Petal.Width + log(Petal.Length) + Species

``` r
length(myFormula)
```

    ## [1] 3

By using a formula evaluation can be delayed, and since the context is captured it evaluates properly even in a different place However, someone has to parse and evaluate the formula object some place!

``` r
x <- 1
add_100 <- function(x) {
  ~ x + 100
}

add_100(2)
```

    ## ~x + 100
    ## <environment: 0x000000001eda9160>

``` r
eval(add_100(2))
```

    ## ~x + 100
    ## <environment: 0x000000001ecf0630>
