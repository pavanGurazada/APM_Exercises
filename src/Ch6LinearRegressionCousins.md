---
title: "Understanding linear regression and its cousins"
date: "February 2018"
header-includes:
    - \usepackage{amsmath}
output: 
  html_document:
    keep_md: true
---

In this chapter we take a closer look at understanding the family of linear regression methods. The focus is on understanding how each method works, rather than the matrix plumbing associated with them. We take an explicit geometric view here, rather than a numerical approach. Hat-tip to [3Blue1Brown](https://www.youtube.com/channel/UCYO_jab_esuFRV4b17AJtAw) for kindling the geometric intuition. In this workbook, we take this initial intuition forward and extend it to linear models. The idea is to describe the geometry of several linear modeling algorithms. The emphasis is on undertsanding why things are the way there are and not how. Read the prose and visualize the geometry.

**Introduction**

*Idea 1* Any point in space can be represented by a vector, i.e., an arrow drawn from a fixed origin to the point. To reiterate, the origin never moves and the tail of every vector lies on the origin. The coordinates of a vector encode the instructions of how to reach the point it represents starting from the origin and moving parallel to the axes, in order, i.e., $x, y, z, \ldots$. 

*Idea 2* Vector addition is an extension of the geometry of addition of two real numbers represented on the number line. $2 + 5$ represents a movement of 2 steps to begin with and 5 steps in the same direction ending up $7$ steps away from a fixed origin. Similarly, $\mathbf{v} + \mathbf{u}$ represents a movement along $\mathbf{v}$ starting from the origin and moving parallel to $\mathbf{u}$ from the tip of $\mathbf{v}$. 

$$
\begin{align}
\mathbf{v} = \begin{bmatrix}
                 v_{1} \\
                 v_{2}
                \end{bmatrix},
\mathbf{u} = \begin{bmatrix}
                 u_{1} \\
                 u_{2}
                \end{bmatrix},
\mathbf{v + u} = \begin{bmatrix}
                 v_{1} + u_{1} \\
                 v_{2} + u_{2}
                \end{bmatrix}

\end{align}
$$

*Idea 3* Multiplication of a vector by real numbers is equivalent to squishing or enlarging the vectors in the same direction (for positive reals) or in the opposite direction (for negative reals). This is the reaosn why real numbers are called scalars in this world.

$$
\mathbf{v} = \left[\begin{array}{rrr}
                   v_1 \\
                   v_2
                   \end{array}
              \right]
\implies 2\mathbf{v} = \left[\begin{array}{rrr}
                   2v_1 \\
                   2v_2
                   \end{array}
              \right]
$$

*Idea 4* Each coordinate of a vector squishes or enlarges unit vectors ($\hat{i}, \hat{j}, \ldots$) along the axes. Adding these scaled unit vectors gives us the original vector. The unit vectors are called the basis vectors of a coordinate system. By altering the choices of the scalars we can reach every point in the space mapped by the basis vectors. 

The choice of basis vectors is not unique. This implies that we could have chosen a non-standard arbitrary set of vectors and through a judicious choice of scalars reach every point in the space mapped by the unit vectors. The space remains the same but the method we use to reach that point, i.e., the choice of the axes differs (the origin is fixed). 

*Idea 5* The process of scaling and adding a set of vectors is called a linear combination. So, a linear combination of the set $\mathbf{v_1}, \mathbf{v_2}, \ldots$ is $\beta_1\mathbf{v_1} + \beta_2\mathbf{v_2 + \ldots}$. By varying the choice of $\beta$'s we can reach the space covered by the set of vectors. The set of all possible vectors one can reach using a linear combination of a set of vectors is called the span of the vectors.  

*Idea 6* In the situation where a suset of vectors point in the same direction or when a vector is in the span of others in the set the reach of the linear combination is limited, i.e., there is redundant information, the linear combination of these vectors lies in the same direction and hence does not cover the entire space. 




```
## Loading required package: lattice
```

```
## Loading required package: ggplot2
```

```
## -- Attaching packages ----------------------------------------------------------------------------------------------------------------------- tidyverse 1.2.1 --
```

```
## v tibble  1.4.2     v purrr   0.2.4
## v tidyr   0.8.0     v dplyr   0.7.4
## v readr   1.1.1     v stringr 1.2.0
## v tibble  1.4.2     v forcats 0.2.0
```

```
## -- Conflicts -------------------------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
## x purrr::lift()   masks caret::lift()
```

