Understanding linear regression and its cousins
================
Pavan Gurazada
February 2018

``` r
library(AppliedPredictiveModeling)
library(caret)
library(tidyverse)
library(latex2exp)

set.seed(20130810)
theme_set(theme_bw())
```

In this chapter we take a closer look at understanding the family of linear regression methods. The focus is on understanding how each method works, rather than the matrix plumbing associated with them. We take an explicit geometric view here, rather than a numerical approach. Hat-tip to [3Blue1Brown](https://www.youtube.com/channel/UCYO_jab_esuFRV4b17AJtAw) for kindling the geometric intuition. In this workbook, we take this initial intuition forward and extend it to linear models. The idea is to describe the geometry of several linear modeling algorithms. The emphasis is on undertsanding why things are the way there are and not how. Read the prose and visualize the geometry.

Introduction
============

*Idea 1*. Any point in space can be represented by a vector, i.e., an arrow drawn from a fixed origin to the point. To reiterate, the origin never moves and the tail of every vector lies on the origin. The coordinates of a vector encode the instructions of how to reach the point it represents starting from the origin and moving parallel to the axes, in order, i.e., *x*, *y*, *z*, ….

``` r
ggplot(data.frame(x = -5:5, y = -5:5), aes(x, y)) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 2), 
               arrow = arrow(ends = "last", type = "closed", length = unit(0.1, "inches"))) + 
  geom_segment(aes(x = 0, y = 0, xend = 3, yend = 1), 
               arrow = arrow(ends = "last", type = "closed", length = unit(0.1, "inches"))) +
  scale_x_continuous(limits = c(-5, 5)) + 
  scale_y_continuous(limits = c(-5, 5)) +
  geom_vline(aes(xintercept = 0)) + 
  geom_hline(aes(yintercept = 0)) +
  geom_text(aes(x = 0.5, y = 0.5, label = "y"), nudge_y = 1) 
```

![](C:\Users\Pavan%20Gurazada\Documents\GitHub\APMExercises\notes\Ch6LinearRegressionCousins_files/figure-markdown_github/unnamed-chunk-2-1.png)

*Idea 2*. Vector addition is an extension of the geometry of addition of two real numbers represented on the number line. For e.g., 2 + 5 represents a movement of 2 steps to begin with and 5 steps in the same direction ending up 7 steps away from a fixed origin. Similarly, **v** + **u** represents a movement along **v** starting from the origin and moving parallel to **u** from the tip of **v**. In this way, vector addition captures the notion that vectors have a size and direction.

*Idea 3*. Multiplication of a vector by real numbers is equivalent to squishing or enlarging the vectors in the same direction (for positive reals) or in the opposite direction (for negative reals). This is the reason why real numbers are called scalars.

*Idea 4*. Each coordinate of a vector squishes or enlarges unit vectors ($\\hat{i}, \\hat{j}, \\ldots$) along the axes. Adding these scaled unit vectors gives us the original vector. The unit vectors are called the basis vectors of a coordinate system. By altering the choices of the scalars we can reach every point in the space mapped by the basis vectors.

The choice of basis vectors is not unique. This implies that we could have chosen a non-standard arbitrary set of vectors and through a judicious choice of scalars reached every point in the space mapped by the unit vectors. The space remains the same but the method we use to reach that point, i.e., the choice of the axes differs. The origin is always fixed.

*Idea 5*. The process of scaling and adding a set of vectors is called a linear combination. So, a linear combination of the set **v**<sub>**1**</sub>, **v**<sub>**2**</sub>, … is *β*<sub>1</sub>**v**<sub>**1**</sub> + *β*<sub>2</sub>**v**<sub>**2**</sub> **+** **…**. By varying the choice of *β*'s we can reach the space covered by the set of vectors. The set of all possible vectors one can reach using a linear combination of a set of vectors is called the span of the vectors.

*Idea 6*. In the situation where a suset of vectors point in the same direction or when a vector is in the span of others in the set the reach of the linear combination is limited, i.e., there is redundant information. The linear combination of these vectors lies in the same direction and hence does not cover the entire space. In this case, the basis vectors are linearly dependent.

This illustrates why having a nice set of basis vectors is important if we wish to explore in the entire parameter space.

*Idea 7*. Matrices encode the idea of linear transformations numerically. In these transformations, the origin remains fixed and the axes and the grid lines remain parallel and evenly spaced (i.e., the scale of the axes is locked). Linear transformation in essense produce a new space centered at the same origin where the vectors from the original space now lie in a different position. To track where a vector **v** lands after a linear transformation, we only need to track where the basis vectors of the original transformation $\\hat{i}, \\hat{j}, \\ldots$ land up.

In other words the transformative effect of the linear transformation can be recorded through the transformation in the basis vectors and every other vector can be derived from these. So, a general vector $\\mathbf{v} = v\_1 \\hat{i} + v\_2 \\hat{j} + \\ldots$ is mapped to a new vector $\\mathbf{v}^' = v\_1 \\hat{i}^' + v\_2 \\hat{j}^' + \\ldots$. Since we can only keep a track of where the basis vectors go, this transformation is encoded as a square matrix where each column of the matrix represents the position of the new basis vectors $\\hat{i}^', \\hat{j}^', \\ldots$ after the transformation.

If the new basis vectors are linearly dependent, all of space is squished into a space of lesser dimension.

Every matrix is a linear transformation of space. Multiplying a vector with the matrix on the left provides the new position of the vector in the transformed space.

A chain of linear transformations, for e.g., rotation (say **A**) followed by shear (say **B**), the final position of a vector **v** is then **B**(**A****v**). This brings forth the notion of a product of two matrices as a composition of two linear transformations.

Overall view of linear models
=============================

With this initial intuition out of the way, we are in a position to interpret linear models in a geometric way. In linear modeling, we predict an outcome vector **y** using a set of feature vectors, usually collected into a model matrix **X**. As discussed earlier, since we deal with linear functions of the predictors, this matrix set up simply means that multiplying this matrix with a vector translates the vector from the standard basis to the feature space. We then seek a solution to the ideal vector (**β**) that when transformed to the parameter space is closest to the outcome vecvot **y**. Once a new sample is generated, we can use **β** to translate back to the parameter space and predict the outcome.
