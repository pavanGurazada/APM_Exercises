#' ---
#' title: "Understanding linear regression and its cousins"
#' header-includes: \usepackage{amsmath}
#' date: "February 2018"
#' output: github_document
#' ---

#' In this chapter we take a closer look at understanding the family of linear
#' regression methods. The focus is on understanding how each method works,
#' rather than the matrix plumbing associated with them. We take an explicit
#' geometric view here, rather than a numerical approach. Hat-tip to
#' [3Blue1Brown](https://www.youtube.com/channel/UCYO_jab_esuFRV4b17AJtAw) for
#' kindling the geometric intuition. In this workbook, we take this initial
#' intuition forward and extend it to linear models. The idea is to describe the
#' geometry of several linear modeling algorithms. The emphasis is on
#' undertsanding why things are the way there are and not how. Read the prose
#' and visualize the geometry.
#'
#' **Introduction**
#'
#' *Idea 1* Any point in space can be represented by a vector, i.e., an arrow
#' drawn from a fixed origin to the point. To reiterate, the origin never moves
#' and the tail of every vector lies on the origin. The coordinates of a vector
#' encode the instructions of how to reach the point it represents starting from
#' the origin and moving parallel to the axes, in order, i.e., $x, y, z,
#' \ldots$.
#'
#' *Idea 2* Vector addition is an extension of the geometry of addition of two
#' real numbers represented on the number line. $2 + 5$ represents a movement of
#' 2 steps to begin with and 5 steps in the same direction ending up $7$ steps
#' away from a fixed origin. Similarly, $\mathbf{v} + \mathbf{u}$ represents a
#' movement along $\mathbf{v}$ starting from the origin and moving parallel to
#' $\mathbf{u}$ from the tip of $\mathbf{v}$.
#'
#' *Idea 3* Multiplication of a vector by real numbers is equivalent to
#' squishing or enlarging the vectors in the same direction (for positive reals)
#' or in the opposite direction (for negative reals). This is the reaosn why
#' real numbers are called scalars in this world.