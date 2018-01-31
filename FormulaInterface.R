library(tidyverse)

data("mpg")
data("iris")

attach(mpg)

class(fo <- hwy ~ class * displ)
typeof(fo)
terms(fo)
model.frame(fo)
model.matrix(fo)

detach(mpg)

# Heavily annotated from the following sources
# https://rviews.rstudio.com/2017/02/01/the-r-formula-method-the-good-parts/
# https://cran.r-project.org/web/packages/lazyeval/vignettes/lazyeval.html
# http://rmhogervorst.nl/cleancode/blog/2016/06/13/NSE_standard_evaluation_dplyr.html
# 

mod1 <- lm(Sepal.Width ~ Petal.Width + log(Petal.Length) + Species,
           data = iris,
           subset = Sepal.Length > 4.6)

model.frame(mod1) %>% head()# Extracts the relevant columns and rows specified 
                            # in the formula and places them in a dataframe

# The formula interface represents the symbolic model, as well as the design 
# matrix (X). Here, y = Sepal.Width and X has the columns Petal.Width, 
# log(Petal.Length) and Species. The rows from the data set to be used is also
# specified.

str(iris$Species)
model.matrix(mod1) %>% head() # three factor levels, so 2 dummy variables are 
                              # automagically created. The underlying matrix is 
                              # always accessible. Note the absence of the 
                              # outcome variable

# The model matrix approach is widely used within the scikit-learn family, but
# R has this rather convenient formula interface which allows a symbolic model
# to be expressed directly in code. Formulas are employed in R beyond statistical
# model specifications (e.g., in dplyr)

# There are three parts to a formula - the tilde(~), expression to the left of
# ~ (usually the response) and the expression to the right of ~ (usually the 
# predictors).
# ~ is an operator and hence is a shortcut to a function just as +, -

`~`(lhs, rhs)
`+`(lhs, rhs)
`+`(1, 2)

# These three function evaluations exemplify the nature of ~. It parses the arguments
# but does not evaluate them, unlike +. This is an entry point to the world of 
# metaprogramming, where the unevaluated code is processed in a specific way
# before evaluation so that the user can enjoy the benefit of a human-parseable
# interface. In sum, formulas capture an enevaluated expression and the context
# in which the expression was created

myFormula <- Sepal.Width ~ Petal.Width + log(Petal.Length) + Species
str(myFormula)
myFormula[[1]] # ~
myFormula[[2]] # lhs
myFormula[[3]] # rhs
length(myFormula)

# By using a formula evaluation can be delayed, and since the context is captured
# it evaluates properly even in a different place
# However, someone has to parse and evaluate the formula object some place!

x <- 1
add_100 <- function(x) {
  ~ x + 100
}

add_100(2)
eval(add_100(2))