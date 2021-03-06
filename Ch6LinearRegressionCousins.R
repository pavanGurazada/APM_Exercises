#' ---
#' title: "Understanding linear regression and its cousins"
#' header-includes: 
#'     - \usepackage{amssymbols}
#' author: Pavan Gurazada
#' date: "February 2018"
#' output: github_document
#' ---

library(AppliedPredictiveModeling)
library(caret)
library(tidyverse)

library(doParallel)
library(parallel)
clusters <- makeCluster(detectCores() - 1)
registerDoParallel(clusters)

set.seed(20130810)
dev.new()
theme_set(theme_bw())

#' In this chapter we take a closer look at understanding the family of linear
#' regression methods. The focus is on understanding how each method works,
#' rather than the numerical plumbing associated with them. Hat-tip to
#' [3Blue1Brown](https://www.youtube.com/channel/UCYO_jab_esuFRV4b17AJtAw) for
#' kindling the geometric intuition. Most of the geometric ideas in the
#' introduction are a direct summary of the information presented in these
#' videos. This is more of a 'teach-myself-how-things-work' while speaking in
#' code. Hence, the idea is to describe the geometry of several linear modeling
#' algorithms. The emphasis is on undertsanding why things are the way there are
#' and not how.
#'
#' To reiterate, there is a strong case for numerical linear algebra. However,
#' it is a lot more fun to think in terms of the geometry of linear models
#' rather than sequences of floating pointoperations.
#'
#' ## Introduction
#'
#' *Idea 1*. Any point in space can be represented by a vector, i.e., an arrow
#' drawn from a fixed origin to the point. The origin never moves and the tail
#' of every vector lies on the origin. The coordinates of a vector encode the
#' instructions of how to reach the point it represents starting from the origin
#' and moving parallel to the axes, in order, i.e., $x, y, z, \ldots$.

ggplot(data.frame(x = -5:5, y = -5:5), aes(x, y)) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 2), 
               arrow = arrow(ends = "last", type = "closed", length = unit(0.1, "inches"))) + 
  geom_segment(aes(x = 0, y = 0, xend = 3, yend = 1), 
               arrow = arrow(ends = "last", type = "closed", length = unit(0.1, "inches"))) +
  geom_segment(aes(x = 3, y = 1, xend = 1, yend = 2), 
               arrow = arrow(ends = "last", type = "closed", length = unit(0.1, "inches"))) +
  scale_x_continuous(limits = c(-5, 5)) + 
  scale_y_continuous(limits = c(-5, 5)) +
  geom_vline(aes(xintercept = 0)) + 
  geom_hline(aes(yintercept = 0)) +
  geom_text(aes(x = 0.5, y = 0.5, label = "y"), nudge_y = 1) + 
  geom_text(aes(x = 1.5, y = 0.5, label = "X b"), nudge_x = 1) +
  geom_text(aes(x = 1.5, y = 2, label = " y - X b"), nudge_x = 1) 
  

#' *Idea 2*. Vector addition is an extension of the geometry of addition of two
#' real numbers represented on the number line. For e.g., $2 + 5$ represents a
#' movement of 2 steps to begin with and 5 steps in the same direction ending up
#' $7$ steps away from a fixed origin. Similarly, $\mathbf{v} + \mathbf{u}$
#' represents a movement along $\mathbf{v}$ starting from the origin and moving
#' parallel to $\mathbf{u}$ from the tip of $\mathbf{v}$. In this way, vector
#' addition captures the notion that vectors have a size and direction.
#'
#' *Idea 3*. Multiplication of a vector by real numbers is equivalent to
#' squishing or enlarging the vectors in the same direction (for positive reals)
#' or in the opposite direction (for negative reals). This is the reason why
#' real numbers are called scalars.
#'
#' *Idea 4*. Each coordinate of a vector squishes or enlarges unit vectors
#' ($\mathbf{i}, \mathbf{j}, \ldots$) along the axes. Adding these scaled unit
#' vectors gives us the original vector. The unit vectors are called the basis
#' vectors of a coordinate system. By altering the choices of the scalars we can
#' reach every point in the space mapped by the basis vectors.
#'
#' The choice of basis vectors is not unique. This implies that we could have
#' chosen a non-standard arbitrary set of vectors and through a judicious choice
#' of scalars reached every point in the space mapped by the unit vectors. The
#' space remains the same but the method we use to reach that point, i.e., the
#' choice of the axes differs. The origin is always fixed.
#'
#' *Idea 5*. The process of scaling and adding a set of vectors is called a
#' linear combination. So, a linear combination of the set $\mathbf{v_1},
#' \mathbf{v_2}, \ldots$ is $b_1 \mathbf{v_1} + b_2 \mathbf{v_2} + \ldots$. By
#' varying the choice of $b$'s we can reach the space covered by the set of
#' vectors. The set of all possible vectors one can reach using a linear
#' combination of a set of vectors is called the span of the vectors.
#'
#' *Idea 6*. In the situation where a subset of vectors point in the same
#' direction or when a vector is in the span of others in the set, the reach of
#' the linear combination is limited, i.e., there is redundant information. The
#' linear combination of these vectors hence does not cover the entire space. In
#' this case, the basis vectors are linearly dependent.
#'
#' This illustrates why having a nice set of basis vectors is important if we
#' wish to explore in the entire parameter space.
#'
#' *Idea 7*. Matrices encode the idea of linear transformations numerically. In
#' these transformations, the origin remains fixed and the axes and the grid
#' lines remain parallel and evenly spaced (i.e., the scale of the axes cannot
#' be manipulated independent of each other). Linear transformation in essense
#' produce a new space centered at the same origin where the vectors from the
#' original space now lie in a different position. To track where a vector
#' $\mathbf{v}$ lands after a linear transformation, we only need to track where
#' the basis vectors of the original transformation $\mathbf{i}, \mathbf{j},
#' \ldots$ land up.
#'
#' In other words the transformative effect of the linear transformation can be
#' recorded through the transformation in the basis vectors and every other
#' vector can be derived from these. So, a general vector $\mathbf{v} = v_1
#' \mathbf{i} + v_2 \mathbf{j} + \ldots$ is mapped to a new vector $\mathbf{v}^'
#' = v_1 \mathbf{i}^' + v_2 \mathbf{j}^' + \ldots$. Since we need to only keep a
#' track of where the basis vectors go, this transformation is encoded as a
#' square matrix where each column of the matrix represents the position of the
#' new basis vectors $\mathbf{i}^', \mathbf{j}^', \ldots$ after the
#' transformation.
#'
#' If the new basis vectors are linearly dependent, all of space is squished
#' into a space of lesser dimension.
#'
#' Every matrix is a linear transformation of space. Multiplying  a vector with
#' the matrix on the left provides the new position of the vector in the
#' transformed space.
#'
#' A chain of linear transformations, for e.g., rotation (say $\mathbf{A}$)
#' followed by shear (say $\mathbf{B}$), the final position of a vector
#' $\mathbf{v}$ is then $\mathbf{B}(\mathbf{A} \mathbf{v})$. This brings forth
#' the notion of a product of two matrices as a composition of two linear
#' transformations.
#'
#' ## Overall view of linear models
#'
#' With this initial intuition, we can explore the geometry of linear models. In
#' linear modeling, we predict an outcome vector $y$ using a set of
#' feature vectors, usually collected into a model matrix $X$. Since
#' the features are a choice of the analyst, the model matrix is the feature
#' space as defined by the analyst. As discussed earlier, multiplying this
#' matrix with a vector translates the vector from the standard basis to the
#' feature space. Since the outcome is also measured in the feature space,
#' linear models seek to minimize the difference between the outcome and the
#' transformed vector. Hence, we seek a solution to the ideal vector
#' ($b$) that when transformed to the parameter space is closest to the
#' outcome vector $y$. For a new model matrix $X_t$ in the
#' feature space, this same transformation $X_t b$ maps these points
#' (hopefully) close to the actual outcome.
#'
#' ## Different flavors of linear models
#'
#' 1. *OLS* models seek to find the $b$ that minimizes the squared
#' error, i.e., $(y -\! X \! b)^2$. This problem admits a well-known closed
#' form solution and hence can be used to compute $b$ without
#' iteration. We can [re-create the
#' wheel](https://www.r-bloggers.com/create-your-machine-learning-library-from-scratch-with-r-1-3/),
#' by creating a OLS linear regression object, defining the predict and plot
#' methods for it. This is a nice example where the mathematics is simple so the
#' R mechanics of creating model objects can be understood clearly:

fitLM <- function(X, y, intercept = TRUE, lambda = 0) {
  if (!is.matrix(X)) {
    X <- as.matrix(X)
  } 
  
  if (!is.matrix(y)) {
    y <- as.matrix(y)
  }
  
  if (intercept) {
    X <- cbind(1, X)
  }
  
  output <- list(intercept = intercept)
  
  output$coefs <- solve(t(X) %*% X) %*% t(X) %*% y
  output$preds <- X  %*% output$coefs
  output$residuals <- output$preds - y
  output$MSE <- mean(output$residuals^2)
  
  attr(output, "class") <- "naiveLM"
  return (output)
}

predict.naiveLM <- function(naiveLMObject, X, ...) {
  if (!is.matrix(X)) {
    X <- as.matrix(X)
  }
  
  if (naiveLMObject$intercept) {
    X <- cbind(1, X)
  }
  
  return (X %*% naiveLMObject$coefs)
}

plot.naiveLM <- function(naiveLMObject, bins = 30, ...) {
  qplot(naiveLMObject$preds,
        naiveLMObject$residuals, 
        geom = "point") +
    xlab("Predicted Values") + 
    ylab("Residuals") + 
    geom_hline(aes(yintercept = 0), linetype = "dotted") + 
    ggtitle("Residuals vs. Fitted Values")
}

myLM <- fitLM(cars[, 1], cars[, 2])
glimpse(myLM)

print(myLM$coefs)
plot(myLM)

#' Another way to derive the regression coefficients is to use the Maximum
#' Likelihood Estimation (MLE) technique. Here, we assume that the residuals are
#' i.i.d. standard normal and derive how likely is it to observe the data.
#'
#' Again, [reinventing the
#' wheel](https://www.r-bloggers.com/fitting-a-model-by-maximum-likelihood/).
#' In my opinion, there is something mathematically comforting about this
#' approach. We build up the probability distribution of the residuals and then
#' hand it over to an optimization routine to minimize this. There is a clean
#' separation between the logic of the estimation routine and the assumptions
#' are clear from the outset.
#' 
#' Start with some fake data

N <- 1e4

x <- runif(N)
y <- 3 + 5 * x + rnorm(N)

#' The residuals likelihood is calculated by drawing from a normal distribution

logLikelihood <- function(beta0, beta1, mu, sigma) {
  resids <- y - (beta0 + x * beta1) 
  
  resids <- suppressWarnings(dnorm(resids, mu, sigma))
  return(-sum(log(resids)))
}

#' The initial guess in the MLE is very important

# fit <- stats4::mle(logLikelihood, 
#                    start = list(beta0 = 3, beta1 = 1, mu = 0, sigma = 1))

fit <- stats4::mle(logLikelihood,
                   start = list(beta0 = 2, beta1 = 1.5, sigma = 1),
                   fixed = list(mu = 0),
                   nobs = length(y))

print(fit)

#' Fitting a different distribution of residuals is a simple matter of changing
#' the `dnorm()` in the log likelihood function to a different distribution.
#' Choosing assumptions and starting points for MLE, though, is not for the
#' faint heart.

data(solubility)
glimpse(solTrainX)

trainingData <- solTrainXtrans
trainingData$Solubility <- solTrainY

lmSol1 <- train(Solubility ~ .,
                data = trainingData,
                method = "lm",
                preProcess = c("center", "scale"),
                trControl = trainControl(method = "optimism_boot", 
                                         number = 200,
                                         allowParallel = TRUE))
lmSol2 <- train(Solubility ~ .,
                data = trainingData,
                method = "lm",
                preProcess = c("center", "scale"),
                trControl = trainControl(method = "boot632", 
                                         number = 200,
                                         allowParallel = TRUE))
lmSol3 <- train(Solubility ~ .,
                data = trainingData,
                method = "lm",
                preProcess = c("center", "scale"),
                trControl = trainControl(method = "repeatedcv",
                                         number = 10, 
                                         repeats = 5,
                                         allowParallel = TRUE))
print(lmSol1$results)
print(lmSol2$results)
print(lmSol3$results)

#' There is a phenomenal difference in the RMSE between the two methods of
#' cross-validation. There were several warnings that were produced in the model
#' fit process essentially indicating a multi-collinearity problem. We can
#' remove the offensive predictors and refit the model

corrOffenders <- findCorrelation(cor(solTrainXtrans), 0.9)
solTrainXFiltered <- solTrainXtrans[, -corrOffenders]
solTestXFiltered <- solTestXtrans[, -corrOffenders]

lmSolFiltered1 <- train(solTrainXFiltered, solTrainY, 
                        method = "lm",
                        preProcess = c("center", "scale"),
                        trControl = trainControl(method = "boot",
                                                 number = 500,
                                                 allowParallel = TRUE))

lmSolFiltered2 <- train(solTrainXFiltered, solTrainY, 
                        method = "lm",
                        preProcess = c("center", "scale"),
                        trControl = trainControl(method = "repeatedcv",
                                                 number = 10,
                                                 repeats = 5,
                                                 allowParallel = TRUE))

print(lmSolFiltered1$results)
print(lmSolFiltered2$results)

#' The differences still remain. Stick to one metric throughout the analysis.
#' Repeated CV is a nice mix between rigor and computational complexity.
#' 
#' 2. *PLS* models seek to find the $b$ that maximizes the correlation
#' between $Xb$ and $y$. Hence, PLS build up linear
#' combinations of the features as an intermediate step, building up an
#' alternate basis as a result. It then iteratively solves for the optimal
#' solution that maximizes the correlation with $y$, i.e., 
#' $argmax(cor(Xb, y))$ while at the same time minimizing the
#' least squared error as in the case of a simpel linear model. This is
#' particularly helpful when we have more features than the data.
#' 
#' The hyperarameter that can be tuned through cross-validation is the number of 
#' principal components to use in the model fit. In a more general model setting
#' the tuning parameters are entered into a grid at which the model is run and 
#' the best parameter setting is on where the RMSE is minimum.

plsSol <- train(solTrainXtrans, solTrainY,
                method = "pls",
                preProcess = c("center", "scale"),
                tuneGrid = data.frame(ncomp = 1:30),
                trControl = trainControl(method = "repeatedcv",
                                         number = 10,
                                         repeats = 5, 
                                         allowParallel = TRUE))
print(plsSol$results)
ggplot(data = plsSol$results, aes(x = ncomp, y = RMSE)) + 
  geom_point() +
  geom_line() 

#' 3. *Penalized least squares* models are an extension of the OLS models
#' discussed earlier. In these models, a penalty is added to the 
#' SSE ($ = (y - Xb)^2$) to reduce the variance of the model estimates while
#' increasing the bias. For *ridge regression*, 
#' $$SSE = (y - X\!b)^2 + \lambda \sum_{j = 1}^{P} \! b_j^2$$ We can infer from 
#' this equation that the only way SSE can reduce if
#' we penalize high values of $b$. Thus the ridge regression shrinks
#' the coefficients towards 0. For *lasso* (least absolute shrinkage and 
#' selection operator), 
#' $$SSE = (y - \! Xb)^2 + \lambda \Sigma_{j = 1}^{P} \! |b_j|$$. The elastic
#' net approach combines best of both lasso and ridge regression.
#' 
#' Faced with a choie of models, we resort to cross-validation or bootstrapping 
#' to decide which one to apply to the problem at hand.
#' 

ridgeSol <- train(solTrainXtrans, solTrainY,
                  method = "ridge",
                  preProcess = c("center", "scale"),
                  tuneGrid = data.frame(.lambda = seq(0, 0.1, length.out = 15)),
                  trControl = trainControl(method = "repeatedcv",
                                           number = 10,
                                           repeats = 5,
                                           allowParallel = TRUE))
print(ridgeSol$results)

ggplot(data = ridgeSol$results, aes(x = lambda, y = RMSE)) +
  geom_point() +
  geom_line() +
  labs(x = expression(lambda))

enetSol <- train(solTrainXtrans, solTrainY,
                 method = "enet",
                 preProcess = c("center", "scale"),
                 tuneGrid = expand.grid(.lambda = c(0, 0.01, 0.1),
                                       .fraction = seq(0.05, 1, length.out = 20)),
                 trControl = trainControl(method = "repeatedcv",
                                          number = 10,
                                          repeats = 5,
                                          allowParallel = TRUE))
print(enetSol$results)

#' It is fine and dandy when we are given the tune grid. How does one narrow 
#' the space down to the interesting region?
#' 
#' *Exercise 1* A model for fat content
#' 
#' Analytical chemistry is usually used to calculate the fat content in a
#' substance. As an alternative IR spectroscopy can be used to map the chemical
#' makeup and predict fat content using this data.

data(tecator)

#' There are two data frames with the data:
glimpse(absorp) # absornabce data for 215 samples at 100 frequencies
glimpse(endpoints) # contains % water, fat and protein

#' Since the measurements are measured on a range of frequencies, there are 
#' bound to be correlations between the measurements.

corrOffenders <- findCorrelation(absorp)
cat("The number of highly correlated features are ", length(corrOffenders))

colnames(absorp) <- paste0("F", 1:100)
pcaTecator <- preProcess(absorp, method = c("center", "scale", "pca"), thresh = 0.99)
print(pcaTecator)

#' Output indicates that 2 components were enough to capture 99% of the
#' variance. The data can be transformed to the parameter space of these two
#' principal components by using the predict function

pcaTecatorTrans <- predict(pcaTecator, absorp)

#' These two components cannot really be interpreted. Proceeding now to fit
#' several models to the data.
#' 
#' First, we split the data into train-test

trainingRows <- createDataPartition(endpoints[, 2], p = 0.75, list = FALSE)
absorpTrain <- absorp[trainingRows, ]
absorpTest <- absorp[-trainingRows, ]
fatTrain <- endpoints[trainingRows, 2]
fatTest <- endpoints[-trainingRows, 2]

lmFat <- train(absorpTrain, fatTrain,
               method = "lm",
               preProcess = c("center", "scale"),
               trControl = trainControl(method = "repeatedcv",
                                        number = 10,
                                        repeats = 5, 
                                        allowParallel = TRUE))

#' Given the findings from PCA above, we might try another regression with the
#' PCA conducted before the fit.

lmFatPCA <- train(absorpTrain, fatTrain,
                  method = "lm",
                  preProcess = c("center", "scale", "pca"),
                  trControl = trainControl(method = "repeatedcv",
                                           number = 10, 
                                           repeats = 5,
                                           allowParallel = TRUE))
print(lmFat$results)
print(lmFatPCA$results)

#' Post PCA fit performs horribly, which means the components did not do a good 
#' job at capturing the variance in the outcome.
#' 
#' Hence, it might be better to conduct a PLS fit

plsFat <- train(absorpTrain, fatTrain,
                method = "pls",
                preProcess = c("center", "scale"),
                tuneGrid = data.frame(ncomp = 1:25),
                trControl = trainControl(method = "repeatedcv",
                                         number = 10,
                                         repeats = 5,
                                         allowParallel = TRUE))
print(plsFat$results)

ggplot(data = plsFat$results, aes(x = ncomp, y = RMSE)) +
  geom_point() +
  geom_line()

#' Finally, we try the elastic net. The lasso is a special case when lambda = 0
#' Always include this in the list of parameters

enetFat <- train(absorpTrain, fatTrain,
                 method = "enet",
                 preProcess = c("center", "scale"),
                 tuneGrid = expand.grid(lambda = c(0, 0.001, 0.01),
                                        fraction = seq(0.05, 0.5, length.out = 20)),
                 trControl = trainControl(method = "repeatedcv",
                                          number = 10,
                                          repeats = 5, 
                                          allowParallel = TRUE))

ggplot(data = enetFat$results, 
       aes(x = fraction, y = lambda)) +
  geom_tile(aes(fill = scale(RMSE))) +
  scale_fill_gradient(low = "steelblue", high = "white")

#' Looks like the lasso is the answer among the penalized models.

print(plsFat)
print(enetFat)

#' There is little to choose between PLS and lasso. PLS is marginally better.

#' * Exercise 2 * A model for permeability
#' 
#' The permeabiility of a drug is to be predicted on the basis of its chemical
#' fingerprints. These fingerprints are chemical substructures within the drug

data("permeability")

#' The data is arranged into two parts, the fingerprints object is the model
#' matrix, while the permeability object is the response.
 
glimpse(fingerprints)
glimpse(permeability)

dim(fingerprints)

#' Checking for degenerate predictors

length(nearZeroVar(fingerprints))
fingerprintsNonZeroVar <- fingerprints[, -nearZeroVar(fingerprints)]
dim(fingerprintsNonZeroVar)

#' Still there are more features than samples

trainingRows <- createDataPartition(permeability, p = 0.75, list = FALSE)
XTrain <- fingerprintsNonZeroVar[trainingRows, ]
yTrain <- permeability[trainingRows]
XTest <- fingerprintsNonZeroVar[-trainingRows, ]
yTest <- permeability[-trainingRows]

#' Training a PLS
#' The outcome shows significant skew
#' 
#' For skewed distributions, a simple log transformation can make a significant
#' difference in predictive power

qplot(yTrain, geom = "histogram")

plsPermeability <- train(XTrain, yTrain,
                         method = "pls",
                         preProcess = c("center", "scale"),
                         tuneGrid = data.frame(ncomp = 1:15),
                         trControl = trainControl(method = "repeatedcv",
                                                  number = 10,
                                                  repeats = 5,
                                                  allowParallel = TRUE))

plsLog10Permeability <- train(XTrain, log10(yTrain),
                            method = "pls",
                            preProcess = c("center", "scale"),
                            tuneGrid = data.frame(ncomp = 1:15),
                            trControl = trainControl(method = "repeatedcv",
                                                     number = 10,
                                                     repeats = 5,
                                                     allowParallel = TRUE))

plsLogPermeability <- train(XTrain, log(yTrain),
                            method = "pls",
                            preProcess = c("center", "scale"),
                            tuneGrid = data.frame(ncomp = 1:15),
                            trControl = trainControl(method = "repeatedcv",
                                                     number = 10,
                                                     repeats = 5,
                                                     allowParallel = TRUE))

plsLog10Permeability2 <- train(XTrain, log10(yTrain),
                               method = "pls",
                               tuneGrid = data.frame(ncomp = 1:15),
                               trControl = trainControl(method = "LGOCV",
                                                        allowParallel = TRUE))

print(plsPermeability$results)
print(plsLog10Permeability$results)
print(plsLogPermeability$results)
print(plsLog10Permeability2$results)

ggplot(plsPermeability$results, aes(x = ncomp, y = RMSE)) +
  geom_point() +
  geom_line()

ggplot(plsLogPermeability$results, aes(x = ncomp, y = RMSE)) +
  geom_point() +
  geom_line()

ggplot(plsPermeability$results, aes(x = ncomp, y = Rsquared)) +
  geom_point() +
  geom_line()

ggplot(plsLog10Permeability2$results, aes(x = ncomp, y = Rsquared)) +
  geom_point() +
  geom_line()

yPred <- predict(plsLog10Permeability, XTest)

qplot(log10(yTest), yPred, geom = "point") +
  geom_abline(aes(slope = 1, intercept = 0)) +
  labs(x = "log10(Observed)",
       y = "log10(Predicted)")

enetPermeability <- train(XTrain, yTrain,
                          method = "enet",
                          preProcess = c("center", "scale"),
                          tuneGrid = expand.grid(lambda = c(0, 0.05, 0.1),
                                                 fraction = seq(0.05, 1, length.out = 20)),
                          trControl = trainControl(method = "repeatedcv",
                                                   number = 10,
                                                   repeats = 5))

enetPermeability$results %>% as.data.frame() %>% 
                             group_by(lambda) %>% 
                             select(lambda, fraction, Rsquared) %>% 
                             ggplot(aes(fraction, Rsquared)) +
                              geom_point(aes(shape = factor(lambda))) +
                              geom_line(aes(group = lambda))

#' Now we need to choose between the elastic net and the PLS

print(plsLog10Permeability)
print(enetPermeability)

#' Elastic net wins on Rsquared
#'
#' *Exercise 3*
#'
#' There is no data for this problem
