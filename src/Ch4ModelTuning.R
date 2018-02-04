
#' ---
#' title: "Model Tuning"
#' author: Pavan Gurazada
#' date: "February 2018"
#' output: github_document
#' ---

library(AppliedPredictiveModeling)
library(caret)
library(tidyverse)

set.seed(20130810)
theme_set(theme_bw())
dev.new()

#' A good model does not memorize, it learns

#' Tests for the learning ability of a model are achieved by cross validation. 
#' Most models have at least one parameter or a modeling choice that cannot be 
#' determined from the data.

#' A general approach is to begin with a wide parameter space and narrow down
#' the space using cross-validation
#'
#' The typical steps in a model building process are:
#'
#' 1. Pre-process the data
#'
#' 2. Estimate model parameters
#'
#' 3. Set up and execute the model
#'
#' 4. Evaluate model performance
#'
#' 5. Fine tune results/classification
#'
#' When splitting a dataset it is preferable to ensure similarity between the
#' train and test set. The train set can be efficiently used by further
#' splitting it into a train and validation set. This can be achieved by
#' generating k-folds (with or without repetition) or leave-one-out data sets.
#' We sacrifice computation time as the amount of resampling increases. The best
#' way to manage both the bias and computational burden is to ensure repeated
#' k-fold cross validation.
#'
#' Bootstrapping the data and validation over the out-of-bag samples is another
#' method to achieve similar results. This book prefers repeated 10-fold
#' cros-validation as a general rule that is performant in a wide variety of
#' scenarios. This is rather nice because the method is easy to understand and
#' tune. In case we are choosing between models, bootstrapping is an ideal
#' solution, due to their low variance. As sample sizes increase, computational
#' constraints overpower the difference between different resampling procedures.
#'
#' Choosing the numerically optimum solution (i.e., the one with the best
#' cross-validation score) might lead to complex models. In such instances, we
#' search for a simpler model with a score no less than 1 SE away from the
#' numericaly optimum solution. In general, choose a model which is less opaque
#' and not far off in terms of accuracy. Use the best methods to obtain
#' performance ceilings and then push the simpler models to go as close as
#' possible to these ceilings.

data(twoClassData)
glimpse(predictors)
glimpse(classes)

#' The following exploration is useful when we want fine grained control over
#' the resampling process. For everyday use pipelines are used.
#'
#' *Repeated splits*

trainingRows <- createDataPartition(classes, 
                                    p = 0.8,
                                    list = TRUE, 
                                    times = 4)
glimpse(trainingRows)

#' *Single split*

trainingRows <- createDataPartition(classes,
                                    p = 0.8,
                                    list = FALSE)

trainPredictors <- predictors %>% slice(trainingRows)
trainClasses <- classes[trainingRows]

testPredictors <- predictors %>% slice(-trainingRows)
testClasses <- classes[-trainingRows]

#' *Resamples for bootstrapping*

trainingRows <- createResample(classes)

#' *Create folds of the training data*

cvSplits <- createFolds(trainClasses,
                        k = 10,
                        returnTrain = TRUE)
glimpse(cvSplits)

#' For model fit, R has two interfaces. First is the formula interface that
#' specifies the mathematical relationship between the features (X) and the
#' outcome (y) using the notation y ~ f(X). Second is the matrix interface that
#' directly takes the model matrix X and the outcome y as inputs. Not all R
#' functions have both options, but the model matrix approach is more efficient
#' in storage. The story of how ~ is parsed internally is a different story, and
#' is worth exploring since it is embedded deep within the R interface to
#' modeling. This exploration is presented in a separate script and also in the
#' code for Chapter 6.
#'
#' Once data has been split, we can fit models and use this to make predictions.
#'
#' In general, ask yourself two questions. Are there any hyperparametes of the
#' model that need to be estimated? How should the cross validation be done?
#'
#' We fit a k-nearest neighbors classification using a single resample split.
#' Then we use the fit to predict on the test set

trainPredictors <- as.matrix(trainPredictors)
knnFit <- knn3(x = trainPredictors,
               y = trainClasses,
               k = 5)
testPredictions <- predict(knnFit, testPredictors, type = "class")

#' Note that the number of nearest neighbors here is a tuning parameter that
#' cannot be estimated from the data. Ergo, we need cross validation

knnFit <- train(x = trainPredictors, y = trainClasses,
                method = "knn",
                trControl = trainControl(method = "repeatedcv",
                                         number = 10,
                                         repeats = 5,
                                         search = "grid"))
knnFit$results

knnFit <- train(x = trainPredictors, y = trainClasses,
                method = "knn",
                trControl = trainControl(method = "repeatedcv",
                                         number = 10,
                                         repeats = 5,
                                         search = "random"))
knnFit$results

knnFit <- train(x = trainPredictors, y = trainClasses,
                method = "knn",
                trControl = trainControl(method = "repeatedcv",
                                         number = 10,
                                         repeats = 5),
                tuneGrid = data.frame(k = 2:100))
knnFit$results

#' These seem to throw up more questions than answers.

#' Intuitively, using the tuneGrid makes a lot of sense since we can have a
#' controlled parameter space over which the modeling is done. Once the model is
#' fit on the grid, one can choose the simplest model (?) based on the one SD
#' rule
#'
#' When this script was run, the accuracy for the best fit k = 58 is 0.7754020, with 
#' a SD of 0.0871577. Hence, minimum acceptable accuracy is 0.7754020 - 0.0871577
#' = 0.6882443, at which k = 3 is also acceptable!
#'
#' Does this mean that we are chasing the wrong model for this data?
#'
#' **GERMAN CREDIT SVM FIT**

data("GermanCredit")
glimpse(GermanCredit)

trainingRows <- createDataPartition(GermanCredit$Class,
                                    p = 0.8,
                                    list = FALSE)

germanCreditTrain <- GermanCredit %>% slice(trainingRows)
germanCreditTest <- GermanCredit %>% slice(-trainingRows)

#' Check for degenerate features and exclude them

if (length(nearZeroVar(germanCreditTrain)) > 0) {
  germanCreditTrain <- germanCreditTrain %>% select(-nearZeroVar(germanCreditTrain))
}

glimpse(germanCreditTrain)

#' Check for highly correlated features and exclude them
highCorrVars <- germanCreditTrain %>% select(-Class) %>% 
                                      cor() %>% 
                                      findCorrelation(cutoff = 0.75)  
germanCreditTrain <- germanCreditTrain %>% select(-highCorrVars)

svmFit <- train(Class ~ .,
                data = germanCreditTrain,
                method = "svmRadial",
                preProcess = c("center", "scale"),
                trControl = trainControl(method = "repeatedcv",
                                         number = 10,
                                         repeats = 5),
                tuneLength = 10)

svmFit$results
plot(svmFit, scales = list(x = list(log = 2)))

svmFit <- train(Class ~ .,
                data = germanCreditTrain,
                method = "svmRadial",
                preProcess = c("center", "scale"),
                trControl = trainControl(method = "repeatedcv",
                                         number = 10,
                                         repeats = 5),
                tuneGrid = expand.grid(C = seq(0.25, 128, length.out = 5),
                                       sigma = seq(0.01, 0.04, length.out = 5)))

svmFit$results
plot(svmFit, scales = list(x = list(log = 2)))
print(svmFit)

#' **GERMAN CREDIT LOGISTIC FIT**

logisticFit <- train(Class ~ .,
                     data = germanCreditTrain,
                     method = "glm",
                     trControl = trainControl(method = "repeatedcv",
                                              number = 10,
                                              repeats = 5))

summary(logisticFit)

#' There seem to be some offending columns that are prompting warnings. Lets
#' try excluding them

logisticFit <- train(Class ~ .,
                     data = germanCreditTrain %>% select(-CheckingAccountStatus.none, 
                                                         -EmploymentDuration.Unemployed, 
                                                         -Personal.Male.Married.Widowed),
                     method = "glm",
                     trControl = trainControl(method = "repeatedcv",
                                              number = 10,
                                              repeats = 5))
logisticFit$results

#' Now comparing models

resamp <- resamples(list(SVM = svmFit, Logistic = logisticFit))
summary(resamp)

#' No statistical test required to see that the logsitic regression is as good
#' as a more complex support vector machine

#' Exercise 4.2

data("permeability")
glimpse(permeability)

ggplot(as.data.frame(permeability), aes(x = permeability)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 5) + # Need to grok this double dots!
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Permeability",
       y = "Percent of total")

#' Exercise 4.3

data("ChemicalManufacturingProcess")
glimpse(ChemicalManufacturingProcess)

#' Check for degenerate features

length(nearZeroVar(ChemicalManufacturingProcess)) > 0 

chemManufactProcessData <- ChemicalManufacturingProcess %>% select(-nearZeroVar(ChemicalManufacturingProcess)) 

#' Quick check on overall extent of missingness

sum(is.na(chemManufactProcessData))

chemManufactProcessData %>% select_if(function(x) any(is.na(x))) %>% 
                            summarize_all(funs(sum(is.na(.))*100/length(.)))

#' All missing values are concentrated in the manufacturing process features.
#' No missing proportions are alarmingly high.
#' Lets impute the missing values using k-nearest neighbors; we use 10-fold 
#' crossvalidation with 5 repeats as usual to infer the value of k. We then 
#' apply the imputer to the data and check that there are no more missing 
#' values

naImputer <- preProcess(chemManufactProcessData, 
                        method = "knnImpute",
                        trControl = trainControl(method = "repeatedcv",
                                                 number = 10,
                                                 repeats = 5), 
                        tuneGrid = data.frame(k = 2:10))

chemManufactProcessData <- predict(naImputer, chemManufactProcessData)
if (sum(is.na(chemManufactProcessData)) == 0) {
  cat("Success! No more missing Values\n")
}

#' Check for highly correlated features 

highCorrVars <- chemManufactProcessData %>% cor() %>% 
                                            findCorrelation(cutoff = 0.75)

chemManufactProcessData <- chemManufactProcessData %>% select(-highCorrVars)
glimpse(chemManufactProcessData)

#' Plot to see if there is any skew in the predictors

chemManufactProcessData %>% gather("Variable", "Value", 2:37) %>% 
                            ggplot(aes(x = Value)) +
                             geom_histogram(binwidth = 5) +
                             facet_wrap(~Variable)

plsProfileChemFit <- train(Yield ~ .,
                           data = chemManufactProcessData,
                           method = "pls",
                           preProcess = c("center", "scale"),
                           tuneLength = 10,
                           trControl = trainControl(method = "repeatedcv",
                                                    number = 10,
                                                    repeats = 5))
R2Values <- plsProfileChemFit$results %>% select(ncomp, Rsquared, RsquaredSD) %>% 
                                          mutate(RsquaredSE = RsquaredSD/sqrt(length(plsProfileChemFit$control$index)))

ggplot(R2Values, aes(x = ncomp, y = Rsquared)) +
  geom_point() +
  geom_errorbar(aes(ymin = Rsquared - RsquaredSE, ymax = Rsquared + RsquaredSE), width = 0.1)

#' Now, we fit a succession of models, with little regard for model hyperparameters

plsChemFit <- train(Yield ~ .,
                    data = chemManufactProcessData,
                    method = "pls",
                    preProcess = c("center", "scale"),
                    tuneLength = 10,
                    trControl = trainControl(method = "repeatedcv",
                                             number = 10,
                                             repeats = 5))

lmChemFit <- train(Yield ~ .,
                   data = chemManufactProcessData,
                   method = "lm",
                   preProcess = c("center", "scale"),
                   trControl = trainControl(method = "repeatedcv",
                                            number = 10,
                                            repeats = 5))
svmChemFit <- train(Yield ~ .,
                    data = chemManufactProcessData,
                    method = "svmLinear",
                    preProcess = c("center", "scale"),
                    tuneLength = 10,
                    trControl = trainControl(method = "repeatedcv",
                                             number = 10,
                                             repeats = 5))
rfChemFit <- train(Yield ~ .,
                   data = chemManufactProcessData,
                   method = "rf",
                   preProcess = c("center", "scale"),
                   tuneLength = 10,
                   trControl = trainControl(method = "repeatedcv",
                                            number = 10,
                                            repeats = 5))
#' Exercise 4.4

data(oil)
glimpse(oilType)

#' This is a nice puzzle. It illustrates the heavy lifting that is done by 
#' createDataPartition()

oilType %>% table() %>% prop.table()
sample(oilType) %>% table() %>% prop.table() # full sample

#' Create a data partition with 60-40 split, using sample.
#' We create 1000 such resamples and check the variability

sampleSize <- floor(0.6*length(oilType))
numResamples <- 1000
randDataPartitions <- matrix(nrow = numResamples, ncol = 7)
colnames(randDataPartitions) <- LETTERS[1:7]

for (sample in 1:numResamples){
  randDataPartitions[sample, ] <- sample(oilType, size = sampleSize, replace = FALSE) %>% 
                                  table() %>% 
                                  prop.table()
}


trainingRows <- createDataPartition(oilType, p = 0.6, times = numResamples)

caretDataPartitions <- matrix(nrow = numResamples, ncol = 7)
colnames(caretDataPartitions) <- LETTERS[1:7]

for (sample in 1:numResamples) {
  caretDataPartitions[sample, ] <- oilType[trainingRows[[sample]]] %>% 
                                   table() %>% 
                                   prop.table()
}

#' Original Data
oilType %>% table() %>% prop.table()

#' Random sampling
randDataPartitions %>% as.data.frame() %>% 
                       summarize_all(funs(mean, sd))

#' Stratified sampling using caret

caretDataPartitions %>% as.data.frame() %>% 
                        summarize_all(funs(mean, sd))

#' There is no variability in the data partitions created by caret!

