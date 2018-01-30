library(AppliedPredictiveModeling)
library(caret)
library(tidyverse)

set.seed(20130810)
theme_set(theme_bw())
dev.new()

# A good model does not memorize, it learns

# Tests for the learning ability of a model are achieved by cross validation
# Most models have at least one parameter or a modelign choice that cannot be 
# determined from the data

# A general approach is to begin with a wide parameter space and narrow down the
# space using cross-validation
# The typical steps in a model building process are:
# 1. Pre-process the data
# 2. Estimate model parameters
# 3. Set up and execute the model
# 4. Evaluate model performance
# 5. Fine tune results/classification 

# When splitting a dataset it is preferable to ensure similarity between the train
# and test set. The train set can be efficiently used by further splitting it into
# a train and validation set. This can be achieved by generating k-folds (with or 
# without repetition) or leave-one-out data sets. We sacrifice computation time as 
# the amount of resampling increases. The best way to manage both the bias and 
# computational burden is to ensure repeated k-fold cross validation.
# Bootstrapping the data and validation over the out-of-bag samples is another
# method to achieve similar results. This book prefers repeated 10-fold 
# cros-validation as a general rule that is performant in a wide variety of 
# scenarios. This is rather nice because the method is easy to understand and tune.
# In case we are choosing between models, bootstrapping is an ideal solution, due 
# to their low variance. As sample sizes increase, computational constraints 
# overpower the difference between different resampling procedures.

# Choosing the numerically optimum solution (i.e., the one with the best cross-validation
# score) might lead to complex models. In such instances, we search for a simpler 
# model with a score no less than 1 SE away from the numericaly optimum solution.
# In general, choose a model which is less opaque and not far off in terms of 
# accuracy. Use the best methods to obtain performance ceilings and then push the 
# simpler models to go as close as possible to these ceilings.

data(twoClassData)
glimpse(predictors)
glimpse(classes)

# The following exploration is useful when we want fine grained control over the
# resampling process. For everyday use pipelines are used.

# Multiple resampled data partitions can be generated
# Repeated splits
trainingRows <- createDataPartition(classes, 
                                    p = 0.8,
                                    list = TRUE, 
                                    times = 4)
glimpse(trainingRows)

# Single split
trainingRows <- createDataPartition(classes,
                                    p = 0.8,
                                    list = FALSE)

trainPredictors <- predictors %>% slice(trainingRows)
trainClasses <- classes[trainingRows]

testPredictors <- predictors %>% slice(-trainingRows)
testClasses <- classes[-trainingRows]

# Resamples for bootstrapping
trainingRows <- createResample(classes)

# Create folds of the training data
cvSplits <- createFolds(trainClasses,
                        k = 10,
                        returnTrain = TRUE)
glimpse(cvSplits)

# For model fit, R has two interfaces. First is the formula interface that specifies
# the mathematical relationship between the features (X) and the outcome (y) using 
# the notation y ~ f(X). Second is the matrix interface that directly takes the 
# model matrix X and the outcome y as inputs. Not all R functions have both options,
# but the model matrix approach is more efficient in storage. The story of how ~ 
# is parsed internally is a different story, and is worth exploring since it is 
# embedded deep within the R interface to modeling

# Once data has been split, we can fit models and use this to make predictions

# We fit a k-nearest neighbors classification using a single resample split
# Then we use the fit to predict on the test set

trainPredictors <- as.matrix(trainPredictors)
knnFit <- knn3(x = trainPredictors,
               y = trainClasses,
               k = 5)
testPredictions <- predict(knnFit, testPredictors, type = "class")

# Note that the number of nearest neighbors here is a tuning parameter that cannot 
# be estimated from the data. Ergo, we need cross validation

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

# These seem to throw up more questions than answers

# Intuitively, using the tuneGrid makes a lot of sense since we cna have a
# controlled parameter space over which the modeling is done. Once the model is 
# fit on the grid, one can choose the simplest model (?) based on the one SD rule

# When this script was run, the accuracy for the best fit k = 58 is 0.7754020, with 
# a SD of 0.0871577. Hence, minimum acceptable accuracy is 0.7754020 - 0.0871577
# = 0.6882443, at which k = 3 is also acceptable!

# Does this mean that we are chasing the wrong model for this data?

# GERMAN CREDIT SVM FIT

data("GermanCredit")
glimpse(GermanCredit)

trainingRows <- createDataPartition(GermanCredit$Class,
                                    p = 0.8,
                                    list = FALSE)

germanCreditTrain <- GermanCredit %>% slice(trainingRows)
germanCreditTest <- GermanCredit %>% slice(-trainingRows)

# Check for degenerate features and exclude them

if (length(nearZeroVar(germanCreditTrain)) > 0) {
  germanCreditTrain <- germanCreditTrain %>% select(-nearZeroVar(germanCreditTrain))
}

glimpse(germanCreditTrain)

# Check for highly correlated features and exclude them
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
                tuneGrid = expand.grid(C = seq(0.25, 128, length.out = 25),
                                       sigma = seq(0.01, 0.04, length.out = 25)))

svmFit$results
plot(svmFit, scales = list(x = list(log = 2)))
