#' ---
#' title: "Preprocessing Data"
#' author: Pavan Gurazada
#' date: "February 2018"
#' output: github_document
#' ---

library(AppliedPredictiveModeling)
library(caret)
library(tidyverse)

library(mlbench)

set.seed(20130810)
theme_set(theme_bw())
dev.new()

#' Here we are concerned about the preprocessing of features only. The outcome
#' is kept out of the discussion.
#'
#' Preprocessing is usually referred to as 'feature engineering' in practise.
#'
#' All feature engineering decisions are made based on the training data and
#' applied to test data.
#'
#' The usual suspects for feature engineering are:
#'
#' 1. Centering and scaling
#'
#' 2. Box-Cox transformations with lambda derived using MLE
#'
#' 3. Outlier treatment using spatial sign tranformation
#'
#' 4. PCA for reduction to a small subset of orthogonal features. The number of
#' features eventually selected is based on a scree plot or using
#' cross-validation
#'
#' 5. Removal of features with near-zero variance
#'
#' 6. Removal of features exhibiting multicollinearity. Easiest way to deal with
#' this is to assert that the pairwise correlation between any two features is
#' below a certain threshold (say 0.75)
#'
#' 7. Addition of dummy variables to split categorical data (one dummy variable
#' per category, unless it is a regression setup)
#'
#' Another pesky issue in feature engineering is the handling of missing values.
#' Always check if missing values in a data set are concentrated in a small
#' subset of predictors. This may be informative about the data generation
#' mechanism. Based on an anlysis of missing values, it might be beneficial to
#' either remove the offending features or the offending samples.
#'
#' In case a decision has been made to not remove missing values, the first pass
#' would be to see if the features with tons of missing values are correlated
#' with any of those that do not have missing values. If this also does not
#' work, an imputation model needs to be applied to infer the missing values
#' from available data. This needs to be done with care and incorporated into
#' the model parameter tuning too.
#'
#' A widely applied method for imputation is K-Nearest Neighbors. Note that both
#' the number of nearest neighbors and the distance metric are parameters in
#' this case.
#' 

data("segmentationOriginal")
glimpse(segmentationOriginal)

#' The code below performs the followng steps:
#' 
#' 1. Filter the training data
#' 
#' 2. Remove the predictor and sample id features
#' 
#' 3. Remove redundant columns that contain "Status" in their name 

segTrain <- segmentationOriginal %>% filter(Case == "Train") %>% 
                                     select(-Cell, -Class, -Case) %>% 
                                     select(-contains("Status"))

#' Next we apply a pre-processing pipeline to the training data, do a box-cox
#' transform, center and scale the data and apply a PCA. The output is a model
#' object.

trans <- segTrain %>% preProcess(method = c("BoxCox", "center", "scale", "pca"))

#' These transformations need to be applied to the data

transSegTrain <- predict(trans, segTrain)
glimpse(transSegTrain) # you should onl see the 19 principal components here

#' This highlights a disadvantage of PCA, i.e., the loss of interpretability of
#' features.
#'
#' PCA essentially projects the data onto a vector pointed in the direction of
#' maximum variance. Starting with a feature matrix $\mathbf{X}$, we wish to
#' transform this matrix to a matrix $\mathbf{Y}$ such that its covariance
#' matrix of $\mathbf{C_Y}$ has the following nice properties:
#'
#' - All off-diagonal elements (corresponding to the covariance terms) are 0
#'
#' - Each successive dimension in $\mathbf{Y}$ should be ordered according to
#' variance (diagonal terms of $\mathbf{C_Y}$)
#'
#' The plan of action in solving the PCA problem is to realize that any
#' symmetric matrix is diagonalized by an orthonormal matrix of its
#' eigenvectors. Hence, the first direction of maximum variance is along the
#' eigen vector corresponding to the largest eigen value and so on.
#'
#' An alternative pathway is to remove zero variance features and features that
#' are highly correlated

#' Check for degenerate features
length(nearZeroVar(segTrain))> 0

#' Check for highly correlated features
#' Compute the correlation among all features, then filter beyond a cutoff

highCorrVars <- cor(segTrain) %>% findCorrelation(cutoff = 0.75)
segTrain <- segTrain %>% select(-highCorrVars)
glimpse(segTrain)

#' Exercise 3.1

data("Glass")
glimpse(Glass)

#' Gather all the features into a column "Variable" and its corresponding value
#' into a column "Value". We leave the Type column alone. We can then plot the
#' histogram of the variables at once using a facet wrap and compare values

meltedGlass <- Glass %>% select(-Type) %>%  
                         gather("Variable", "Value", 1:9)
glimpse(meltedGlass)

ggplot(meltedGlass) +
  geom_histogram(aes(x = Value)) + 
  facet_wrap(~Variable)

#' The plot shows signs of both bimodality and skewness.

#' Check for degenerate features
length(nearZeroVar(Glass)) > 0 # No problem here

#' We look for highly correlated predictors

highCorrVars <- Glass %>% select(-Type) %>% 
                          cor() %>% 
                          findCorrelation(cutoff = 0.75)

#' This is suggesting that removing Ca might be a good option since it is highly 
#' correlated with others

#' Many zeros exist in the data, hence Yeo-Johnson transformation might be better

yjTrans <- Glass %>% select(-Type) %>% 
                     preProcess(method = "YeoJohnson")

yjTransData <- predict(yjTrans, Glass[, -10]) 
glimpse(yjTransData)

#' Now we relook at the distribution of the variables

meltedYJTransData <- yjTransData %>% gather("Variable", "Value", 1:9)
ggplot(meltedYJTransData) +
  geom_histogram(aes(x = Value)) +
  facet_wrap(~Variable)

#' Does not seem to have a big difference

#' Moving on to spatial sign transformation for outliers

spatSignTrans <- Glass %>% select(-Type) %>% 
                           preProcess(method = c("center", "scale", "spatialSign"))
ssData <- predict(spatSignTrans, Glass[, -10])
glimpse(ssData)

#' Exercise 3.2

data("Soybean")
glimpse(Soybean)

#' Are the missing values concentrated among few predictors? Here is a helper
#' function that computes the missing values percentage by variable

naPercentage <- function(df) {
  percentage <- floor(colSums(is.na(df))*100/dim(df)[1])
  return(percentage)
}

#' Another alternative is to use dplyr; one of the rare occassions where 
#' it is a bit ugly

Soybean %>% select_if(function(x) any(is.na(x))) %>% 
            summarize_all(funs(sum(is.na(.))*100/length(.)))

#' We can look at the distribution of NAs by Class. This will help decide if we need
#' to exclude certain variables

#' What is the percentage of missing values for each predictor by class?

naByPredByClass <- Soybean %>% gather("Variable", "Value", -Class) %>% 
                               group_by(Class, Variable) %>% 
                               filter(is.na(Value)) %>% 
                               summarize_at("Value", funs(sum(is.na(.)))) 
glimpse(naByPredByClass)

#' There are loads of missing values, scattered among mainly three classes
#' How would one do imputation here?
