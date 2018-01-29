library(AppliedPredictiveModeling)
library(caret)
library(tidyverse)

set.seed(20130810)
theme_set(theme_bw())


# Here we are concerned about the preprocessing of features only. The outcome is
# kept out of the discussion. Essentially, these are unsupervised techniques
# Preprocessing is usually referred to as 'feature engineering' in practise

# All feature engineering decisions are made based on the training data and applied
# to test data

# The usual suspects for feature engineering are:
# 1. Centering and scaling 
# 2. Box-Cox transformations with lambda derived using MLE
# 3. Outlier treatment using spatial sign tranformation
# 4. PCA for reduction to a small subset of orthogonal features.
#    The number of features eventually selected is based on a scree plot or using
#    cross-validation
# 5. Removal of features with near-zero variance
# 6. Removal of features exhibiting multicollinearity. Easiest way to deal with this
#    is to assert that the pairwise correlation between any two features is below
#    a certain threshold (say 0.75)
# 7. Addition of dummy variables to split categorical data (one dummy variable per
#    category, unless it is a regression setup)

# Another pesky issue in feature engineering is the handling of missing values. Always check
# if missing values in a data set are concentrated in a small subset of predictors.
# This may be informative about the data generation mechanism. Based on an anlysis 
# of missing values, it might be beneficial to either remove the offending features
# or the offending samples. 
# In case a decision has been made to not remove missing values, the first pass would
# be to see if the features with tons of missing values are correlated with any of 
# those that do not have missing values. If this also does not work, an imputation model
# needs to be applied to infer the missing values from available data. This needs to
# be done with care and incorporated into the model parameter tuning too. 
# A widely applied method for imputation is K-Nearest Neighbors. Note that both 
# the number of nearest neighbors and the distance metric are parameters in this
# case. 
# 
# Computation begins here...

data("segmentationOriginal")
glimpse(segmentationOriginal)

# Filter the training data,
# remove the predictor and sample id features,
# remove redundant columns that contain "Status" in their name 

segTrain <- segmentationOriginal %>% filter(Case == "Train") %>% 
                                     select(-Cell, -Class, -Case) %>% 
                                     select(-contains("Status"))

# Apply a pre-processing pipeline to the training data
# do a box-cox transform, center and scale the data and apply a PCA.
# The output is a model object.

trans <- segTrain %>% preProcess(method = c("BoxCox", "center", "scale", "pca"))

# These transformations need to be applied to the data

transSegTrain <- predict(trans, segTrain)
glimpse(transSegTrain) # you should onl see the 19 principal components here

# This highlights a disadvantage of PCA, i.e., the loss of interpretability of 
# features

# An alternative pathway is to remove zero variance features and features that
# are highly correlated

# Check for degenerate features
length(nearZeroVar(segTrain))> 0

# Check for highly correlated features
# Compute the correlation among all features, then filter beyond a cutoff

highCorrVars <- cor(segTrain) %>% findCorrelation(cutoff = 0.75)
segTrain <- segTrain %>% select(-highCorrVars)
glimpse(segTrain)

