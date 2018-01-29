library(AppliedPredictiveModeling)
library(caret)
library(tidyverse)

set.seed(20130810)
theme_set(theme_bw())

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
# method to achieve similar results. This book prefers 10-fold cros-validation
# as a general rule that is performant in a wide variety of scenarios. This is rather 
# nice because the method is easy to understand and tune.





