library(AppliedPredictiveModeling)
library(caret)
library(tidyverse)


set.seed(20130810)
theme_set(theme_bw())
dev.new()

data <- data.frame(observed = c(0.22, 0.83, -0.12, 0.89, -0.23, -1.3, -0.15, -1.4,
                                0.62, 0.99, -0.18, 0.32, 0.34, -0.3, 0.04, -0.87,
                                0.55, -1.3, -1.15, 0.2),
                   predicted = c(0.24, 0.78, -0.66, 0.53, 0.7, -0.75, -0.41, -0.43, 
                                 0.49, 0.79, -1.19, 0.06, 0.75, -0.07, 0.43, -0.42,
                                 -0.25, -0.64, -1.26, -0.07))


data %>% mutate(residualValues = observed - predicted) %>% 
         summarize_at("residualValues", funs(mean, median, sd))

axisRange <- extendrange(c(observed, predicted))

ggplot(data, aes(x = observed, y = predicted)) +
  geom_point(size = 2) +
  geom_abline(aes(intercept = 0, slope = 1), linetype = "dashed", size = 1) +
  scale_x_continuous(limits = axisRange) + 
  scale_y_continuous(limits = axisRange)

data <- data %>% mutate(residualValues = observed - predicted)

ggplot(data, aes(x = predicted, y = residualValues)) +
  geom_point(size = 2) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", size = 1) +
  scale_x_continuous(limits = axisRange) + 
  scale_y_continuous(limits = axisRange)

# In sum, use RMSE, do not take R2 seriously and plot residuals

with(data, R2(predicted, observed))
with(data, RMSE(predicted, observed))

cor(predicted, observed) # correlation
cor(predicted, observed, method = "spearman") # rank correlation 

