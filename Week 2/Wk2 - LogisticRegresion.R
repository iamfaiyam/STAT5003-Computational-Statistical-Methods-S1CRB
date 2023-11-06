### Libraries to load 
library(ggplot2) 
library(tidyverse)
library(mlbench)

data("BreastCancer", package = "mlbench")
? mlbench::BreastCancer
head(BreastCancer)

# Checking how many samples there are in this dataset
dim(BreastCancer) 

# Finding the levels of target class
levels(BreastCancer$Class) 

# Let's just remove samples with missing values
BreastCancer.complete <- BreastCancer[complete.cases(BreastCancer),]
dim(BreastCancer.complete) 

# Or alternatively use `tidyr::dropna`
BreastCancer <- BreastCancer %>% drop_na
dim(BreastCancer) 

## Logistic Regression 
? glm
? family
str(BreastCancer) 

# Now run logistic regression model 
logistic.model <- glm(Class ~ as.numeric(Cell.size), 
                      data = BreastCancer, 
                      family = binomial(link = 'logit'))
summary(logistic.model) 

# Visualise the results of the logistic regression, see how
# the fitted values are between 0 and 1 
plot(BreastCancer$Cell.size, logistic.model$fitted.values) 

# Predit methods
predict(logistic.model) |> head() 
pred.classes <- ifelse(predict(logistic.model) > 0, "malignant", "benign")
pred.classes.2 <- ifelse(predict(logistic.model, type = "response") > 0.5, 
                         "malignant", "benign")
# calculate classification accuracy (in percentage %)
mean(pred.classes == BreastCancer$Class) * 100

