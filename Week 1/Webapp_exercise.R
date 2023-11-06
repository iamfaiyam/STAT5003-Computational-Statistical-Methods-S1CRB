### STAT5003 - Week01 - WebApp Exercise

# install the AER package (once) 
# install.packages("AER")

# load the AER package
library(AER)

# load the dataset in the workspace
data(CASchools) 

class(CASchools) 

head(CASchools)

# compute STR and append it to CASchools
CASchools$STR <- CASchools$students/CASchools$teachers

# compute TestScore and append it to CASchools
CASchools$score <- (CASchools$read + CASchools$math)/2

# compute sample averages of STR and score
avg_STR <- mean(CASchools$STR) 
avg_score <- mean(CASchools$score) 

# compute sample standard deviations of STR and score
sd_STR <- sd(CASchools$STR) 
sd_score <- sd(CASchools$score) 

# set up a vector of percentiles and compute the quantiles
quantiles <- c(0.10, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9) 
quant_STR <- quantile(CASchools$STR, quantiles) 
quant_score <- quantile(CASchools$score, quantiles) 

# gather everything in a data.frame
DistributionSummary <- data.frame(Average = c(avg_STR, avg_score), 
                                  StandardDeviation = c(sd_STR, sd_score), 
                                  quantile = rbind(quant_STR, quant_score))

# print the summary to the console
DistributionSummary

plot(score ~ STR, 
     data = CASchools, 
     main = "Scatterplot of TestScore and STR", 
     xlab = "STR (X)", 
     ylab = "Test Score (Y)")

cor(CASchools$STR, CASchools$score) 

# OLS for Linear Regression 
# The command below enables us to address a variable contained in 
# CASchools by its name: it is no longer necessary to use the $
# operator in conjunction with the dataset: R may evaluate the 
# variable name directly 
attach(CASchools) 

# compute beta_1_hat
beta_1 <- sum((STR - mean(STR)) * (score - mean(score))) / sum((STR - mean(STR))^2)

# compute beta_0_hat
beta_0 <- mean(score) - beta_1 * mean(STR) 

# print the results to the console
beta_1 

beta_0

# estimate the model and assign the result to linear_model 
linear_model <- lm(score ~ STR, data = CASchools) 

# print the standard output of the estimated lm object to the console
linear_model 

# plot the data 
plot(score ~ STR, 
     data = CASchools, 
     main = "Scatterplot of TestScore and STR", 
     xlab = "STR (X)",
     ylab = "Test Score (Y)", 
     xlim = c(10, 30), 
     ylim = c(600, 720))

# add the regression line
abline(linear_model) 














