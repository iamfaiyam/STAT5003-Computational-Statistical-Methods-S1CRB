# Intro to Support Vector Machines
library(e1071)

# create example data
f1 <- c(.5, 1, 1, 2, 3, 3.5, 1, 3.5, 4, 5, 5.5, 6) 
f2 <- c(3.5, 1, 2.5, 2, 1, 1.2, 5.8, 3, 4, 5, 4, 1) 
cls <- c(rep(+1, 6), rep(-1, 6))
dat <- cbind(f1, f2) 

# plot all points from the two classes 
plot(dat, col = (cls + 3)/2, pch = 19, xlim = c(-1, 6), ylim = c(-1, 6))

# train a maximal margin classifier (hard margin) 
svm.model <- svm(dat, y = cls, kernel = "linear", type = "C-classification", scale = FALSE) 


# plot support vectors
points(svm.model$SV, col = "blue", cex = 2) 
text(svm.model$SV + 0.2, labels = row.names(svm.model$SV)) # Could use labels here with 
# svm.model$index

# coefs: estimated betas
# svm.model$ceofs gives the alpha weights 
# svm.model$SV gives the support vectors
# The hyperplane f(x) = beta_0 + \sum{i in S} alpa_i <x, support_vector_i> 
# alpha = svm.model$coefs
# support_vector = svm.model$SV
# Compute the weights
w <- t(svm.model$coefs) %*% svm.model$SV
# ----- This is beyond the scope of the course, but for interests -----
# rho: the negative intercept of decision boundary 
# Extract the beta_0 
beta_0 <- -svm.model$rho 
# Remap plane, i.e. equation currently in form f(x) = 0 = a + bx + cy 
# where w = (b, c) and a = beta_0 
# We want y = -a/c - a/c * x to use abline
abline(a = -beta_0 / w[1, 2], b = -w[1, 1] / w[1, 2], col = "black", lty = 1)
# plot margins
abline(a = (-beta_0-1) / w[1, 2], b = -w[1, 1] / w[1, 2], col = "red", lty = 3) 
abline(a = (-beta_0+1) / w[1, 2], b = -w[1, 1] / w[1, 2], col = "red", lty = 3)

# Create simulation dataset
# create positive class sample with 2 descriptive features
set.seed(1) 
f1 <- rnorm(10, mean = 6, sd = 1) 
f2 <- rnorm(10, mean = 6, sd = 1) 
old.P.data <- cbind(f1, f2) 

# Alternatively 
set.seed(1) 
P.data <- replicate(2, rnorm(10, mean = 6, sd = 1))
colnames(P.data) <- c("f1", "f2")

# create negative class sample with 2 descriptive features
N.data <- replicate(2, rnorm(30, mean = 4, sd = 1))

# combine all samples
data.mat <- data.frame(rbind(P.data, N.data), 
                       Class = rep(c("P", "N"), times = c(nrow(P.data), nrow(N.data))), 
                       stringsAsFactors = TRUE)
rownames(data.mat) <- paste("s", 1:(nrow(P.data) + nrow(N.data)), sep = "")

# plot data 
plot(P.data, col = "red", pch = 16, ylim = c(0, 9), xlim = c(0, 9))
points(N.data, col = "blue", pch = 16)

# Train a support vector classifier
# Try it with 3 different cost values
svm.model1 <- svm(x = data.mat[, -3], y = data.mat[, 3], 
                  kernel = "linear", type = "C-classification", cost = 1) 
svm.model2 <- svm(x = data.mat[, -3], y = data.mat[, 3], 
                  kernel = "linear", type = "C-classification", cost = 10) 
svm.model3 <- svm(x = data.mat[, -3], y = data.mat[, 3], 
                  kernel = "linear", type = "C-classification", cost = 0.01)

par(mfrow = c(2, 2))
# mapping decision boundary for model1 
# set up the plot without plotting points
plot(P.data, ylim = c(0, 9), xlim = c(0, 9), type = "n", main = "c = 1")

# Use the SVM model we've trained, predict a grid of points
for (x in seq(0, 10, by = 0.2)) {
  for (y in seq(0, 10, by = 0.2)) {
    t <- cbind(x, y)
    colnames(t) <- c("f1", "f2")
    if (predict(svm.model1, t) == 'N') {
      points(x, y, col = "blue", cex = 0.3, pch = 16) 
    } else { 
      points(x, y, col = "red", cex = 0.3, pch = 16)
    }
  }
}

points(P.data, col = "red", pch = 16) 
points(N.data, col = "blue", pch = 16) 

# mapping decision boundary fro model2
plot(P.data, ylim = c(0, 9), xlim = c(0, 9), type = "n", main = "c = 10") 
dat <- expand.grid(f1 = seq(0, 10, by = 0.2), f2 = seq(0, 10, by = 0.2))
predictions <- predict(svm.model2, dat) 
points(dat, col = ifelse(predictions == "P", "red", "blue"), cex = 0.3, pch = 16) 
points(P.data, col = "red", pch = 16) 
points(N.data, col = "blue", pch = 16) 

# mapping decision boundary for model3
plot(P.data, ylim = c(0, 9), xlim = c(0, 9), type = "n", main = "c = 0.01")
predictions <- predict(svm.model3, dat) 
points(dat, col = ifelse(predictions == "P", "red", "blue"), cex = 0.3, pch = 16) 
points(P.data, col = "red", pch = 16) 
points(N.data, col = "blue", pch = 16) 

# Create linearly non-separable data 
# create positive class sample with 2 descriptive features
set.seed(3) 
f1 <- rnorm(50, mean = 6, sd = 0.6) 
set.seed(4) 
f2 <- rnorm(50, mean = 6, sd = 0.6) 
P1.data <- cbind(f1, f2) 

set.seed(5) 
f1 <- rnorm(50, mean = 3, sd = 0.6) 
set.seed(6) 
f2 <- rnorm(50, mean = 3, sd = 0.6) 
P2.data <- cbind(f1, f2) 
P.data <- rbind(P1.data, P2.data) 

svm.model <- svm(x = data.mat[, -3], 
                 y = data.mat[, -3], 
                 kernel = "polynomial", 
                 degree = 6, 
                 type = "C-classification")

# plot data 
plot(P.data, col = "red", pch = 16, ylim = c(0, 9), xlim = c(0, 9))
points(N.data, col = "blue", pch = 16) 


# mapping decision boundary 
predictions <- predict(svm.model, dat) 
points(dat, col = ifelse(predictions == "P", "red", "blue"), cex = 0.3, pch = 16)

# LIC went through polynomial and radial basis too quickly, need to look at tutorial 








