library(tidyverse) 
library(mlbench) 
library(MASS) 

# loading our dataset 
data("BreastCancer", package = "mlbench")
BreastCancer.complete <- BreastCancer %>% drop_na

# LDA Classification 
BreastCancer.complete$Cell.size <- as.numeric(BreastCancer.complete$Cell.size) 
lda.model <- MASS::lda(Class ~ Cell.size, data = BreastCancer.complete) 
lda.fitted <- predict(lda.model, BreastCancer.complete)$posterior[, "malignant"]

# plot fitted values from LDA model 
plot(BreastCancer.complete$Cell.size, lda.fitted) 

