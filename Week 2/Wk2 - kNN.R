# Loading packages
library(tidyverse) 
library(mlbench) 
library(class) 

data("BreastCancer") 

# Or alternatively use `tidyr::drop_na`
BreastCancer <- BreastCancer %>% drop_na 
dim(BreastCancer) 

# Split the data into training data and prediction 
train.inds <- sample.int(nrow(BreastCancer), size = 600) 
knn.data <- BreastCancer[!names(BreastCancer) == "Id"]
train.data <- knn.data[train.inds, ]
predict.data <- knn.data[-train.inds, ]

# We don't include class variable because we need to use
# that for classifying the tumours in the dataset
training.features <- train.data[names(train.data) != "Class"]
training.labels <- train.data[["Class"]]
predict.features <- predict.data[names(predict.data) != "Class"]
knn.model <- knn(train = training.features, 
                 test = predict.features, 
                 cl = training.labels, 
                 k = 5) 

mean(knn.model == predict.data$Class) 

