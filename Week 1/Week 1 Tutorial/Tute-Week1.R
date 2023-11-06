### Lab Week 1 ###
## 1.1 File I/O
# Reading a dataset
library(readr) 
library(tidyverse)

cereal <- read.csv("Cereal.csv", header = TRUE)
cereal_tbl <- read_csv("Cereal.csv")
cereal_tbl

# 1.2 Data Types
# Data frames
head(cereal)

cereal_tbl |> head(7) # using pipe 
# note: |> is the same as %>% it's just a new update 

cereal_tbl |> class() # tells us this is a tibble 
# remember tibble from ECON2209? 

# colnames of cereal table
cereal_tbl |> colnames() 

cereal_tbl |> dim()

cereal_tbl |> nrow()

dim(cereal) 

nrow(cereal) 

# Some 'tidy' ways
Cal <- cereal_tbl %>%
  select(calories) # tibble with one column 

Cal <- cereal_tbl %>%
  pull(calories) # pull out the column as vector

# Base R 
AlternativeCal <- cereal[["calories"]]
acal <- cereal$calories
identical(AlternativeCal, acal) # checks if they are the same 

class(cereal["calories"])

class(cereal[["calories"]])

cereal[1:10,]

# Tidyverse
cereal_tbl %>%
  slice(1:10) # prefer this way 

# New dataframe with only rows that belong to Kelloggs only
Kelloggs <- subset(cereal, mfr == "K")
head(Kelloggs)

Kelloggs.2 <- cereal[cereal$mfr == "K", -2] # removes 2nd column
head(Kelloggs.2)

# Base R splitting
cereal.splitted <- split(cereal, cereal$mfr) # list of 7 items based on mfr 
Kelloggs.3 <- cereal.splitted["K"]
Kelloggs.4 <- cereal.splitted[["K"]]
identical(Kelloggs, Kelloggs.4)

# Tidyverse way 
kelloggs_tbl <- cereal_tbl %>%
  filter(mfr == "K")
kelloggs_tbl <- cereal_tbl %>%
  filter(mfr == "K") %>%
  select(-mfr) # drop mfr

# Load cereal data again, then use stringsAsFactors = True
cereal <- read_csv("Cereal.csv")
cereal.with.factors <- read.csv("Cereal.csv", stringsAsFactors = TRUE)
cereal.with.factors

levels(cereal.with.factors$mfr)

class(cereal.with.factors$mfr)

class(cereal$mfr)

# or 
class(cereal.with.factors$carbo) 

class(cereal$carbo) 

# or 
str(cereal.with.factors) # only characters become factors

str(cereal) 

# Levels in mfr and type (using functions levels or nlevels) 

levels(cereal.with.factors$mfr) 

# or 
nlevels(cereal.with.factors$mfr)

# or 
str(cereal.with.factors$mfr)

# class() typeof() 

# extracting calories into new vector: cereal.calories
cereal.calories <- cereal$calories
cereal.calories <- cereal[["calories"]]
cereal_calories <- cereal_tbl %>%
  pull(calories)

# elements in cereal.calories (length) 
length(cereal.calories) 

cereal_calories %>%
  length() # not a fan of this method 

# extract the 5th to the 10th element from cereal.calories
cereal_calories[5:10]

# add one more element to cereal.calories using c()
cereal_calories <- c(cereal_calories, 1.0) # c for concatenate
length(cereal_calories) 

# Matrix
# Forcing cereal dataframe to be a Matrix
cereal.matrix <- as.matrix(cereal) 
str(cereal.matrix) 

# Again, but this time leaving out mfr, name and type columns
cereal.removed <- cereal[, -(1:3)]
cereal.removed

cereal.numeric.matrix <- as.matrix(cereal.removed) 
str(cereal.numeric.matrix) 

## 1.3 Numerical summary 
summary(cereal$sodium) 

# Basic statistics
cereal_tbl %>%
  pull(sodium) %>%
  summary()

sodium <- cereal$sodium 
max(sodium) 

min(cereal$sodium) 

sd(cereal$sodium) 

mean(cereal$sodium) 

summary(sodium) 

# mean sodium of each mfr
# can be done by repeated subsetting, this is tedious
kelloggs.cereals <- subset(cereal, mfr == "K")
mean(kelloggs.cereals$sodium) 

# Can use a formula and the aggregate function 

mean.sodiums <- aggregate(sodium ~ mfr, data = cereal, FUN = mean) 
# Can split vector (or data.frame if you wanted) by 
# another vector. In this case, split by species. 

split.sodium <- split(cereal$sodium, cereal$mfr) 
# Apply a function over a list and return a list (_l_apply for list apply) 
lapply(split.sodium, mean) 

# Apply a function over a list and return a _s_implified format (_s_apply for simplify apply) 
sapply(split.sodium, mean) 

# Also could use by and tapply, vapply
cereal_tbl %>%
  select(sodium, mfr) %>%
  group_by(mfr) %>%
  summarise(mean_sodium = mean(sodium))

## 1.4 Graphical summary 
# Boxplot of sodium against mfr using boxplot() 
boxplot(sodium ~ mfr, data = cereal, 
        xlab = "Manufacturer", ylab = "Sodium content", main = "Something")

ggplot(cereal_tbl, aes(x = mfr, y = sodium)) + geom_boxplot() + theme_classic()

cereal_tbl %>%
  ggplot(aes(x = mfr, y = sodium)) + geom_boxplot() + theme_classic()

# Scatterplot 
# Plot calories against sodium using plot()
# Using formula
plot(calories ~ sodium, data = cereal, main = "Something")

# Another way, define x and y 
x <- cereal$sodium
y <- cereal$calories
plot(x, y) 

plot(cereal$sodium, cereal$calories) 

# Alternatively, use with to help R find the vectors
# with(cereal, plot(sodium, calories))

ggplot(cereal_tbl, aes(x = sodium, y = calories)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) 

## 1.5 Write Data to File
# Data frame with only the Kellogg's observations to 
# a file called kelloggs.csv 
write.csv(kelloggs_tbl, file = "kelloggs.csv")
head(cereal) 

## 2. Melbourne house prices regression model 
melb.dat <- read.csv("Melbourne_housing_FULL.csv")
melbdata <- read_csv("Melbourne_housing_FULL.csv")

# 2.2 Initial data analysis
# Take subset of the data to look at 3 suburbs - Brunswick, Craigieburn and Hawthorn
# Base R
melb.data.sub <- subset(melbdata, Suburb == "Hawthorn" | Suburb == "Brunswick" | Suburb == "Craigieburn")
melb.data.sub2 <- subset(melbdata, Suburb %in% c("Hawthorn", "Brunswick", "Craigieburn"))
identical(melb.data.sub, melb.data.sub2)

split.data <- split(melb.data.sub[["Price"]], melb.data.sub[["Suburb"]])
suburb.means <- vapply(split.data, mean, numeric(1L), na.rm = TRUE) 
suburb.medians <- vapply(split.data, median, numeric(1L), na.rm = TRUE) 

# Tidyverse way 
melbdata.sub <- melbdata %>%
  filter(Suburb %in% c("Hawthorn", "Brunswick", "Craigieburn")) %>%
  mutate(Suburb = factor(Suburb, levels = c("Craigieburn", "Brunswick", "Hawthorn")))

melbdata %>%
  filter(Suburb %in% c("Hawthorn", "Brunswick", "Craigieburn")) %>%
  group_by(Suburb) %>%
  summarise(Mean_Price = mean(Price, na.rm = TRUE), Median_price = median(Price, na.rm = TRUE))

## 2.3 Finding Association 1
# Form the linear regression model 
lm1 <- lm(data = melbdata.sub, Price/1000 ~ BuildingArea) 

# Inspect coefficients
coef(lm1) 

lm1 |> coef() # get coefficients

lm1 |> fitted() |> head() # fitted values

lm1 |> resid() |> head() # residuals i.e. errors, check mean is zero. 

ggplot(melbdata.sub |> select(BuildingArea, Price) |> drop_na()) + 
  aes(x = BuildingArea, y = Price/1000) + 
  geom_point() + geom_smooth(formula = y ~ x, method = "lm", se = FALSE) + 
  theme_classic() + labs(x = "Building area", y = "Price (in $1000s)", title = "Chosen title")

# Base R way 
summary(lm1) 

plot(Price/1000 ~ BuildingArea, data = melbdata.sub, 
     main = "House prices in Brunswick, Craigieburn and Hawthorn", 
     xlab = "Building Area (in sqm)", ylab = "Price (in 1000s of $AUD)")
abline(lm1, col = "red", lty = "dotted")

par(mfrow = c(2, 2)) 
plot(lm1, which = 1:4) 

r2 <- round(summary(lm1)$r.squared, 4) 
r2

# Tidyverse way 
# doesn't seem to work..
library(ggfortify) 
autoplot(lm1, which = 1:6, nrow = 3, ncol = 2) 

# lots of nice convenient code
lm1 |> coefficients() # get coefficients
# coefficients() is different to coef() ! 
lm1 |> fitted() # fitted values
lm1 |> residuals() %>% 
  mean() # residuals i.e. errors, check mean is zero. 

library(broom) 
lm1 |> augment() # full table of fitted values, cooks distance, etc

lm1 |> glance() # key values e.g. R squared, can pull out

lm1 |> tidy() # conveniently puts summary into tibble format

r2 <- lm1 |> glance() |> pull(r.squared) 

melbdata.sub_out <- melbdata %>%
  filter(Suburb %in% c("Hawthorn", "Brunswick", "Craigieburn")) %>%
  mutate(Suburb = factor(Suburb, levels = c("Craigieburn", "Brunswick", "Hawthorn"))) %>%
  slice(-c(158,682)) # remove the worst two outliers

lm1_alt <- lm(data = melbdata.sub_out, Price/1000 ~ BuildingArea) 

# outliers have been removed
ggplot(melbdata.sub_out |> select(BuildingArea, Price) |> drop_na()) +
  aes(x = BuildingArea, y = Price/1000) + 
  geom_point() + geom_smooth(formula = y ~ x, method = "lm", se = FALSE) + 
  theme_classic() + labs(x = "Building area", y = "Price (in $1000s)", title = "Chosen title")

# Base R 
lmfit1 <- lm(data = melbdata.sub, Price/1000 ~ BuildingArea) 
# Get semi-transparent red, green and blue
my.colours <- rgb(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1), alpha = 0.5) 
plot(Price/1000 ~ BuildingArea, data = melbdata.sub, 
     main = "House prices of some suburbs against Building Areas", 
     xlab = "Building Area (in sqm)", ylab = "Price (in 1000s of $AUD)", 
     col = my.colours[as.integer(melbdata.sub[["Suburb"]])], 
     pch = 19)
legend("topright", legend = levels(melbdata.sub[["Suburb"]]), 
       col = my.colours, pch = 19)
abline(lmfit1, col = "red", lty = "dotted")

ggplot(melbdata.sub_out |> select(BuildingArea, Price, Suburb) |> drop_na()) +
  aes(x = BuildingArea, y = Price/1000, color = Suburb) +
  geom_point() +
  theme_classic() +
  labs(x = "Area", y = "Price", title = "Chosen title") +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE)

## 2.4 Finding association II
# Base R 
boxplot(Price/10000 ~ Suburb, data = melbdata.sub, ylab = "Price (in 1000$)", xlab = "Suburb")

lm2 <- lm(Price/1000 ~ BuildingArea + Suburb, data = melb.data.sub) 
coefs <- lm2 |> coef() 

# Base R 
par(mfrow = c(2, 2))
invisible(lapply(levels(melbdata.sub$Suburb), function(x) {
  plot(Price/1000 ~ BuildingArea, data = subset(melbdata.sub, Suburb == x), main = x, 
       xlab = "Building Area (in sqm)", ylab = "Price (in 1000s of $AUD)")
  int <- coefs[1]
  if (any(adjust.ind <- grepl(paste0(x, "$"), names(coefs))))
    int <- int + coefs[adjust.ind]
  abline(int, coefs[2])
}))

# Tidyverse way 
ggplot(melbdata.sub |> select(Suburb, Price) |> drop_na()) + 
  aes(x = Suburb, y = Price/1000) + geom_boxplot()

lmfit2 <- lm(data = melbdata.sub %>% slice(-c(158, 682)), Price/1000 ~ BuildingArea + Suburb) 
coefs <- lmfit2$coefficients

autoplot(lmfit2, which = 1:6, nrow = 3, ncol = 2) 

ggplot(melbdata.sub_out |> select(BuildingArea, Price, Suburb) |> drop_na()) + 
  aes(x = BuildingArea, y = Price) + 
  geom_point() + 
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE) + 
  facet_wrap(~Suburb)

summary(lmfit2) 

lmfit2 %>% glance() 

r2s <- lmfit2 %>% glance() %>% pull(r.squared) 
r2s <- summary(lmfit2)$r.squared

# One way to highlight that the regression lines for the 
# three suburbs are parallel is to put all three on the same
# graph, as follows

# Base R 
plot(Price/10000 ~ BuildingArea, data = melbdata.sub, 
     main = "House prices of some suburbs against Building Area", 
     xlab = "Building Area (in sqm)", ylab = "Price (in 1000s of $AUD)", 
     col = my.colours[as.integer(melbdata.sub[["Suburb"]])], 
     pch = 19)
legend("topright", legend = levels(melbdata.sub[["Suburb"]]), 
       col = my.colours, pch = 19)
coefs <- coefficients(lmfit2) 
names(my.colours) <- levels(melbdata.sub$Suburb) 
r2 <- round(summary(lmfit1)$r.squared, 4) 

obs.buildingarea.suburb <- subset(melbdata.sub, select = c("BuildingArea", "Suburb"))
obs.buildingarea.suburb <- na.omit(obs.buildingarea.suburb) 
buildingarea.by.suburb <- with(obs.buildingarea.suburb, split(BuildingArea, Suburb))
buildingarea.by.suburb <- lapply(buildingarea.by.suburb, range) 

invisible(lapply(levels(melbdata.sub$Suburb), function(x) {
  pred.df <- data.frame(BuildingArea = buildingarea.by.suburb[[x]], 
                        Suburb = x)
  lines(pred.df[["BuildingArea"]], predict(lm2, newdata = pred.df), 
        col = my.colours[which(levels(obs.buildingarea.suburb[["Suburb"]]) == x)])
}))

# Embedding model fit in ggplot
library(ggplot2) 
ggplot(melbdata.sub, aes(x = BuildingArea, y = Price/1000, color = Suburb)) + 
  geom_point(na.rm = TRUE) + 
  geom_smooth(formula = "y~x", method = "lm", se = FALSE, fullrange = TRUE, na.rm = TRUE) + 
  xlab("Building Area") + ylab("Price ($1,000)") + labs(colour = "Suburb:")

# Specifying the colour slightly differently, results in a different model! 

ggplot(melbdata.sub, aes(x = BuildingArea, y = Price/1000)) + 
  geom_point(aes(color = Suburb), na.rm = TRUE) + 
  geom_smooth(formula = "y~x", method = "lm", se = FALSE, fullrange = TRUE, na.rm = TRUE) + 
  xlab("Building Area") + ylab("Price ($1,000)") + labs(colour = "Suburb:")

# A simple way to avoid the problem is to make sure that the 
# model being plotted is the original model used for the numerical analysis: 

# reuse previous color key
colScale <- scale_colour_manual(name = "Suburb:", values = my.colours) # name is used as legend title
ggplot(melbdata.sub, aes(x = BuildingArea, y = Price/1000, col = Suburb) ) +
  geom_point(na.rm = TRUE) +
  xlab("Building Area") + ylab("Price ($1,000)") + colScale +
  sapply(unique(melbdata.sub$Suburb), function(x) {
    int <- coefs[1]
    if (any(adjust.ind <- grepl(paste0(x, "$"), names(coefs))))
      int <- int + coefs[adjust.ind]
    geom_abline( intercept=int, slope=coefs[2], col=my.colours[x] )
  })

"""
The `suburb` predictor improves the fit of the model by increasing the R^2
form 0.1725 to 0.5767645. However, the adjusted R^2 is a more appropriate 
goodness of fit measure when there is more than one predictor in the model since
adding another predictor will always increase the R^2. In this case the adjusted
R^2 increases by a similar amount suggesting Suburb is a good additional predictor. 

Interpreting the `BuildingArea` slope has the interpretation that for each unit 
increase in square metre of building size, the expected average price would increase
by $4435. Interpreting the categorical predictors needs to be done by intercept 
adjustment. The first categorical level of `Suburb` (Brunswick) becomes the baseline
intercept and the other suburbs are adjusted against the baseline interecept. 

In this case Craigieburn and Hawthorn have adjustments of -660,000 and 400,000 
respectively. This should be interpretted that properties in Craigieburn are 
$660,000 cheaper than Brunswick on average (if BuildingArea is held fixed). 
Hawthorn properties are $400,000 more expensive than Brunswick. This is consistent
with the graphical summary in the boxplot which indicates without adjusting for 
Building Area, Craigieburn tends to have cheaper houses with low variance while
Hawthorn has a large variance in house prices with many very expensive outlying 
properties. 
"""

# Adding the number of car spaces into the model and 
# compare the goodness of fit measures
lmfit4 <- lm(data = melbdata.sub, Price ~ BuildingArea + Suburb + Car) 
summary(lmfit4)

# Adding car spaces seems to improve the prediction model if `BuildingArea`
# `Suburb` are already included in the model. The goodness of fit metrics 
# (both raw and adjusted) increase. 

## 2.5 Impact of outliers
# A simple strategy to assess the impact of outliers is to remove the 
# outliers and see if you can improve the prediction model

melbdata.sub.2 <- subset(melbdata.sub, BuildingArea > 10 & BuildingArea < 300) 
lmfit3 <- lm(data = melbdata.sub.2, Price/1000 ~ BuildingArea + Suburb) 
coefs <- lmfit3$coefficients
par(mfrow = c(2, 2))
invisible(lapply(unique(melbdata.sub$Suburb), function(x) {
  plot(Price/1000 ~ BuildingArea, data = subset(melbdata.sub, Suburb == x), main = x, 
       xlab = "Building Area (in sqm)", ylab = "Price (in 1000s of $AUD)")
  int <- coefs[1]
  if (any(adjust.ind <- grepl(paste0(x, "$"), names(coefs))))
    int <- int + coefs[adjust.ind]
  abline(int, coefs[2])  
}))
summary(lmfit3) 

# Removing the outliers does improve the fit of the model as the overall 
# residual standard error has decreased and the goodness of fit metrics have 
# increased to around 0.6. Also visually we can see an improved fit for 
# Hawthorn and Brunswick. Although there still is a poor fit for Craigieburn

## 2.6 Prediction 
predict(lmfit4, data.frame(Suburb = "Hawthorn", BuildingArea = 100, Car = 2), interval = "confidence")

# If using the `lmfit4` model, then the predicted value for an average 
# property with those features would be $1,514,653




