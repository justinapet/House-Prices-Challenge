library(readr)
library(extrafont)
library(lubridate)
library(ggplot2)
library(tidyr)
library(dplyr)
library(caret)
library(purrr)
install.packages("caret")

setwd("C:\\Users\\m.vilkaite\\Desktop\\Kaggle\\20180422_House_prices\\3. Uploaded Data")

test <- read_csv("test.csv", col_names = TRUE)
train <- read_csv("train.csv", col_names = TRUE)

glimpse(test)
glimpse(train)
summary(test)

X <- data.frame(train[, 2:80])
Y <- data.frame(train[, 81])

cat_var <- names(train)[which(map_lgl(train, is.character))]
numeric_var <- names(train)[which(map_lgl(train, is.numeric))]

should_be_numeric <- c("LotFrontage", "LotArea", "YearBuilt", "YearRemodAdd", 
                      "MasVnrArea", "BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF",
                      "TotalBsmtSF", "1stFlrSF", "2ndFlrSF", "LowQualFinSF", 
                      "GrLivArea","BsmtFullBath", "BsmtHalfBath", "FullBath", 
                      "HalfBath", "BedroomAbvGr","KitchenAbvGr", "TotRmsAbvGrd", "Fireplaces",
                      "GarageYrBlt","GarageCars", "GarageArea", "WoodDeckSF", 
                      "OpenPorchSF", "EnclosedPorch", "3SsnPorch", "ScreenPorch", "PoolArea",
                      "MiscVal", "YrSold")
numeric_var_train <- train[, should_be_numeric]
numeric_train <- map(numeric_var_train, as.numeric)
glimpse(numeric_train)

cat_var_train <- train %>%
  names() %>%
  setdiff(should_be_numeric)

head(cat_var_train)  

cat_train <- map(train[, cat_var_train], factor)
glimpse(cat_train)

new_train <- cbind(as.data.frame(cat_train),as.data.frame(numeric_train))
dim(new_train)



summary(train$SalePrice)
names(train)

model <- train(log(SalePrice) ~ YearBuilt + LotArea, data = train, method = "lm")
summary(model)

ggplot(train, aes(x = SalePrice)) +
  geom_histogram()

ggplot(train, aes(y = log(SalePrice), x = YearBuilt)) +
  geom_point()
