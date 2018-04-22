library(readr)
library(extrafont)
library(lubridate)
library(ggplot2)
library(tidyr)
library(dplyr)
library(caret)
library(purrr)
library(corrplot)

test <- read_csv("Input/test.csv", col_names = TRUE)
train <- read_csv("Input/train.csv", col_names = TRUE)

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
                      "MiscVal", "YrSold", "SalePrice")
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

train_fin <- new_train
summary(train_fin)


train_fin$LotFrontage[is.na(train_fin$LotFrontage)] <- 0
train_fin$BsmtQual <- factor(ifelse(is.na(train_fin$BsmtQual), "None", paste(train_fin$BsmtQual)), levels = c(levels(train_fin$BsmtQual), "None"))
train_fin$BsmtFinType1 <- factor(ifelse(is.na(train_fin$BsmtFinType1), "None", paste(train_fin$BsmtFinType1)), levels = c(levels(train_fin$BsmtFinType1), "None"))
train_fin$BsmtFinType2 <- factor(ifelse(is.na(train_fin$BsmtFinType2), "None", paste(train_fin$BsmtFinType2)), levels = c(levels(train_fin$BsmtFinType2), "None"))
train_fin$BsmtCond <- factor(ifelse(is.na(train_fin$BsmtCond), "None", paste(train_fin$BsmtCond)), levels = c(levels(train_fin$BsmtCond), "None"))
train_fin$Alley <- factor(ifelse(is.na(train_fin$Alley), "None", paste(train_fin$Alley)), levels = c(levels(train_fin$Alley), "None"))
train_fin$BsmtExposure <- factor(ifelse(is.na(train_fin$BsmtExposure), "None", paste(train_fin$BsmtExposure)), levels = c(levels(train_fin$BsmtExposure), "None"))
train_fin$MiscFeature <- factor(ifelse(is.na(train_fin$MiscFeature), "None", paste(train_fin$MiscFeature)), levels = c(levels(train_fin$MiscFeature), "None"))
train_fin$FireplaceQu <- factor(ifelse(is.na(train_fin$FireplaceQu), "None", paste(train_fin$FireplaceQu)), levels = c(levels(train_fin$FireplaceQu), "None"))
train_fin$PoolQC <- factor(ifelse(is.na(train_fin$PoolQC), "None", paste(train_fin$PoolQC)), levels = c(levels(train_fin$PoolQC), "None"))
train_fin$Fence <- factor(ifelse(is.na(train_fin$Fence), "None", paste(train_fin$Fence)), levels = c(levels(train_fin$Fence), "None"))
train_fin$MasVnrArea[is.na(train_fin$MasVnrArea)] <- 0
train_fin$MasVnrType <- factor(ifelse(is.na(train_fin$MasVnrType), "Unknown", paste(train_fin$MasVnrType)), levels = c(levels(train_fin$MasVnrType), "Unknown"))

summary(train_fin)
sum(is.na(train_fin))

train_fin %>%
  select(contains("arage")) %>%
  filter(GarageArea == 0)

train_fin <- train_fin %>%
  mutate(pool_avail = as.factor(ifelse(PoolArea > 0, "yes", "no")),
         alley_avail = as.factor(ifelse(Alley != "None", "yes", "no"))) %>%
  select(-c(Id, GarageType, GarageFinish,GarageQual, GarageCond, GarageYrBlt)) %>%
  filter(!is.na(Electrical))

sum(is.na(train_fin))
summary(train_fin)

write_csv(train_fin, "C:\\Users\\m.vilkaite\\House-Prices-Challenge\\clean_data.csv")


dim(train_fin)
X <- train_fin[,-which(names(train_fin) == "SalePrice")]
Y <- train_fin$SalePrice

model <- train(log(SalePrice) ~ ., data = train_fin, method = "gbm", preProcess = ("pca"))
summary(model)

X

