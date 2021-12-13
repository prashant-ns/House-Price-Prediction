House_Price<-read.csv("~/Data Mining And Application//house-prices-advanced-regression-techniques//House Price.csv")
View(House_Price)

# Analysing the structure of the data set

str(House_Price)

# Changing datatype of columns in data set

House_Price$MSSubClass<-as.factor(House_Price$MSSubClass)
House_Price$OverallCond<-as.factor(House_Price$OverallCond)
House_Price$OverallQual<-as.factor(House_Price$OverallQual)
House_Price$YearBuilt<-as.factor(House_Price$YearBuilt)
House_Price$YearRemodAdd<-as.factor(House_Price$YearRemodAdd)
House_Price$GarageYrBlt<-as.factor(House_Price$GarageYrBlt)
install.packages("zoo")
library(zoo)
House_Price$YrSold<- as.yearmon(paste(House_Price$YrSold, House_Price$MoSold), "%Y %m")

House_Price$YrSold<-as.Date(House_Price$YrSold,format='%Y/%M')
# Removing columns for basement square fit as we have total basement squarefit

House_Price<-House_Price[,-77]
House_Price$BsmtFinSF1<-NULL
House_Price$BsmtFinSF2<-NULL
House_Price$BsmtUnfSF<-NULL

# Adding area of basement,first floor and second floor to calculate total area of house

House_Price$TotalSqfit<-House_Price$TotalBsmtSF+House_Price$X1stFlrSF+House_Price$X2ndFlrSF

str(House_Price)
# Removing columns like totalbsmtSf, X1!stFlr and X2ndFlrSF as we have total area of house.

House_Price$Id<-NULL
House_Price$X1stFlrSF<-NULL
House_Price$X2ndFlrSF<-NULL
House_Price$TotalBsmtSF<-NULL
str(House_Price)
library(dplyr)

# Checking Total NA in the data set.
colSums(is.na(House_Price))

# From the output we can see four columns has more than 80% missing values. So we can drop those columns.
House_Price$Alley<-NULL
House_Price$PoolQC<-NULL
House_Price$Fence<-NULL
House_Price$MiscFeature<-NULL


# Seperating data set into factor and integer columns

House_Price_int<-House_Price %>% select_if(is.integer)
str(House_Price_int)
House_Price_fact<-House_Price[,!sapply(House_Price,is.integer)]
str(House_Price_fact)
View(House_Price_int)
# Visualizing skewness of each column and generating Skewness table
library(tidyr)
library(ggplot2)
ggplot(gather(House_Price_int), aes(value)) +
  geom_density() +
  facet_wrap(~key, scales = 'free_x')


# Applying Log+1 transformation on columns which have skewness greater than 0.5 
library(moments)
for (i in c(1:24)){
  
  s <- skewness(House_Price_int[,i], na.rm = TRUE)
  if (s>0.5){
    House_Price_int[,i]<-log1p(House_Price_int[,i])
  }
}

View(House_Price_int$SalePrice)

# Applying Normalization on each column having integer

House_Price_int_Norm <- as.data.frame(scale(House_Price_int[1:24]))
View(House_Price_int_Norm)

ggplot(gather(House_Price_int_Norm), aes(value)) +
  geom_density() +
  facet_wrap(~key, scales = 'free_x')

#Checking NA 
colSums(is.na(House_Price_int))

# We can see that only two columns (Lot Frontage and MasVnrArea) has NA
# So replacing NA using Knnimputation method

install.packages("DMwR")
library("DMwR")

House_Price_int_Norm<-knnImputation(House_Price_int_Norm, k = 10, scale = T, meth = "weighAvg", distData = NULL)
colSums(is.na(House_Price_int_Norm))
View(House_Price_int_Norm)

# Combining the integer and factor data 

New_House_Price<-cbind(House_Price_fact,House_Price_int_Norm)
View(New_House_Price)
str(New_House_Price)

# Checking NA in the dataset

h<-New_House_Price[,c(16,39)]
h$new=ifelse(as.character(h$YearBuilt) == as.character(h$GarageYrBlt), 1, 0)
table(h$new)

# From the above output we can see that maximum garage were built in the same year as the house was built.
# So we can replace NA of GarageYrBlt column from those entries in  Yearbuilt column.


New_House_Price$GarageYrBlt<-ifelse(is.na(New_House_Price$GarageYrBlt),paste(New_House_Price$YearBuilt),paste(New_House_Price$GarageYrBlt))

table(New_House_Price$GarageType)
table(New_House_Price$GarageFinish)

# Replacing NA with mode of those column having missing values more than 5%.
table(New_House_Price$GarageType)
table(New_House_Price$GarageFinish)
table(New_House_Price$GarageQual)
table(New_House_Price$GarageCond)
New_House_Price$GarageType[is.na(New_House_Price$GarageType)]<-"Attchd"
New_House_Price$GarageFinish[is.na(New_House_Price$GarageFinish)]<-"Unf"
New_House_Price$GarageQual[is.na(New_House_Price$GarageQual)]<-"TA"
New_House_Price$GarageCond[is.na(New_House_Price$GarageCond)]<-"TA"

colSums(is.na(New_House_Price))
# Predicting NA using Random Forest
install.packages("randomForest")
library(randomForest)
New_data<-New_House_Price%>% select(-YearBuilt,-YearRemodAdd,-GarageYrBlt,-YrSold)
Data_na<-subset(New_House_Price,is.na(New_House_Price$FireplaceQu))
colSums(is.na(Data_na))
data_cl<-subset(New_House_Price,!(is.na(New_House_Price$FireplaceQu)))
str(Data_na)
a<-data_cl%>% select(-YearBuilt,-YearRemodAdd,-GarageYrBlt,-YrSold)
b<-Data_na%>% select(-YearBuilt,-YearRemodAdd,-GarageYrBlt,-YrSold)
house_model <- randomForest(FireplaceQu~.,data = a, na.action = na.omit)
p<-predict(house_model,b)
View(p)
# adding column to data

house_price1_pred<-cbind(Data_na,p)

colSums(is.na(house_price1_pred))

house_price1_pred<-house_price1_pred[,-37]


colnames(house_price1_pred)[70]<-"FireplaceQu"

Cleaned_data<-rbind(house_price1_pred,data_cl)
dim(Cleaned_data)
str(Cleaned_data)
colSums(is.na(Cleaned_data))

# Omitting all the remaining missing values as they are less than 5%

Cleaned_data<-na.omit(Cleaned_data)
str(Cleaned_data)
colSums(is.na(Cleaned_data))
View(Cleaned_data)
dim(Cleaned_data)
# Converting data type of columns

Cleaned_data$OverallQual<-as.numeric(Cleaned_data$OverallQual)
Cleaned_data$OverallCond<-as.numeric(Cleaned_data$OverallCond)
Cleaned_data$YearBuilt<-as.numeric(Cleaned_data$YearBuilt)
Cleaned_data$YearRemodAdd<-as.numeric(Cleaned_data$YearRemodAdd)
Cleaned_data$GarageYrBlt<-as.numeric(Cleaned_data$GarageYrBlt)

# Seperating dataset into numeric and categorical columns

Cleaned_data_num<-Cleaned_data[,sapply(Cleaned_data,is.numeric)]
str(Cleaned_data_num)
Cleaned_data_fact<-Cleaned_data[,!sapply(Cleaned_data,is.numeric)]
str(Cleaned_data_fact)

# Building correlation of Sales price among numeric columns

correlation<-cor(Cleaned_data_num)
library(corrplot)
corrplot(correlation,method = "number",number.cex = 0.5)

# Removing columns which have correlation with SalePrice less than 10 %
Cleaned_data_num$OverallCond<-NULL
Cleaned_data_num$LowQualFinSF<-NULL
Cleaned_data_num$BsmtHalfBath<-NULL
Cleaned_data_num$KitchenAbvGr<-NULL
Cleaned_data_num$X3SsnPorch<-NULL
Cleaned_data_num$ScreenPorch<-NULL
Cleaned_data_num$PoolArea<-NULL
Cleaned_data_num$MiscVal<-NULL
str(Cleaned_data_num)
correlation1<-cor(Cleaned_data_num)
corrplot(correlation1,method = "number",number.cex = 0.5)

# Checking correlation of categorical variable with sales price 
# Using Anova analysis

Cleaned_data_fact_Sale<-Cleaned_data_fact
Cleaned_data_fact_Sale$SalePrice<-Cleaned_data$SalePrice

anova_test<-aov(SalePrice~.,data = Cleaned_data_fact_Sale)
summary(ano)

# Removing insignificant column
Cleaned_data_fact$Street<-NULL
Cleaned_data_fact$Utilities<-NULL
Cleaned_data_fact$LandSlope<-NULL
Cleaned_data_fact$BsmtFinType2<-NULL
Cleaned_data_fact$Electrical<-NULL
Cleaned_data_fact$GarageType<-NULL
Cleaned_data_fact$PavedDrive<-NULL
Cleaned_data_fact$YrSold<-NULL
Cleaned_data_fact$SaleType<-NULL
str(Cleaned_data_fact)

# Combining categorical and numerical columns 

Cleaned_data_new<-cbind(Cleaned_data_num,Cleaned_data_fact)

dim(Cleaned_data_new)

# splitting into test and train data set

set.seed(1)
s<-sample(nrow(Cleaned_data_new),nrow(Cleaned_data_new)*0.8)
House_train<-Cleaned_data_new[s,]
House_test<-Cleaned_data_new[-s,]
nrow(House_train)
nrow(House_test)


# Applying model on the train dataset

house_model<-lm(SalePrice~.,data=House_train)
summary(house_model)

# Optimizing Model by removing insignificant column

house_model<-lm(SalePrice~.-LotFrontage-GarageYrBlt-MasVnrArea-TotRmsAbvGrd-BedroomAbvGr-HalfBath-FullBath-Foundation-GarageCars-OpenPorchSF-EnclosedPorch-LotShape-LandContour-Exterior2nd-FireplaceQu-GarageFinish-RoofStyle,data = House_train)
summary(house_model)


# Predict values for test dataset

House_test1<-subset(House_test,!(House_test$Neighborhood=="Blueste"))
House_test1<-subset(House_test1,!(House_test1$Condition2=="PosA"))

nrow(House_test1)
pred<-predict(house_model,newdata = House_test1)
View(pred)

pred1<-cbind(pred,House_test1$SalePrice)
View(pred1)

head(House_test1$SalePrice)
head(pred)

pred2<-cbind(pred1,(pred1[,1]-pred1[,2]))
View(pred2)
mean(pred2[,1])
mean(pred2[,2])

# Week 3 start #####

library(caret)
library(tidyverse)
install.packages("leaps")
library(leaps)
library(MASS)

step_model <- stepAIC(house_model, direction = "both", trace = FALSE)
summary(step_model)

new_pred<-predict(step_model,newdata = House_test1)

summary(step_model)

# Comparing RMSE of initial model and after optimizing model
RMSE(House_test1$SalePrice, pred)

RMSE(House_test1$SalePrice, pred5)

# Checking VIF in the data 
library('car')
install.packages("usdm")
library('usdm')
car::vif(step_model)

# Applying model and removing columns (MasVnrArea, GrLivArea ) with VIF greter than 5

mod<-lm(formula = SalePrice ~ OverallQual + YearBuilt + YearRemodAdd + 
          LotArea +BsmtFullBath + Fireplaces + 
          GarageCars + GarageArea + WoodDeckSF + TotalSqfit + MSSubClass + 
          MSZoning + LotConfig + Neighborhood + Condition1 + Condition2 + 
          HouseStyle + RoofMatl + Exterior1st + MasVnrType + ExterCond + 
          Foundation + BsmtQual + BsmtCond + BsmtExposure + BsmtFinType1 + 
          Heating + HeatingQC + CentralAir + KitchenQual + Functional + 
          GarageQual + GarageCond + SaleCondition, data = House_train)
summary(mod)
car::vif(mod)


# Applying K fold technique for improving predictions

set.seed(123)

train_control <- trainControl(method = "cv", number = 10)

model_log <- train(SalePrice ~ OverallQual + YearBuilt + YearRemodAdd + 
                     LotArea + MasVnrArea + GrLivArea + BsmtFullBath + Fireplaces + 
                     GarageCars + GarageArea + WoodDeckSF + TotalSqfit + MSSubClass + 
                     MSZoning + LotConfig + Neighborhood + Condition1 + Condition2 + 
                     HouseStyle + RoofMatl + Exterior1st + MasVnrType + ExterCond + 
                     Foundation + BsmtQual + BsmtCond + BsmtExposure + BsmtFinType1 + 
                     Heating + HeatingQC + CentralAir + KitchenQual + Functional + 
                     GarageQual + GarageCond + SaleCondition, data = House_train, trControl=train_control,method="lm")
model_log$results

model_log$bestTune

summary(model_log$finalModel)

model_log$resample

# Using Decision tree

m<-rpart(SalePrice~., data = House_train, method = "anova")
plot(m)
printcp(m)
m$cptable
m$cptable[which.min(m$cptable[,4])]

tm<-prune(m, cp=m$cptable[which.min(m$cptable[,4])])
print(tm)
post(tm, filename = "first_deci")
p<-predict(tm, newdata = House_test1)

RMSE(House_test1$SalePrice, p)
summary(m)







