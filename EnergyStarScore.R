getwd()

#Importing the required libraries
library(Hmisc)
library(caret)
library(corrplot)
library(doParallel)
library(Metrics)

memory.limit()
memory.limit(size=16000)

#Importing the dataset
data <- read.csv(file = 'Usecase1_Dataset.csv')
str (data)
ncol(data)

#There are 60 variables including a mix of discrete and continuous variables
# We can remove the unwanted columns like additional location details (Street number, street address) which is taken care by Postal Code variable
#Assumption 1 - Property id and other property details do not affect the energy score as these are only identification details for record purposes
#Assumption 2 - Duplicate entries like 'Primary Property Type' can be omitted since the data is covered well in 'Largest property use type'

data_clean <- data[,11:60]
data_clean$Street.Number <- NULL
data_clean$Street.Name <- NULL
data_clean$List.of.All.Property.Use.Types.at.Property <- NULL
data_clean$Primary.Property.Type...Self.Selected <- NULL

# Postal code and borough seem to be correlated
# There are more missing values in Borough than in postal code
# hence dropping Borough column

data_clean$Borough<- NULL
data_clean$NTA <- NULL
data_clean$DOF.Gross.Floor.Area <- NULL
data_clean$Release.Date <- NULL

# Removing latitude & longitude columns as we have postal code to group according to area
data_clean <- data_clean[,1:(ncol(data_clean)-5)]

#Tackling rows with 'Not available' data for energy score (target variable)
#Assumption 3 - Removing data points with NaN values of target variable will not affect the accuracy of model as the removed data points are much less

sum(data_clean$ENERGY.STAR.Score == "Not Available")
data_clean <- data_clean[-which(data_clean$ENERGY.STAR.Score == "Not Available"),]

# Dropping variables not normalized for weather and as we are taking (/ftA2) values instead of total value
data_clean$`Site.EUI..kBtu.ft².` <- NULL
data_clean$`Source.EUI..kBtu.ft².` <- NULL
data_clean$Total.GHG.Emissions..Metric.Tons.CO2e. <- NULL
data_clean$Weather.Normalized.Site.Electricity..kWh. <- NULL
data_clean$Water.Required. <- NULL
data_clean$Water.Use..All.Water.Sources...kgal. <- NULL

#Remove data columns with more than 40% of 'Not Available' entries
cnt_NA <- rep(0,31)

for (i in 1:31){
  if (length(grep("Not Available", data_clean[,i])) > 0){
    cnt_NA[i] <- length(which(data_clean[,i] == "Not Available")) 
  }
}
rm(i)

percent_NA <- (cnt_NA/nrow(data_clean))*100
df_NAcolumns <- cbind(colnames(data_clean), percent_NA)
rm(cnt_NA)

# Remove the columns with more than 40% of rows containing NA values

data_clean <- data_clean[,which(percent_NA <= 40)]
rm(percent_NA)

# Arrange columns per factored, continuous and target variable/s
data_clean <- subset(data_clean,select = c(1,2,7,8,21,3:6,10:20,9))

#Converting Postal code data to valid data, truncating first five digits (as is the standard format)
data_clean$Postal.Code <- substr(data_clean$Postal.Code, 1,5)

#Convert the variables to factors and numericals

for (i in 1:5){
  data_clean[,i] <- as.factor(data_clean[,i])
}

for (i in 6:21){
  data_clean[,i] <- as.numeric(data_clean[,i])
}
rm(i)

summary(data_clean)

# Understanding class distribution of levels for discrete variables
# As can be seen in the proportion table, 'Metered areas Energy' column has skewed ratio of the factors
# and very less data points for 'another config' and 'not available', hence removing these rows (20 count) and the column
# Similarly with 'Metered areas water', removing the rows with factors of less data points and removing the column

# Bar Plot for Discrete Variables

par(mfrow=c(2,2))
barplot(prop.table(table(data_clean$Metered.Areas..Energy.)), ylab = "Frequency", 
        main = "Frequency chart for 'Metered Areas Energy'", col = "blue")
barplot(prop.table(table(data_clean$Metered.Areas...Water.)), ylab = "Frequency", 
        main = "Frequency chart for 'Metered Areas Water'", col = "Cyan")
barplot(prop.table(table(data_clean$Largest.Property.Use.Type)), ylab = "Frequency", 
        main = "Frequency chart for 'Largest.Property.Use.Type'", col = "Red")
barplot(prop.table(table(data_clean$Postal.Code)), ylab = "Frequency", 
        main = "Frequency chart for 'Postal.Code'", col = "Black")
par(mfrow=c(1,1))

#Cleaning Metered Areas Energy column
data_clean <- data_clean[which(data_clean$Metered.Areas..Energy. == "Whole Building"),]
data_clean$Metered.Areas..Energy. <- NULL

#Cleaning 'Metered Area Water' column
data_clean <- data_clean[which(data_clean$Metered.Areas...Water. == "Not Available" | data_clean$Metered.Areas...Water. == "Whole Building"), ]
data_clean$Metered.Areas...Water. <- NULL

#Cleaning 'Largest Property Use Type' column
table(data_clean$Largest.Property.Use.Type )

data_clean <- data_clean[which(data_clean$Largest.Property.Use.Type == "Multifamily Housing" |
                                 data_clean$Largest.Property.Use.Type == "Office" |
                                 data_clean$Largest.Property.Use.Type == "Hotel" |
                                 data_clean$Largest.Property.Use.Type == "Non-Refrigerated Warehouse"),]
data_clean$Largest.Property.Use.Type <- factor(data_clean$Largest.Property.Use.Type)

summary(data_clean)
str(data_clean)

# Removing outliers from continuous variables and replacing missing values with average values
#Plot boxplot for each continuos variable
par(mfrow=c(4,4))
for (i in 6:21){
  boxplot(data_clean[,i], xlab = colnames(data_clean)[i])
}

# Find absolute value of z-score for each value in each column

df <- data_clean[,4:19]
z_scores <- as.data.frame(sapply(df, function(df) (abs(df-mean(df, na.rm = TRUE))/sd(df, na.rm = TRUE))))
data_clean <- data_clean[!rowSums(z_scores>3, na.rm = TRUE), ]

for (i in 1:19){
  if(length(which(is.na(data_clean[,i]))=="TRUE")>0){
  data_clean[which(is.na(data_clean[,i])),i] <- median(data_clean[,i], na.rm = TRUE)
  }
}
rm(i)
rm(df)

#Correlation Matrix for feature selection
par(mfrow=c(1,1))
cor_matrix <- data_clean
data_clean.cor <- cor(cor_matrix[4:19])
colnames(data_clean.cor) <- substring(colnames(data_clean[,4:19]),1,15)
rownames(data_clean.cor) <- substring(colnames(data_clean[,4:19]),1,15)
corrplot(data_clean.cor)

highlyCorrelated <- findCorrelation(data_clean.cor, cutoff=0.75, names = TRUE)

# Removing highly correlated columns
data_clean <- data_clean[,-which(colnames(data_clean) %in% highlyCorrelated)]

#checking for columns with count of missing values
summary(data_clean)

# renaming columns
length(colnames(data_clean))
names_col <- c("PostalCode","LargeUse", "SubmissionStatus",
                          "BuiltYear","CountofBuildings", "Occupancy", 
                          "WeatherNormSiteEUI", "WeatherNormElecEUI", "WeatherNormNGEUI",
                          "ElecUse","DirectGHG","WaterIntensity","EnergyScore")
length(names_col)
colnames(data_clean) <- names_col
rm(names_col)

# remove duplicate entries
data_clean <- unique(data_clean)

str(data_clean)

#Visualizing the distribution of Target variable for any skewed distribution
barplot(data_clean$EnergyScore)

# split data in train and test in 80-20
 
dt <- sort(sample(nrow(data_clean), nrow(data_clean)*.8))

train_data <- data_clean[dt,]
test_data <- data_clean[-dt,]
rm(dt)

#scaling variables for uniform range. As the range for EnergyScore is not much, we will scale only the X variables and not target variable
train_data[,4:12] <- scale(train_data[,4:12])
test_data[,4:12] <- scale(test_data[,4:12])

#Removing the Submission Status column in train data. This column can later be used to predict only 'Not Submitted' Entries in test data
train_data_submission_col <- train_data$SubmissionStatus
train_data$SubmissionStatus <- NULL

test_data_submission_col <- test_data$SubmissionStatus
test_data$SubmissionStatus <- NULL

str(train_data)
str(test_data)

#Modeling with all variable and understanding each variable's significance

gc()

#10 fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "RMSE"

#Simple rpart - decision tree
set.seed(5)
fit.rpart <- train(EnergyScore~., data=train_data, method="rpart",metric = "RMSE", 
                   trControl=control, tuneLength = 15)
fit.rpart

#xgboost
set.seed(7)
fit.boost <- train(EnergyScore~., data=train_data, method="xgbTree")
fit.boost

#knn
set.seed(5)
fit.knn <- train(EnergyScore~., data=train_data, method="knn")

#svm
set.seed(7)
fit.svm <- train(Species~., data=dataset, method="svmRadial")

#random forest

set.seed(5)
fit.rf <- train(EnergyScore~., data=train_data, method="rf")

gc()

#Based on the comparative results of all algorithms (MAE and r-squared), we can go ahead with xGBoost 
#Further the accuracy of the selected model can be improved with Cross validation and tuning the hyper parameters for XGBoost
#It is not completed in this exercise due to resource limitations on the personal computer

#The model can be further improved by removing the non-important variables as follows. This assignment does not cover it due to time constraints.
Importance <- varImp(fit.boost, scale=FALSE)
plot(Importance, top = 10)

#summarize best model
print(fit.boost)

#Test the data with selected model
predictions <- predict(fit.boost, test_data[,1:11])
MAE(test_data$EnergyScore,predictions)

#MAE for train model - 19
#MAE for test model - ranges from 10.7 to 12.6
#As test model MAE is about 2 times the MAE of train model, we can understand the model is overfit 
#We can perform k-fold cv to tune the parameters for xGBoost and run random forest for further improvement in the trained model
#We will skip this step due to time limitation and resouce limitations
#Since range for the target variable is 1-100, we can go ahead with this model 

#-------------------------------------------------------------------------------
#Improving the model accuracy (difference between the MAE of train and test and MAE of test itself)
#Build dataset with important variable contributing more than 95% (drop postal code variable)

train_data_mod <- train_data[,2:12]
test_data_mod <- test_data[,2:12]

train_data_mod$SubmissionStatus <- NULL
test_data_mod$SubmissionStatus <- NULL

set.seed(7)
fit.rpart.mod <- train(EnergyScore~., data=train_data_mod, method="rpart")
fit.rpart.mod

set.seed(7)
fit.boost.mod <- train(EnergyScore~., data=train_data_mod, method="xgbTree")
fit.boost.mod

predictions.mod <- predict(fit.rpart.mod, test_data_mod[,1:10])
MAE(test_data_mod$EnergyScore,predictions)
