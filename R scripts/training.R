#Write R output to a file 
sink("ROutput1.txt")

# Enable all the required packages (Assuming that the packages were already installed):
pkgs <- c("MASS","miscTools","ggplot2","randomForest","dplyr","caret","stats","plyr","gridExtra")
lapply(pkgs, require, character.only = TRUE)

# Import data
main_data <- read.csv("train (data sci challenge).csv")

# Find data types of columns
dim(main_data)
str(main_data)

# Seperating rows that has NA value
row.has.na <- apply(main_data, 1, function(x){any(is.na(x))})
sum(row.has.na)
data.un <- main_data[row.has.na,]
dim(data.un)
main_data <- main_data[!row.has.na,]
dim(main_data)

############ Converting datatypes
main_data$rank_category = factor(main_data$rank_category)
class(main_data$rank_category)
main_data$app_category = factor(main_data$app_category)
class(main_data$app_category)
main_data$app_subcategory = factor(main_data$app_subcategory)
class(main_data$app_subcategory)

# Generate new columns
main_data$day <- format(as.Date(main_data$date,format="%m/%d/%y"), "%d")

# Add columns to dataframe
main_data$day <- as.integer(main_data$day)
class(main_data$day)


# Eliminating unnecessary columns 
main_subset <- subset(main_data, select =  -c(app_id,date))

# Normalizing 'Rank' column
summary(main_subset$rank)
main_subset$ranks <- (main_subset$rank - min(main_subset$rank)) / (max(main_subset$rank) - min(main_subset$rank))
summary(main_subset$ranks)

#********* Creating frequency distribution tables for rank_category and app_subcategory *********
#************** and finiding the pattern in data to decide the binning criteria *****************
#********************************** app_subcategory *******************************************
# Frequency distribution and density plot for app_subcategory
freqDistAppSubCat <- count(main_subset, vars = "app_subcategory")
freqDistAppSubCat$freqPercent <- round((freqDistAppSubCat$freq / sum(freqDistAppSubCat$freq)*100),digits = 2)
freqDistAppSubCat
summary(freqDistAppSubCat$freqPercent)

# Density plot
densityFreqDistAppSubCat <- ggplot(data = freqDistAppSubCat, aes(x = freqPercent))
densityFreqDistAppSubCat <- densityFreqDistAppSubCat + geom_density(fill = "skyblue",alpha = 0.6, color = "brown")
densityFreqDistAppSubCat <- densityFreqDistAppSubCat + scale_x_continuous(breaks = 0:13)
densityFreqDistAppSubCat


# Downloads by app_subcategory and density distribution of downloads by app_subcategory
downloadsByAppSubCat <- aggregate(main_subset$downloads, by = list(App_SubCategory = main_subset$app_subcategory), FUN = sum)
names(downloadsByAppSubCat)[2] <- "SumOfDownloads"
downloadsByAppSubCat$DownByPercent <- round((downloadsByAppSubCat$SumOfDownloads /sum(downloadsByAppSubCat$SumOfDownloads)*100),digits = 2)
downloadsByAppSubCat
summary(downloadsByAppSubCat$DownByPercent)

# Density plot
densityDownsAppSubCat <- ggplot(data = downloadsByAppSubCat, aes(x = DownByPercent)) 
densityDownsAppSubCat <-  densityDownsAppSubCat + geom_density(fill = "skyblue", color = "brown", alpha = 0.6)
densityDownsAppSubCat <- densityDownsAppSubCat + scale_x_continuous(breaks = 0:37)
densityDownsAppSubCat


#************************** rank_category *******************************************
# Frequency distribution and density plot for rank_category
freqDistRankCat <- count(main_subset, vars = "rank_category")
freqDistRankCat$freqPercent <- round((freqDistRankCat$freq / sum(freqDistRankCat$freq)*100),digits = 2)
freqDistRankCat
summary(freqDistRankCat$freqPercent)

# Density plot
densityFreqDistRankCat <- ggplot(data = freqDistRankCat, aes(x = freqPercent))
densityFreqDistRankCat <- densityFreqDistRankCat + geom_density(fill = "skyblue",alpha = 0.6, color = "brown")
densityFreqDistRankCat <- densityFreqDistRankCat + scale_x_continuous(breaks = 0:13)
densityFreqDistRankCat


# Downloads by rank_category and density distribution of downloads by rank_category
downloadsByRankCat = aggregate(main_subset$downloads, by = list(Rank_Category = main_subset$rank_category), FUN = sum)
names(downloadsByRankCat)[2] = "SumOfDownloads"
downloadsByRankCat$DownByPercent = round((downloadsByRankCat$SumOfDownloads /sum(downloadsByRankCat$SumOfDownloads)*100),digits = 2)
downloadsByRankCat
summary(downloadsByRankCat$DownByPercent)

# Density plot
densityDownsRankCat = ggplot(data = downloadsByRankCat, aes(x = DownByPercent)) 
densityDownsRankCat =  densityDownsRankCat + geom_density(fill = "skyblue", color = "brown", alpha = 0.6)
densityDownsRankCat = densityDownsRankCat + scale_x_continuous(breaks = 0:37)
densityDownsRankCat

# Loops through downloadsByAppSubCat and downloadsByRankCat and assign Bins to each category: 
# downloadsByAppSubCat *********************************************************
# Bin range : [0 - 1] , (1 - 4], (4 - 8], (8 - 15], , [30 - ]
for( i in 1:nrow(downloadsByAppSubCat)){
  tempValue = downloadsByAppSubCat$DownByPercent[i]
  if(tempValue >= 0 && tempValue <= 1){
    downloadsByAppSubCat$Bin_AppSubCat[i] = "Bin_AppSubCat_1"
  }else if(tempValue > 1 && tempValue <= 4){
    downloadsByAppSubCat$Bin_AppSubCat[i] = "Bin_AppSubCat_2"
  }else if(tempValue > 4 && tempValue <= 8){
    downloadsByAppSubCat$Bin_AppSubCat[i] = "Bin_AppSubCat_3"
  }else if(tempValue > 8 && tempValue <= 15){
    downloadsByAppSubCat$Bin_AppSubCat[i] = "Bin_AppSubCat_4"
  }else if(tempValue >= 30){
    downloadsByAppSubCat$Bin_AppSubCat[i] = "Bin_AppSubCat_5"
  }
}

# downloadsByRankCat *********************************************************
# Bin range : [0 - 1] , (1 - 4], (4 - 8], (8 - 16], , [20 - ]
for( i in 1:nrow(downloadsByRankCat)){
  tempValue = downloadsByRankCat$DownByPercent[i]
  if(tempValue >= 0 && tempValue <= 1){
    downloadsByRankCat$Bin_RankCat[i] = "Bin_RankCat_1"
  }else if(tempValue > 1 && tempValue <= 4){
    downloadsByRankCat$Bin_RankCat[i] = "Bin_RankCat_2"
  }else if(tempValue > 4 && tempValue <= 8){
    downloadsByRankCat$Bin_RankCat[i] = "Bin_RankCat_3"
  }else if(tempValue > 8 && tempValue <= 16){
    downloadsByRankCat$Bin_RankCat[i] = "Bin_RankCat_4"
  }else if(tempValue >= 20){
    downloadsByRankCat$Bin_RankCat[i] = "Bin_RankCat_5"
  }
}

# Loop through the main_subset and assign bin to each row, using downloadsByAppSubCat and downloadsByRankCat tables as reference
for( i in 1:nrow(main_subset)){
  # Get the category
  app_subcategory_temp = main_subset$app_subcategory[i]
  rank_category_temp = main_subset$rank_category[i]
  # Get the bins for these categories from downloadsByAppSubCat and downloadsByRankCat tables 
  main_subset$Bin_AppSubCat[i] = (filter(downloadsByAppSubCat, App_SubCategory == app_subcategory_temp))$Bin_AppSubCat
  main_subset$Bin_RankCat[i] = (filter(downloadsByRankCat, Rank_Category == rank_category_temp))$Bin_RankCat
}

# Convert the character type columns to Factor type
main_subset$Bin_AppSubCat = as.factor(main_subset$Bin_AppSubCat)
main_subset$Bin_RankCat = as.factor(main_subset$Bin_RankCat)

# Remove the origrinal app_subcategory and rank_category columns
main_subset = subset(main_subset, select = -c(rank,rank_category,app_subcategory))
str(main_subset)

# Converting logical columns to binary
cols <- sapply(main_subset, is.logical)
main_subset[,cols] <- lapply(main_subset[,cols], as.integer)
head(main_subset)

# Creating training and testing from main_subset (with out dummy variables)
set.seed(1990)
trainPercentage <- 0.70 # Percentage of rows in training data set
noTrainingRows <- floor(trainPercentage * nrow(main_subset))
trainingIndex <- sample(seq_len(nrow(main_subset)), size = noTrainingRows)
trainingData <- main_subset[trainingIndex,]
testingData <- main_subset[-trainingIndex,]


# Function to calculate mean absolute error - takes two arguments : actual value, predicted value
funcMAE <- function(actual,predicted){
  mean(abs(actual-predicted))
}


# Function to create a scatter plot between predicted and actual values, with a smoothing curve
createPredVsAcSP <- function(actualValues, predictedValues){
  p = ggplot(aes(x = actual, y = predicted), data = data.frame(actual = actualValues,predicted = predictedValues))
  p = p + geom_point(color = "steelblue",size = 2) + geom_smooth(color = "red",method = "lm")
  p
}

# Function to create a residual plot
createResidualPlot <- function(actualValues, predictedValues){
  res = actualValues - predictedValues
  p = ggplot(aes(x = actual, y = residuals)
                    ,data = data.frame(actual = actualValues
                                       ,residuals = res))
  p = p + geom_point(color = "steelblue", size = 2)
  p = p + geom_hline(yintercept = 0, color = "red")
  p
}


# Random Forest
randForModel <- randomForest(downloads ~ ., data = trainingData, ntree = 100)
summary(randForModel)
r_Train <- cor(trainingData$downloads, predict(randForModel, trainingData))
r_Train
r2_Train <- rSquared(trainingData$downloads, (trainingData$downloads - predict(randForModel, trainingData)))
r2_Train
mse_Train <- mean((trainingData$downloads - predict(randForModel, trainingData))^2)
mse_Train
rmse_Train <- sqrt(mse_Train)
rmse_Train
mae_Train <- funcMAE(trainingData$downloads,predict(randForModel,trainingData))
mae_Train

#Scatterplot
predVsAct_Train <- createPredVsAcSP(actualValues = trainingData$downloads
                 ,predictedValues = predict(randForModel,trainingData))
predVsAct_Train

# Residual Plot
residualPlotTrain <- createResidualPlot(actualValues = trainingData$downloads,predictedValues = predict(randForModel,trainingData))
residualPlotTrain

# Predictor Importance
importance(randForModel, type = 2)
varImpPlot(randForModel)

# Applying model to testing data
testPred <- predict(randForModel, testingData)
r_Test <- cor(testingData$downloads,testPred)
r_Test
r2_Test <- rSquared(testingData$downloads, (testingData$downloads-testPred))
r2_Test
mse_Test <- mean((testingData$downloads-testPred)^2)
mse_Test
rmse_Test <- sqrt(mse_Test)
rmse_Test
mae_Test <- funcMAE(testingData$downloads,testPred)
mae_Test


#Scatterplot
predVsAct_Test <- createPredVsAcSP(actualValues = testingData$downloads
                                 ,predictedValues = testPred)
predVsAct_Test

# Residual Plot
residualPlotTest <- createResidualPlot(actualValues = testingData$downloads,predictedValues = testPred)
residualPlotTest

#**************************** CROSS VALIDATION*****************
train_control <- trainControl(method = 'cv', number = 4)
model <- train(downloads~., data = main_subset, trControl = train_control, method = 'rf', metric = 'RMSE', importance = TRUE)
model
predictors(model)
varImp(model)

sink(NULL)
