# Import data
mainTest = read.csv("test (data sci challenge).csv")

testOriginal = mainTest

# Find data types of columns
str(mainTest)

############ Converting datatypes
mainTest$rank_category = factor(mainTest$rank_category)
mainTest$app_category = factor(mainTest$app_category)
mainTest$app_subcategory = factor(mainTest$app_subcategory)

# Generate new columns
Day = format(as.Date(mainTest$date,format="%m/%d/%y"), "%d")

# Add columns to dataframe
mainTest$day <- as.integer(Day)

# Eliminating unnecessary columns
mainTest = subset(mainTest, select =  -c(app_id,date))

# Normalizing Rank
mainTest$ranks <- (mainTest$rank - min(mainTest$rank)) / (max(mainTest$rank) - min(mainTest$rank))
summary(main_subset$ranks)

# Loop through the test data and assign bins to rank_category and app_subcategory
# For the additional level 31, assign the same to which category 30 is assigned. 

for( i in 1 : nrow(mainTest)){
  # Get the category
  app_subcategory_temp = as.character(mainTest$app_subcategory[i])
  rank_category_temp = as.character(mainTest$rank_category[i])
  # Get the bins for these categories from downloadsByAppSubCat and downloadsByRankCat tables
  app_subcatbin_temp = (filter(downloadsByAppSubCat, App_SubCategory == app_subcategory_temp))$Bin_AppSubCat
  rank_bin_temp = (filter(downloadsByRankCat, Rank_Category == rank_category_temp))$Bin_RankCat
  
  # The length of the variable will be zero when it doesn't find the level in reference table, i.e., for 31. 
  # In that case, we will get the bin of category 30 and assign it to the row
  if(length(rank_bin_temp) == 0 || length(app_subcatbin_temp) == 0 ){
    mainTest$Bin_AppSubCat[i] = (filter(downloadsByAppSubCat, App_SubCategory == 30))$Bin_AppSubCat
    mainTest$Bin_RankCat[i] = (filter(downloadsByRankCat, Rank_Category == 30))$Bin_RankCat
  }else{
    mainTest$Bin_AppSubCat[i] = rank_bin_temp
    mainTest$Bin_RankCat[i] = app_subcatbin_temp
  }
}

# Remove rank_category, app_subcategory
mainTest = subset(mainTest, select = -c(rank,rank_category,app_subcategory))

str(mainTest)

# Convert the character type columns to Factor type
mainTest$Bin_AppSubCat = as.factor(mainTest$Bin_AppSubCat)
mainTest$Bin_RankCat = as.factor(mainTest$Bin_RankCat)

# Converting logical to binary
cols <- sapply(mainTest, is.logical)
mainTest[,cols] <- lapply(mainTest[,cols], as.integer)
head(mainTest)


# Match training and testing factor levels
for(colName in names(mainTest)){
  if(is.factor(mainTest[[colName]])){
    levels(mainTest[[colName]]) = levels(main_subset[[colName]])
  }
}

mainPred = predict(randForModel, mainTest) # New factor levels not present in the training data

PredictedTestingData = testOriginal
PredictedTestingData$PredictedDownloads = round(mainPred)

PredictedTestingData$PredictedDownloads

#******CROSS VALIDATION*****
pred <- predict(model, mainTest)
mainTest$downloads_pred <- round(pred)

write.csv(PredictedTestingData, "Output1.csv")
write.csv(mainTest, "Final_Output2.csv")




