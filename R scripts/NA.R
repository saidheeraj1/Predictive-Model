

#******************************* Scoring NA data #*************************************#
names(data.un)
data.un <- data.un[,-15]
# Find data types of columns
str(data.un)

############ Converting datatypes
data.un$rank_category = factor(data.un$rank_category)
data.un$app_category = factor(data.un$app_category)
data.un$app_subcategory = factor(data.un$app_subcategory)

# Generate new columns
Day = format(as.Date(data.un$date,format="%m/%d/%y"), "%d")

# Add columns to dataframe
data.un$day <- as.integer(Day)

# Eliminating unnecessary columns
data.un = subset(data.un, select =  -c(app_id,date))

# Normalizing 'Rank' column
summary(data.un$rank)
data.un$ranks <- (data.un$rank - min(data.un$rank)) / (max(data.un$rank) - min(data.un$rank))
summary(data.un$ranks)

# Loop through the test data and assign bins to rank_category and app_subcategory
# For the additional level 31, assign the same to which category 30 is assigned. 
for( i in 1 : nrow(data.un)){
  # Get the category
  app_subcategory_temp = as.character(data.un$app_subcategory[i])
  rank_category_temp = as.character(data.un$rank_category[i])
  # Get the bins for these categories from downloadsByAppSubCat and downloadsByRankCat tables
  app_subcatbin_temp = (filter(downloadsByAppSubCat, App_SubCategory == app_subcategory_temp))$Bin_AppSubCat
  rank_bin_temp = (filter(downloadsByRankCat, Rank_Category == rank_category_temp))$Bin_RankCat
  
  # The length of the variable will be zero when it doesn't find the level in reference table, i.e., for 31. 
  # In that case, we will get the bin of category 30 and assign it to the row
  if(length(rank_bin_temp) == 0 || length(app_subcatbin_temp) == 0 ){
    data.un$Bin_AppSubCat[i] = (filter(downloadsByAppSubCat, App_SubCategory == 30))$Bin_AppSubCat
    data.un$Bin_RankCat[i] = (filter(downloadsByRankCat, Rank_Category == 30))$Bin_RankCat
  }else{
    data.un$Bin_AppSubCat[i] = rank_bin_temp
    data.un$Bin_RankCat[i] = app_subcatbin_temp
  }
}

# Remove rank_category, app_subcategory
data.un = subset(data.un, select = -c(rank,rank_category,app_subcategory))

str(data.un)

# Convert the character type columns to Factor type
data.un$Bin_AppSubCat = as.factor(data.un$Bin_AppSubCat)
data.un$Bin_RankCat = as.factor(data.un$Bin_RankCat)

# Converting logical to binary
cols <- sapply(data.un, is.logical)
data.un[,cols] <- lapply(data.un[,cols], as.integer)
head(data.un)

# Match training and testing factor levels
for(colName in names(data.un)){
  if(is.factor(data.un[[colName]])){
    levels(data.un[[colName]]) = levels(main_subset[[colName]])
  }
}

data.un$naPred <- predict(model, data.un)

write.csv(data.un, "naPredicted.csv")
