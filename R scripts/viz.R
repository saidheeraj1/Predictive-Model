# Barplot - country
bpcountry = ggplot(data = main_subset, aes(x = country)) 
bpcountry = bpcountry + geom_bar(fill = "brown", color = "black")
bpcountry = bpcountry + labs(title = "Country Distribution", x= "Country", y = "Count")
bpcountry = bpcountry + theme(axis.title = element_text(size = 16), axis.text = element_text(size = 11), title = element_text(size = 18))
bpcountry


# Histogram of Rank with density curve
histRank <- ggplot(main_subset, aes( x = rank))
histRank <- histRank + geom_histogram(binwidth = 20
                                      ,aes (y = ..density..)
                                      ,fill = I("Brown")
                                      ,colour = I("Black"))
histRank <- histRank + geom_density(fill = "blue", alpha = 0.3)
histRank <- histRank + labs(title = "Histogram of Rank", x = "Rank", y = "Density")
histRank <- histRank + scale_x_continuous(breaks = 100*(0:15))
histRank = histRank + theme(axis.title = element_text(size = 16), axis.text = element_text(size = 11), title = element_text(size = 18))
histRank

# Barplot - rank_category
bpRankCat = ggplot(data = main_subset, aes(x = rank_category)) 
bpRankCat = bpRankCat + geom_bar(fill = "brown", color = "black")
bpRankCat = bpRankCat + labs(title = "Rank_Category Distribution", x= "Rank_Category", y = "Count")
bpRankCat = bpRankCat + theme(axis.title = element_text(size = 16), axis.text = element_text(size = 11), title = element_text(size = 18))
bpRankCat

# Barplot - rank_kind
bpRankkind = ggplot(data = main_subset, aes(x = rank_kind)) 
bpRankkind = bpRankkind + geom_bar(fill = "brown", color = "black")
bpRankkind = bpRankkind + labs(title = "Rank_Kind Distribution", x= "Rank_Kind", y = "Count")
bpRankkind = bpRankkind + theme(axis.title = element_text(size = 16), axis.text = element_text(size = 11), title = element_text(size = 18))
bpRankkind

# Barplot - age_restrictions
bpAgeRest = ggplot(data = main_subset, aes(x = age_restrictions)) 
bpAgeRest = bpAgeRest + geom_bar(fill = "brown", color = "black")
bpAgeRest = bpAgeRest + labs(title = "Age_Restrictions Distribution", x= "Age_Restrictions", y = "Count")
bpAgeRest = bpAgeRest + theme(axis.title = element_text(size = 16), axis.text = element_text(size = 11), title = element_text(size = 18))
bpAgeRest

# Barplot - offers_in_app_purchases
bpOffer = ggplot(data = main_subset, aes(x = offers_in_app_purchases)) 
bpOffer = bpOffer + geom_bar(fill = "brown", color = "black")
bpOffer = bpOffer + labs(title = "Offers_in_app_purchases Distribution", x= "Offers_In_App_Purchases", y = "Count")
bpOffer = bpOffer + theme(axis.title = element_text(size = 16), axis.text = element_text(size = 11), title = element_text(size = 18))
bpOffer

# Barplot - paid
bpPaid = ggplot(data = main_subset, aes(x = paid)) 
bpPaid = bpPaid + geom_bar(fill = "brown", color = "black")
bpPaid = bpPaid + labs(title = "Paid Distribution", x= "Paid", y = "Count")
bpPaid = bpPaid + theme(axis.title = element_text(size = 16), axis.text = element_text(size = 11), title = element_text(size = 18))
bpPaid

# Histogram of Us_Price with density curve
histPrice <- ggplot(main_subset, aes( x = us_price))
histPrice <- histPrice + geom_histogram(binwidth = 20
                                      ,aes (y = ..density..)
                                      ,fill = I("Brown")
                                      ,colour = I("Black"))
histPrice <- histPrice + geom_density(fill = "blue", alpha = 0.3)
histPrice <- histPrice + labs(title = "Histogram of Us_Price", x = "Us_Price", y = "Density")
histPrice <- histPrice + scale_x_continuous(breaks = 100*(0:15))
histPrice <- histPrice + theme(axis.title = element_text(size = 16), axis.text = element_text(size = 11), title = element_text(size = 18))
histPrice

# Barplot - app_category
bpAppCat = ggplot(data = main_subset, aes(x = app_category)) 
bpAppCat = bpAppCat + geom_bar(fill = "brown", color = "black")
bpAppCat = bpAppCat + labs(title = "App_Category Distribution", x= "App_Category", y = "Count")
bpAppCat = bpAppCat + theme(axis.title = element_text(size = 16), axis.text = element_text(size = 11), title = element_text(size = 18))
bpAppCat

# Barplot - app_subcategory
bpAppSubCat = ggplot(data = main_subset, aes(x = app_subcategory)) 
bpAppSubCat = bpAppSubCat + geom_bar(fill = "brown", color = "black")
bpAppSubCat = bpAppSubCat + labs(title = "App_SubCategory Distribution", x= "App_SubCategory", y = "Count")
bpAppSubCat = bpAppSubCat + theme(axis.title = element_text(size = 16), axis.text = element_text(size = 11), title = element_text(size = 18))
bpAppSubCat

# Barplot - total_estimated_installs
bpTotIns = ggplot(data = main_subset, aes(x = total_estimated_installs)) 
bpTotIns = bpTotIns + geom_bar(fill = "brown", color = "black")
bpTotIns = bpTotIns + labs(title = "Total_Estimated_Installs Distribution", x= "Total_Estimated_Installs", y = "Count")
bpTotIns = bpTotIns + theme(axis.title = element_text(size = 16), axis.text = element_text(size = 11), title = element_text(size = 18))
bpTotIns

# Barplot - operating_system
bpOs = ggplot(data = main_subset, aes(x = operating_system)) 
bpOs = bpOs + geom_bar(fill = "brown", color = "black")
bpOs = bpOs + labs(title = "Operating_System Distribution", x= "Operating_System", y = "Count")
bpOs = bpOs + theme(axis.title = element_text(size = 16), axis.text = element_text(size = 11), title = element_text(size = 18))
bpOs

# Histogram of downloads with density curve
histDown <- ggplot(main_subset, aes( x = downloads))
histDown <- histDown + geom_histogram(binwidth = 20
                                        ,aes (y = ..density..)
                                        ,fill = I("Brown")
                                        ,colour = I("Black"))
histDown <- histDown + geom_density(fill = "blue", alpha = 0.3)
histDown <- histDown + labs(title = "Histogram of Downloads", x = "Downloads", y = "Density")
histDown <- histDown + scale_x_continuous(breaks = 100*(0:15))
histDown <- histDown + theme(axis.title = element_text(size = 16), axis.text = element_text(size = 11), title = element_text(size = 18))
histDown

plot(main_subset$downloads)

#********* Creating frequency distribution tables for rank_category and app_subcategory *********
#************** and finiding the pattern in data to decide the binning criteria *****************
#************************** rank_category *******************************************
# Frequency distribution and density plot for rank_category
freqDistRankCat = count(main_subset, vars = "rank_category")
freqDistRankCat$freqPercent = round((freqDistRankCat$freq / sum(freqDistRankCat$freq)*100),digits = 2)
freqDistRankCat
summary(freqDistRankCat$freqPercent)

# Density plot
densityFreqDistRankCat = ggplot(data = freqDistRankCat, aes(x = freqPercent))
densityFreqDistRankCat = densityFreqDistRankCat + geom_density(fill = "skyblue",alpha = 0.6, color = "brown")
densityFreqDistRankCat = densityFreqDistRankCat + scale_x_continuous(breaks = 0:13)
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

#********************************** app_subcategory *******************************************
# Frequency distribution and density plot for app_subcategory
freqDistAppSubCat = count(main_subset, vars = "app_subcategory")
freqDistAppSubCat$freqPercent = round((freqDistAppSubCat$freq / sum(freqDistAppSubCat$freq)*100),digits = 2)
freqDistAppSubCat
summary(freqDistAppSubCat$freqPercent)

# Density plot
densityFreqDistAppSubCat = ggplot(data = freqDistAppSubCat, aes(x = freqPercent))
densityFreqDistAppSubCat = densityFreqDistAppSubCat + geom_density(fill = "skyblue",alpha = 0.6, color = "brown")
densityFreqDistAppSubCat = densityFreqDistAppSubCat + scale_x_continuous(breaks = 0:13)
densityFreqDistAppSubCat


# Downloads by app_subcategory and density distribution of downloads by app_subcategory
downloadsByAppSubCat = aggregate(main_subset$downloads, by = list(App_SubCategory = main_subset$app_subcategory), FUN = sum)
names(downloadsByAppSubCat)[2] = "SumOfDownloads"
downloadsByAppSubCat$DownByPercent = round((downloadsByAppSubCat$SumOfDownloads /sum(downloadsByAppSubCat$SumOfDownloads)*100),digits = 2)
downloadsByAppSubCat
summary(downloadsByAppSubCat$DownByPercent)

# Density plot
densityDownsAppSubCat = ggplot(data = downloadsByAppSubCat, aes(x = DownByPercent)) 
densityDownsAppSubCat =  densityDownsAppSubCat + geom_density(fill = "skyblue", color = "brown", alpha = 0.6)
densityDownsAppSubCat = densityDownsAppSubCat + scale_x_continuous(breaks = 0:37)
densityDownsAppSubCat

# ******************************* Downloads vs Rest ****************************
# Barplot - country Vs Downloads # main_subset
barPtcountry_tr = ggplot(data = main_subset, aes(x = country, y = downloads))
barPtcountry_tr = barPtcountry_tr + geom_bar(stat = "identity", fill = "brown")
barPtcountry_tr = barPtcountry_tr + labs(title = "Country Distribution with Downloads", x= "Country", y = "Downloads")
barPtcountry_tr = barPtcountry_tr + theme(axis.title = element_text(size = 16), axis.text = element_text(size = 11), title = element_text(size = 18))
barPtcountry_tr

# Scatter plot: Rank vs Downloads
spRankDown <- ggplot(main_subset, aes(x = rank, y = downloads))
spRankDown <- spRankDown + geom_point(color = "steelblue", size = 2) + geom_smooth(method = "lm", color = "red")
spRankDown <- spRankDown + labs(title = "Rank vs Downloads", x = "Rank", y = "Downloads")
spRankDown = spRankDown + theme(axis.title = element_text(size = 16), axis.text = element_text(size = 11), title = element_text(size = 18))
spRankDown

# Barplot - rank_category Vs Downloads # main_subset
barPtrankcat_tr = ggplot(data = main_subset, aes(x = rank_category, y = downloads))
barPtrankcat_tr = barPtrankcat_tr + geom_bar(stat = "identity", fill = "brown")
barPtrankcat_tr = barPtrankcat_tr + labs(title = "Rank_Category Distribution with Downloads", x= "Rank_Category", y = "Downloads")
barPtrankcat_tr = barPtrankcat_tr + theme(axis.title = element_text(size = 16), axis.text = element_text(size = 11), title = element_text(size = 18))
barPtrankcat_tr

# Barplot - rank_kind Vs Downloads # main_subset
barPtrankind_tr = ggplot(data = main_subset, aes(x = rank_kind, y = downloads))
barPtrankind_tr = barPtrankind_tr + geom_bar(stat = "identity", fill = "brown")
barPtrankind_tr = barPtrankind_tr + labs(title = "Rank_Kind Distribution with Downloads", x= "Rank_Kind", y = "Downloads")
barPtrankind_tr = barPtrankind_tr + theme(axis.title = element_text(size = 16), axis.text = element_text(size = 11), title = element_text(size = 18))
barPtrankind_tr

# Barplot - offers_in_app_purchases Vs Downloads # main_subset
barPtoiap_tr = ggplot(data = main_subset, aes(x = offers_in_app_purchases, y = downloads))
barPtoiap_tr = barPtoiap_tr + geom_bar(stat = "identity", fill = "brown")
barPtoiap_tr = barPtoiap_tr + labs(title = "OIAP Distribution with Downloads", x= "OIAP", y = "Downloads")
barPtoiap_tr = barPtoiap_tr + theme(axis.title = element_text(size = 16), axis.text = element_text(size = 11), title = element_text(size = 18))
barPtoiap_tr

# Barplot - paid Vs Downloads # main_subset
barPtpaid_tr = ggplot(data = main_subset, aes(x = paid, y = downloads))
barPtpaid_tr = barPtpaid_tr + geom_bar(stat = "identity", fill = "brown")
barPtpaid_tr = barPtpaid_tr + labs(title = "Paid Distribution with Downloads", x= "Paid", y = "Downloads")
barPtpaid_tr = barPtpaid_tr + theme(axis.title = element_text(size = 16), axis.text = element_text(size = 11), title = element_text(size = 18))
barPtpaid_tr

# Scatter plot: Us_Price vs Downloads
spusprice <- ggplot(main_subset, aes(x = us_price, y = downloads))
spusprice <- spusprice + geom_point(color = "steelblue", size = 2) + geom_smooth(method = "lm", color = "red")
spusprice <- spusprice + labs(title = "US_Price vs Downloads", x = "Us_Price", y = "Downloads")
spusprice = spusprice + theme(axis.title = element_text(size = 16), axis.text = element_text(size = 11), title = element_text(size = 18))
spusprice

# Barplot - app_category Vs Downloads # main_subset
barPtappcat_tr = ggplot(data = main_subset, aes(x = app_category, y = downloads))
barPtappcat_tr = barPtappcat_tr + geom_bar(stat = "identity", fill = "brown")
barPtappcat_tr = barPtappcat_tr + labs(title = "App_Category Distribution with Downloads", x= "App_Category", y = "Downloads")
barPtappcat_tr = barPtappcat_tr + theme(axis.title = element_text(size = 16), axis.text = element_text(size = 11), title = element_text(size = 18))
barPtappcat_tr

# Barplot - app_subcategory Vs Downloads # main_subset
barPtappsubcat_tr = ggplot(data = main_subset, aes(x = app_subcategory, y = downloads))
barPtappsubcat_tr = barPtappsubcat_tr + geom_bar(stat = "identity", fill = "brown")
barPtappsubcat_tr = barPtappsubcat_tr + labs(title = "App_SubCategory Distribution with Downloads", x= "App_SubCategory", y = "Downloads")
barPtappsubcat_tr = barPtappsubcat_tr + theme(axis.title = element_text(size = 16), axis.text = element_text(size = 11), title = element_text(size = 18))
barPtappsubcat_tr

# Barplot - total_estimated_installs Vs Downloads # main_subset
barPttei_tr = ggplot(data = main_subset, aes(x = total_estimated_installs, y = downloads))
barPttei_tr = barPttei_tr + geom_bar(stat = "identity", fill = "brown")
barPttei_tr = barPttei_tr + labs(title = "TEI Distribution with Downloads", x= "TEI", y = "Downloads")
barPttei_tr = barPttei_tr + theme(axis.title = element_text(size = 16), axis.text = element_text(size = 11), title = element_text(size = 18))
barPttei_tr

# Barplot - operating_system Vs Downloads # main_subset
barPtos_tr = ggplot(data = main_subset, aes(x = operating_system, y = downloads))
barPtos_tr = barPtos_tr + geom_bar(stat = "identity", fill = "brown")
barPtos_tr = barPtos_tr + labs(title = "OS Distribution with Downloads", x= "OS", y = "Downloads")
barPtos_tr = barPtos_tr + theme(axis.title = element_text(size = 16), axis.text = element_text(size = 11), title = element_text(size = 18))
barPtos_tr

#********************Result Plots****************
plot(model, plotType = 'line')
plot(model$finalModel)
plot(model$results)
(model$bestTune)



#****************************** ChiSquare test of independence *******************
# Null hypothesis: Variables are independent / There is no association
# Alternative Hypothesis: Variables are dependent / There is an association

attach(main_data)
chisq.test(table(country,as.factor(rank_category))) # Variables are independent, because p is > 0.05 and hence we fail to reject null hypothesis
chisq.test(table(rank_category,as.factor(app_subcategory))) # There is an association because p is < 0.05 and hence we reject null hypothesis
chisq.test(table(country, rank_kind))
chisq.test(table(country, age_restrictions))
chisq.test(table(app_category, app_subcategory))
chisq.test(table(total_estimated_installs, operating_system))
chisq.test(table(country, total_estimated_installs))
chisq.test(table(country, operating_system))
chisq.test(table(offers_in_app_purchases, paid))
detach(main_data)

#***********************ANOVA*****************
counry_aov <- aov(downloads~country, data = main_data)
summary(country)
tei_aov <- aov(downloads ~ total_estimated_installs, data = main_data)
summary(tei_aov)
os_aov <- aov(downloads ~ operating_system, data = main_data)
summary(os_aov)
kind_aov <- aov(downloads ~ rank_kind, data = main_data)
summary(kind_aov)
rankcat_aov <- aov(downloads ~ rank_category, data = main_data)
summary(rankcat_aov)
appcat_aov <- aov(downloads ~ app_category, data = main_data)
summary(appcat_aov)
appsubcat_aov <- aov(downloads ~ app_subcategory, data = main_data)
summary(appsubcat_aov)
