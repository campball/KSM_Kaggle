#KSM Consulting Data Exercise
#Alex Campbell
#8/13/2018

library(randomForest)
#setwd("X:/KSM_Kaggle")
home_data = read.csv("train.csv")

#ID is worthless
home_data$Id = NULL

#find number of NAs in columns
#shortcut thanks to https://stackoverflow.com/questions/24027605/determine-the-number-of-na-values-in-a-column
na_count <-sapply(home_data, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

#eliminate columns that are almost entire N/A
home_data$Alley = NULL
home_data$PoolQC = NULL
home_data$Fence = NULL
home_data$MiscFeature = NULL

#remove columns that provide little information
home_data$PoolArea = NULL
home_data$Condition2 = NULL

#remove FireplaceQu because lots of N/A
home_data$FireplaceQu = NULL

#generate random forest model
#random forest used here because no guarantee of linearity
#and a large number of features
forest = randomForest(SalePrice ~ ., data=home_data, na.action=na.exclude)

#thanks to https://stackoverflow.com/questions/14860078/plot-multiple-lines-data-series-each-with-unique-color-in-r
#and https://stats.stackexchange.com/questions/140474/single-column-plot-in-r-versus-gnuplot
# and https://stackoverflow.com/questions/4862178/remove-rows-with-all-or-some-nas-missing-values-in-data-frame

#plot predicted vs. actual home sale prices
home_data_complete = home_data[complete.cases(home_data), ]
#random forest model predicts well despite removal of incomplete data features
#model more often underpredicted sale price than overpredicted for outliers
#for further research, would be interesting to see if other populated features would improve prediction
plot(x=home_data_complete$SalePrice/1000, y=forest$predicted/1000,
     xlab = "Actual Sale Price ($1000s)", ylab = "Predicted Sale Price ($1000s)",
     main = "Sale Prices of Ames, Iowa Homes (2006-2010)
     n = 1094")
lines(x=seq(800000), y=seq(800000), col = "purple")
text(600, 200, "r = 0.94")
text(560, 503, "Ideal Prediction", col="purple")

#good correlation!
cor(home_data_complete$SalePrice, forest$predicted/1000)
