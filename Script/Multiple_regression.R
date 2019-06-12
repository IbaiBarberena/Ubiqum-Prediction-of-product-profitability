setwd("C:/Users/Ibai/Desktop/Part_2/Task_3/Data")
pacman::p_load(caret)
# Importing the dataset:
Existingproducts <- read.csv(file= "existingproductattributes2017.csv", stringsAsFactors = FALSE, header = TRUE)
# Check the type of the features:
str(Existingproducts)
# dummify the data
dmy <- dummyVars("~ .", data = Existingproducts)
ExistingproductsD <- data.frame(predict(dmy, newdata = Existingproducts))
# Let's check if the variable type of the dummy data
str(ExistingproductsD)
summary(ExistingproductsD)
# Delete Best Seller Rank as it has missing values:
ExistingproductsD$BestSellersRank <- NULL

