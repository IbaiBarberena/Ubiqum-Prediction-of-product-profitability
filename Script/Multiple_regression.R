setwd("C:/Users/Ibai/Desktop/Part_2/Task_3/Data")
pacman::p_load(caret, corrplot, party, dplyr, ggplot2)
# Party package for creating the decision tree
# Importing the dataset:
Existingproducts <- read.csv(file= "existingproductattributes2017.csv", stringsAsFactors = FALSE, header = TRUE)
# Check the type of the features:
str(Existingproducts)

# Feature selection ####
# Delete Best Seller Rank as it has missing values:
Existingproducts$BestSellersRank <- NULL

#Correlation Matrix
ExistingproductsCor <- Existingproducts[-c(1,2)]
names(ExistingproductsCor) <- c("Price","x5StarR","x4StarR","x3StarR","x2StarR","x1StarR","PositR","NegatR","RecomP","SWeight","PDepth","PWidth","PHeight","ProfMargin","Volume")
CorrData <- cor(ExistingproductsCor)
CorrData
corrplot(CorrData,
         method = "color",
         diag= FALSE,
         type = "upper",
         order = "FPC",
         addCoef.col = "black",
         tl.srt= 20,
         # cl.align = "r",
         tl.col = "red",
         # tl.srt = 70,
         number.cex = 0.7,
         tl.pos = "td",
         tl.cex = 0.7)
findCorrelation(CorrData, 
                cutoff = 0.85, 
                verbose = FALSE, 
                names = TRUE,
                exact = TRUE)

# Decision Tree for finding relevant variables:
ExistingproductsTree <- ExistingproductsCor[-2]
clastree<- ctree(Volume~., 
                 data= ExistingproductsTree, 
                 controls = ctree_control(maxdepth = 6))
clastree
plot(clastree)

# The atributes X4StarReview and PositiveReviews are the most relevant.

# Deleting unnecesary variables:
# Delete x5StarReviews, ProductDepth, x3StarReviews, ProductHeight, ProductWidth, ProductNum
Existingproducts <- select(Existingproducts, -c(x5StarReviews, ProductDepth, x3StarReviews, ProductHeight, ProductWidth, ProductNum))
# Deleting x1StarReviews or NegativeServiceReview
# Existingproducts <- select(Existingproducts, -x1StarReviews)
# Should we include ProfitMargin in the model?
# Existingproducts <- select(Existingproducts, -ProfitMargin)

# dummify the data
dmy <- dummyVars("~ .", data = Existingproducts)
ExistingproductsD <- data.frame(predict(dmy, newdata = Existingproducts))
# Let's check if the variable type of the dummy data
str(ExistingproductsD)
summary(ExistingproductsD)
summary(ExistingproductsD) #Summary after creating the dummies

# Outliers ####
# Plots
ggplot(ExistingproductsD, aes(x=x4StarReviews, y=Volume)) + geom_point(color= "darkblue")
# Boxplots
boxplot(ExistingproductsD$Price)


OutlierRecognicer <- function(x, y){
  
  sd <- sd(x)
  med <- median(x)
  out_neg <- c()
  out_pos <- c()
  for (i in 1:length(x)){
    value <- x[i]
    if (value - (y * sd) > med){
      out_pos <- c(out_pos, i)
    }
    if(value + (y * sd) < med){
      out_neg <- c(out_neg, i)
    }
  }
  print(paste("You have", length(out_neg), "numbers of outliers below the distribution"))
  return(c(out_neg, out_pos))
}

outlier <- apply(ExistingproductsD, 2, function(x){OutlierRecognicer(x,4)}) #2 column recognizer and 1 row recognizer

# Creation of the predictive model ####
set.seed(123)
inTraining <- createDataPartition(ExistingproductsD$Volume, 
                                  p= .75, 
                                  list=FALSE)
training <- ExistingproductsD[inTraining,]
testing <- ExistingproductsD[-inTraining,]

# Models with combinations of variables and Methods
Negat_PrM <- "Volume~ ProductTypeAccessories + ProductTypeDisplay + ProductTypeExtendedWarranty + ProductTypeGameConsole + ProductTypeLaptop + ProductTypeNetbook + ProductTypePC + ProductTypePrinter + ProductTypePrinterSupplies + ProductTypeSmartphone + ProductTypeSoftware + ProductTypeTablet + Price + x4StarReviews + x2StarReviews + PositiveServiceReview + NegativeServiceReview + Recommendproduct + ShippingWeight + ProfitMargin"
x1Star_PrM <- "Volume~ ProductTypeAccessories + ProductTypeDisplay + ProductTypeExtendedWarranty + ProductTypeGameConsole + ProductTypeLaptop + ProductTypeNetbook + ProductTypePC + ProductTypePrinter + ProductTypePrinterSupplies + ProductTypeSmartphone + ProductTypeSoftware + ProductTypeTablet + Price + x4StarReviews + x2StarReviews + PositiveServiceReview + x1StarReviews + Recommendproduct + ShippingWeight + ProfitMargin"
Negat <- "Volume~ ProductTypeAccessories + ProductTypeDisplay + ProductTypeExtendedWarranty + ProductTypeGameConsole + ProductTypeLaptop + ProductTypeNetbook + ProductTypePC + ProductTypePrinter + ProductTypePrinterSupplies + ProductTypeSmartphone + ProductTypeSoftware + ProductTypeTablet + Price + x4StarReviews + x2StarReviews + PositiveServiceReview + NegativeServiceReview + Recommendproduct + ShippingWeight"
x1Star <- "Volume~ ProductTypeAccessories + ProductTypeDisplay + ProductTypeExtendedWarranty + ProductTypeGameConsole + ProductTypeLaptop + ProductTypeNetbook + ProductTypePC + ProductTypePrinter + ProductTypePrinterSupplies + ProductTypeSmartphone + ProductTypeSoftware + ProductTypeTablet + Price + x4StarReviews + x2StarReviews + x1StarReviews + PositiveServiceReview + Recommendproduct + ShippingWeight"

M <- c("knn", "rf", "svmLinear")
V <- c("Negat_PrM", "x1Star_PrM", "Negat","x1Star")
# Setting the parameters for creating the  predictive models:
fitControl <- trainControl(method = "repeatedcv",
                           number=10,
                           repeats = 2)
compare <- c()
for (i in V) {
  for(j in M){
  model <- train(formula(i), data = training, method= j, trControl= fitControl, tuneLength = 20, preProcess = c("center","scale"))
  pred <- predict(model, testing)
  pred_metric <- postResample(testing$Volume, pred)
  compare  <- cbind(compare, pred_metric)
}
}

names_var <- c()
for (i in V){
  for(j in M){
    names_var <- append(names_var, paste(i,j))
  }
}

colnames(compare) <- names_var
class(compare)
compare

compare_melt <- melt(compare,  varnames = c("metric", "model"))
class(compare_melt)
compare_melt

# Graphs of the results ####

ggplot(compare_melt, aes(x=model, y=value))+
  geom_col(fill= "darkblue")+
  facet_grid(metric~., scales="free") + 
  theme_classic() + 
  labs(title="Performance of the models", y="Performances", x="Predictive models")
