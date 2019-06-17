setwd("C:/Users/Ibai/Desktop/Part_2/Task_3/Data")
pacman::p_load(caret, corrplot, party, dplyr, ggplot2, reshape2, h2o, rpart, rpart.plot)
# Party package for creating the decision tree. Reshape for melt function. Rpart and Rpart.plot to create the decision tree
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
tree <- rpart(formula = Volume~., data= ExistingproductsTree, cp = .001)
summarytree <- summary(tree)
summarytree$variable.importance
rpart.plot(x = tree, box.palette = "RdBu", nn= TRUE, type = 1, branch = .5, clip.right.labs=FALSE)
rpart.rules(tree)

# Deleting unnecesary variables:
# Delete x5StarReviews, ProductDepth, x3StarReviews, ProductHeight, ProductWidth, ProductNum, x2StarsReviews, ShippingWeight
Existingproducts <- select(Existingproducts, -c(x5StarReviews, ProductDepth, x3StarReviews, ProductHeight, ProductWidth, ProductNum, x2StarReviews, ShippingWeight))
# Deleting x1StarReviews or NegativeServiceReview
# Should we include ProfitMargin in the model?

# Removing examples: Extended Warranty ####
Existingproducts <- Existingproducts[-(which (Existingproducts$ProductType == "ExtendedWarranty")), ]

# dummify the data
dmy <- dummyVars("~ .", data = Existingproducts)
ExistingproductsD <- data.frame(predict(dmy, newdata = Existingproducts))
# Let's check if the variable type of the dummy data
str(ExistingproductsD)
summary(ExistingproductsD)
summary(ExistingproductsD) #Summary after creating the dummies

# Outliers:


# OutlierRecognicer <- function(x, y){
#   out_neg <- c()
#   out_pos <- c()
#   for (i in 1:length(x)){
#     value <- x[i]
#     
#     if (value - (y * sd(x)) > median(x)){
#       out_pos <- c(out_pos, i)
#     }
#     if(value + (y * sd(x)) < median(x)){
#       out_neg <- c(out_neg, i)
#     }
#   }
#   print(paste("You have", length(out_neg), "numbers of outliers below the distribution"))
#   return(c(out_neg, out_pos))
# }
# 
# outlier <- apply(ExistingproductsD, 2, function(x){OutlierRecognicer(x,4)}) #2 column recognizer and 1 row recognizer

# Creation of the predictive model ####
set.seed(123)
inTraining <- createDataPartition(ExistingproductsD$Volume, 
                                  p= .75, 
                                  list=FALSE)
training <- ExistingproductsD[inTraining,]
testing <- ExistingproductsD[-inTraining,]

# Models with combinations of variables and Methods
Negat_PrM <- "Volume~ ProductTypeAccessories + ProductTypeDisplay + ProductTypeGameConsole + ProductTypeLaptop + ProductTypeNetbook + ProductTypePC + ProductTypePrinter + ProductTypePrinterSupplies + ProductTypeSmartphone + ProductTypeSoftware + ProductTypeTablet + x4StarReviews + PositiveServiceReview + NegativeServiceReview + ProfitMargin"
x1Star_PrM <- "Volume~ ProductTypeAccessories + ProductTypeDisplay + ProductTypeGameConsole + ProductTypeLaptop + ProductTypeNetbook + ProductTypePC + ProductTypePrinter + ProductTypePrinterSupplies + ProductTypeSmartphone + ProductTypeSoftware + ProductTypeTablet + x4StarReviews + PositiveServiceReview + x1StarReviews + ProfitMargin"
Negat <- "Volume~ ProductTypeAccessories + ProductTypeDisplay + ProductTypeGameConsole + ProductTypeLaptop + ProductTypeNetbook + ProductTypePC + ProductTypePrinter + ProductTypePrinterSupplies + ProductTypeSmartphone + ProductTypeSoftware + ProductTypeTablet + x4StarReviews + PositiveServiceReview + NegativeServiceReview"
x1Star <- "Volume~ ProductTypeAccessories + ProductTypeDisplay + ProductTypeGameConsole + ProductTypeLaptop + ProductTypeNetbook + ProductTypePC + ProductTypePrinter + ProductTypePrinterSupplies + ProductTypeSmartphone + ProductTypeSoftware + ProductTypeTablet + x4StarReviews + x1StarReviews + PositiveServiceReview"

M <- c("knn", "rf", "svmLinear", "lm")
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
  theme_grey() + 
  labs(title="Performance of the models", y="Performances", x="Predictive models")


# Choosing one function: The best performance comes from not using ProfitMargin attribute and from using x1StarReviews instead of NegativeServiceReviews
## Variables used: "Volume ~ ProductTypeAccessories + ProductTypeDisplay + ProductTypeExtendedWarranty + ProductTypeGameConsole + ProductTypeLaptop + ProductTypeNetbook + ProductTypePC + ProductTypePrinter + ProductTypePrinterSupplies + ProductTypeSmartphone + ProductTypeSoftware + ProductTypeTablet + Price + x4StarReviews + x2StarReviews + x1StarReviews + PositiveServiceReview + Recommendproduct + ShippingWeight"
Optimized <- x1Star
M <- c("knn", "rf", "svmLinear", "lm")

Performance <- c()
predresult <- c()
for(i in M){
  model <- train(formula(Optimized), data = training, method= i, trControl= fitControl, tuneLength = 20, preProcess = c("center","scale"))
  pred <- predict(model, testing)
  pred_metric <- postResample(testing$Volume, pred)
  Performance  <- cbind(Performance, pred_metric)
  predresult <- cbind(predresult, pred)
}
colnames(Performance) <- M
colnames(predresult) <- M
class(Performance)
class(predresult)
Performance
predresult

Performance_melt <- melt(Performance,  varnames = c("metric", "model"))
class(Performance_melt)
Performance_melt

# Graph of the optimized results ####

ggplot(Performance_melt, aes(x=model, y=value))+
  geom_col(fill= "darkblue")+
  facet_grid(metric~., scales="free") + 
  theme_classic() +
  labs(title="Performance of the models", y="Performances", x="Predictive models")

# It is very clear that the linear model is the most optimized model. However, let's compare the errors 

# Comparing the errors

predresult <- as.data.frame(predresult)

PredictionsTesting <- cbind(testing, predresult)
PredictionsTesting <- select(PredictionsTesting, -c(1:18))

# 1- Absolute Error
AbsEr <- c()
for(i in 2:ncol(PredictionsTesting)){
 AbsolutEr1 <- abs(PredictionsTesting[,1] - PredictionsTesting[,i])
 AbsEr <- cbind(AbsEr, AbsolutEr1)
}

NamesPrediction <- colnames(PredictionsTesting[-1])

NamesPredictionAE <- paste("AE", NamesPrediction, sep="_" )
# names(PredictionsTesting)
colnames(AbsEr) <- NamesPredictionAE
AbsEr <- as.data.frame(AbsEr)
AbsEr <- cbind(testing$Volume, AbsEr)
AbsEr <- rename(AbsEr, "Volume" = "testing$Volume")

AbsEr_melt <- melt(AbsEr, id.vars = "Volume")
names(AbsEr_melt) <- c("Volume","Model","Absolute_Error")

dev.off()
ggplot(AbsEr_melt, aes(Volume, Absolute_Error)) +
  geom_point() +
  facet_grid(~Model)

# 2- Relative errors

Proc <- AbsEr

RelEr <- c()

for (i in 2:ncol(Proc)){
  RelEr2_1 <- Proc[,1]/Proc[,i]
  RelEr <- cbind(RelEr, RelEr2_1)
}

NamesPredictionRE <- paste("RE", NamesPrediction, sep="_" )
colnames(RelEr) <- NamesPredictionRE
RelEr <- as.data.frame(RelEr)
RelEr <- cbind(testing$Volume, RelEr)
RelEr <- rename(RelEr, "Volume" = "testing$Volume")

RelEr_melt <- melt(RelEr, id.vars = "Volume")
names(RelEr_melt) <- c("Volume","Model","Relative_Error")

# Remove rows with Inf value
# RelEr_melt <- RelEr_melt[-c(9,10,42),]

# Plotting the Relative Error of each model:
dev.off()
ggplot(RelEr_melt, aes(Volume, Relative_Error)) +
  geom_point() +
  facet_grid(~Model)

# Final model: Linear model ####

LMRegression <- train(formula(Optimized), data = training, method= "lm", trControl= fitControl, tuneLength = 20, preProcess = c("center","scale"))
LMRegression
summary(LMRegression)
ImportanceLM <- varImp(LMRegression, scale = FALSE)
ImportanceLM
PredLM <- predict.train(LMRegression, testing)
pred_metricLM <- postResample(testing$Volume, PredLM)
pred_metricLM

# Prediction of the Incomplete data. What is the Volume of each product? ####

IncompleteData <- read.csv(file= "newproductattributes2017.csv", stringsAsFactors = FALSE, header = TRUE)
head(IncompleteData)
str(IncompleteData)
# Is there any missing value in the dataset?
anyNA(IncompleteData)
# Dummify the data:
# dummify the data
dmy <- dummyVars("~ .", data = IncompleteData)
IncompleteDataD <- data.frame(predict(dmy, newdata = IncompleteData))
# Removing the variables that hasn't been used in the Predictive model:
IncompleteDataD <- select(IncompleteDataD, -c(x5StarReviews, ProductDepth, x3StarReviews, x2StarReviews, ProductHeight, ProductWidth, ProductNum, NegativeServiceReview, ProfitMargin, BestSellersRank, ShippingWeight, Recommendproduct))

# Prediction of the incomple data:
IncompleteDataD$Volume_Prediction <- predict(LMRegression, IncompleteDataD)
write.csv(IncompleteDataD, file="C2.T3PredictedData.csv", row.names = TRUE)
  
