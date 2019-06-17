Library: 
  
  pacman::p_load(caret, corrplot, party, dplyr, ggplot2, reshape2, h2o, rpart, rpart.plot)


# The function used for removing the outliers of the dataset:


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
#   return(c(out_neg, out_pos))
# }


# Importing the dataset:

Existingproducts <- read.csv(file= "C:/Users/Ibai/Desktop/Part_2/Task_3/Data/existingproductattributes2017.csv", stringsAsFactors = FALSE, header = TRUE)


## Preprocessing the data

str(Existingproducts)
names(Existingproducts)


# All the irrelevant observations have been removed from the dataset: Extended Warranties

Existingproducts <- Existingproducts[-(which (Existingproducts$ProductType == "ExtendedWarranty")), ] 


# I have also changed the categorical variables into binary variables in order to create a regression model.

dmy <- dummyVars("~ .", data = Existingproducts)
ExistingproductsD <- data.frame(predict(dmy, newdata = Existingproducts))


### Feature selection ####

# Removing features with missing values:
ExistingproductsD$BestSellersRank <- NULL


# Correlation matrix: What is the correlation between all the features:

correlationmatrix <- cor(ExistingproductsD[,-c(1:12)])

corrplot(correlationmatrix,
         method = "color",
         diag= FALSE,
         type = "upper",
         order = "FPC",
         addCoef.col = "black",
         tl.srt= 50,
         # cl.align = "r",
         tl.col = "red",
         # tl.srt = 70,
         number.cex = 0.46,
         # tl.pos = "td",
         tl.cex = 0.55)


# Decision tree for finding relevant variables:


tree <- rpart(formula = Volume~., data= ExistingproductsD[, -(c(12,14))], cp = .001 )
rpart.plot(x = tree, box.palette = "RdBu", nn= TRUE, type = 1, branch = .5, clip.right.labs=FALSE)

rpart.plot(x = tree, box.palette = "RdBu", nn= TRUE, type = 1, branch = .5, clip.right.labs=FALSE)


# Importance of each variable:

summarytree <- summary(tree)

summarytree$variable.importance


# All the variables that have high correlation with other independent variables have been removed, as well as, the variables that are not relevant to the Sales Volume. 
# About the x5StarReviews, it has a correlation of 1 with the dependent variable, so it has been removed. It is very likely that the information of 5 stars is not correct.


Existingproducts <- select(Existingproducts, -c("x5StarReviews", "ProductDepth", "x3StarReviews", "ProductHeight", "ProductWidth", "x2StarReviews", "ShippingWeight", "NegativeServiceReview"))


# Removing outliers manually (without the function used above)

Existingproducts <- Existingproducts[which(Existingproducts$Volume < 5999),]



# Partition of training and testing



set.seed(123)
partition<-createDataPartition(y = Existingproducts$Volume, times = 1,p = 0.75,
                               list = FALSE )
training<- Existingproducts[partition,]
testing<- Existingproducts[-partition,]

training2<-training[,-which(names(training)%in%"ProductNum")]

names(training2)

# Models with combinations of variables and Methods

Function1 <- "Volume~  x4StarReviews + PositiveServiceReview + x1StarReviews + ProfitMargin"
Function2 <- "Volume~ x4StarReviews + PositiveServiceReview + x1StarReviews"
Function3 <- "Volume~ x4StarReviews + PositiveServiceReview"

M <- c("knn", "rf", "svmLinear", "lm")
V <- c("Function1", "Function2", "Function3")


# Setting the parameters for creating the  predictive models:

fitControl <- trainControl(method = "repeatedcv",
                           number=10,
                           repeats = 2)


# Predictive model:

compare <- c()
for (i in V) {
  for(j in M){
    model <- train(formula(i), data = training2, method= j, trControl= fitControl, tuneLength = 20, preProcess = c("center","scale"))
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



# Using "Function3 <- "Volume~ x4StarReviews + PositiveServiceReview"". I get the best results. 
# Let's compare this function with different predictive models.


Optimized <- "Volume~ x4StarReviews + PositiveServiceReview"
M <- c("knn", "rf", "svmLinear", "lm")

Performance <- c()
predresult <- c()
for(i in M){
  model <- train(formula(Optimized), data = training2, method= i, trControl= fitControl, tuneLength = 20, preProcess = c("center","scale"))
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
  geom_bar(stat= "identity", aes(fill= "model"))+
  stat_summary(fun.y = max, colour="black", geom="text",
               vjust=+1,
               # position = position_nudge(x = 0, y = -0.12),
               aes(label=round(..y.., digits=4))) +
  facet_grid(metric~., scales="free") + 
  scale_fill_brewer(palette = "Spectral") +
  labs(title="Performance of the models", y="Performances", x="Predictive models")


# It is very clear that the linear model is the most optimized model. However, let's compare the errors 

predresult <- as.data.frame(predresult)

PredictionsTesting <- cbind(testing, predresult)
PredictionsTesting <- select(PredictionsTesting, -c(1:9))


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
# AbsEr <- rename(AbsEr, replace = c("Volume" = "testing$Volume"))

AbsEr_melt <- melt(AbsEr, id.vars = "testing$Volume")
names(AbsEr_melt) <- c("Volume","Model","Absolute_Error")


# Plotting the absolute error:


# dev.off()
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
# RelEr <- rename(RelEr, "Volume" = "testing$Volume")

RelEr_melt <- melt(RelEr, id.vars = "testing$Volume")
names(RelEr_melt) <- c("Volume","Model","Relative_Error")


# Plotting the Relative Error of each model:

# dev.off()
ggplot(RelEr_melt, aes(Volume, Relative_Error)) +
  geom_point() +
  facet_grid(~Model)


### Final model: Linear model

# The most optimized model among all the predictive model is the Linear model

LMRegression <- train(formula(Optimized), data = training, method= "lm", trControl= fitControl, tuneLength = 20, preProcess = c("center","scale"))
LMRegression
summary(LMRegression)
ImportanceLM <- varImp(LMRegression, scale = FALSE)
ImportanceLM
PredLM <- predict.train(LMRegression, testing)
pred_metricLM <- postResample(testing$Volume, PredLM)
pred_metricLM


###Prediction of the Incomplete data
IncompleteData <- read.csv(file= "C:/Users/Ibai/Desktop/Part_2/Task_3/Data/newproductattributes2017.csv")
head(IncompleteData)
str(IncompleteData)
# Is there any missing value in the dataset?
anyNA(IncompleteData)
# Dummify the data:
dmy <- dummyVars("~ .", data = IncompleteData)
IncompleteData <- data.frame(predict(dmy, newdata = IncompleteData))
# Removing the variables that hasn't been used in the Predictive model:
IncompleteData <- select(IncompleteDataD, -c(x5StarReviews, ProductDepth, x3StarReviews, x2StarReviews, ProductHeight, ProductWidth, ProductNum, NegativeServiceReview, ProfitMargin, BestSellersRank, ShippingWeight, Recommendproduct))


# Prediction of the incomple data:
IncompleteData$Volume_Prediction <- predict(LMRegression, IncompleteDataD)


# Saving the data in desktop:

write.csv(IncompleteData, file="C2.T3PredictedData.csv", row.names = TRUE)