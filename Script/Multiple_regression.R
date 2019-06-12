setwd("C:/Users/Ibai/Desktop/Part_2/Task_3/Data")
pacman::p_load(caret, corrplot, party)
# Party for creating the decision tree
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
# dummify the data
dmy <- dummyVars("~ .", data = Existingproducts)
ExistingproductsD <- data.frame(predict(dmy, newdata = Existingproducts))
# Let's check if the variable type of the dummy data
str(ExistingproductsD)
summary(ExistingproductsD)
summary(ExistingproductsD) #Summary after creating the dummies

# Outliers ####
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
