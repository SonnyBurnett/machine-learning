#
# KNN - Nearest Neighbor Algorithm
#

#
# Read the CSV file and put it in a data frame
# https://www.kaggle.com/niharika41298/nutrition-details-for-most-common-foods 
#

#
# Step 1: Read the data
#

currwd <- getwd()
setwd("~/machinelearning/data")
food_set <- read.csv(file="food.csv")

#
# Step 2: Data Cleaning
#
# Replace non-numerical characters with 0
#

food_set$caloriesN <- gsub("t","0",food_set$Calories)
food_set$proteinN <- gsub("t","0",food_set$Protein) 
food_set$fatN <- gsub("t","0",food_set$Fat) 
food_set$satFatN <- gsub("t","0",food_set$Sat.Fat) 
food_set$fiberN <- gsub("t","0",food_set$Fiber) 
food_set$fiberN <- gsub("a","0",food_set$fiberN) 
food_set$carbsN <- gsub("t","0",food_set$Carbs) 

#
# Replace comma (,) 
#

food_set$caloriesN <- gsub(",","",food_set$caloriesN)
food_set$proteinN <- gsub(",","",food_set$proteinN) 
food_set$fatN <- gsub(",","",food_set$fatN) 
food_set$satFatN <- gsub(",","",food_set$satFatN) 
food_set$fiberN <- gsub(",","",food_set$fiberN) 
food_set$carbsN <- gsub(",","",food_set$carbsN)
food_set$gramsN <- gsub(",","",food_set$Grams) 

#
# Replace - with a dot to get rid of ranges
#

food_set$caloriesN <- gsub("-",".",food_set$caloriesN)
food_set$proteinN <- gsub("-",".",food_set$proteinN) 
food_set$fatN <- gsub("-",".",food_set$fatN) 
food_set$satFatN <- gsub("-",".",food_set$satFatN) 
food_set$fiberN <- gsub("-",".",food_set$fiberN) 
food_set$carbsN <- gsub("-",".",food_set$carbsN)
food_set$gramsN <- gsub("-",".",food_set$gramsN)

#
# Remove single quotes
#

food_set$caloriesN <- gsub("'","",food_set$caloriesN)
food_set$proteinN <- gsub("'","",food_set$proteinN) 
food_set$fatN <- gsub("'","",food_set$fatN) 
food_set$satFatN <- gsub("'","",food_set$satFatN) 
food_set$fiberN <- gsub("'","",food_set$fiberN) 
food_set$carbsN <- gsub("'","",food_set$carbsN)
food_set$gramsN <- gsub("'","",food_set$gramsN)

typeof(food_set$caloriesN)

# 
# Change the numbers from String to Number (Double)
#

food_set$caloriesN <- as.numeric(food_set$caloriesN)
food_set$proteinN <- as.numeric(food_set$proteinN)
food_set$fatN <- as.numeric(food_set$fatN)
food_set$satFatN <- as.numeric(food_set$satFatN) 
food_set$fiberN <- as.numeric(food_set$fiberN)
food_set$carbsN <- as.numeric(food_set$carbsN) 
food_set$gramsN <- as.numeric(food_set$gramsN) 

#
# Remove columns we do not use
#

test_set <- subset(food_set, select = c("Food", 
                                        "caloriesN", "proteinN", "fatN", 
                                        "satFatN", "fiberN", 
                                        "carbsN", "Category"))
#
# Replace all NA (no value) with a 0 (zero)
#

test_set[is.na(test_set)] <- 0

table(test_set$Category)

#
# Bundle the Fruits to one category
# And the same for Vegetables
#

test_set$Category <- gsub(" A-F","", test_set$Category)
test_set$Category <- gsub(" G-P","", test_set$Category)
test_set$Category <- gsub(" R-Z","", test_set$Category)
test_set$Category <- gsub(" A-E","", test_set$Category)
test_set$Category <- gsub(" F-P","", test_set$Category)
table(test_set$Category)

#
# Select only Fruits and Vegetables, and label them
#

test_set <- subset(test_set, (Category == "Fruits" | Category == "Vegetables"))
test_set$Category <- factor(test_set$Category, levels = c("Fruits", "Vegetables"),
                             labels = c("Fruits", "Vegetables"))

#
# Check the distribution
#

round(prop.table(table(test_set$Category)) * 100, digits = 1)

#
# re-order to get a more random subset later for training and testing
#

test_set <- test_set[order(test_set$Food),] 
head(test_set,20)

#
# Step 3: Normalize the data
# To make sure all factors count equally
#
summary(test_set)

normalize <- function(x) {
  a <- max(x)-min(x)
  if(a==0){
    return (0)
  } else {
    return ((x - min(x)) / (max(x) - min(x)))
  }
}

new_test_set <- as.data.frame(lapply(test_set[2:7], normalize))
summary(new_test_set)

#
# Step 4: split the data in a train set and a test set
#
dim(new_test_set)
round(prop.table(table(test_set$Category)) * 100, digits = 1)
table(test_set$Category)

food_train <- new_test_set[1:99, ]
food_test <- new_test_set[100:128, ]

head(food_train)
colnames(test_set)

#
# When we created our normalized data we lost the category
# We need it for the training and testing
# So we store in in a data frame
#

food_train_labels <- test_set[1:99, 8]
food_test_labels <- test_set[100:128, 8]

head(food_test_labels,25)

#
# Our algorithm is in a separate package
# We need to install it
#

#install.packages("class")
library(class)

#
# Step 5: train our model
#

food_test_pred <- knn(train = food_train, test = food_test, 
                 cl = food_train_labels, k=7)

food_test_pred
food_info <- cbind(test_set[100:128,1:8], food_test_pred)
food_info

#
# Step 6: evaluate our model
#

#install.packages("gmodels")
library(gmodels)
CrossTable(x = food_test_labels, y = food_test_pred,
           prop.chisq=FALSE)

