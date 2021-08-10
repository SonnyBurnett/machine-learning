#
# Linear Regression Prediction
#

#
# Read the CSV file and put it in a data frame
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
# Replace comma (,) with dot (.)
#

food_set$caloriesN <- gsub(",",".",food_set$caloriesN)
food_set$proteinN <- gsub(",",".",food_set$proteinN) 
food_set$fatN <- gsub(",",".",food_set$fatN) 
food_set$satFatN <- gsub(",",".",food_set$satFatN) 
food_set$fiberN <- gsub(",",".",food_set$fiberN) 
food_set$carbsN <- gsub(",",".",food_set$carbsN)
food_set$gramsN <- gsub(",",".",food_set$Grams) 

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

typeof(test_set$caloriesN)

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
# Step 4: split the data in a train set and a test set
#
dim(new_test_set)
round(prop.table(table(test_set$Category)) * 100, digits = 1)
table(test_set$Category)

food_train <- test_set[1:99, c(2,3,4,5,6,7)]
food_test <- test_set[100:128, c(2,3,4,5,6,7) ]




cor(test_set$caloriesN, test_set$satFatN)
cor(test_set$caloriesN, test_set$fatN)
cor(test_set$caloriesN, test_set$proteinN)
cor(test_set$caloriesN, test_set$fiberN)
cor(test_set$caloriesN, test_set$carbsN)

x <- test_set$carbsN
y <- test_set$caloriesN
plot(x, y, main = "Fruit&Veggie",
     xlab = "Carbs", ylab = "Calories",
     pch = 19, col = "red", frame = FALSE)
abline(lm(y ~ x, data = test_set), col = "blue")

# x <- test_set$carbsN
# y <- test_set$caloriesN
# plot(x, y, main = "Regression",
#      xlab = "independent var", ylab = "dependent var",
#      pch = 19, col = "red", frame = FALSE)
# 
# x <- test_set$fiberN
# y <- test_set$caloriesN
# plot(x, y, main = "Regression",
#      xlab = "independent var", ylab = "dependent var",
#      pch = 19, col = "red", frame = FALSE)

food_reg_model <- lm(caloriesN ~ ., data = food_train)
summary(food_reg_model)

food_reg_pred <- as.data.frame(predict(food_reg_model,food_test,type = "response"))

names(food_reg_pred)[1] <- "cal_predict"
food_reg_pred$cal_predict <- round(food_reg_pred$cal_predict)
food_tmp <- test_set[100:128, ]
food_tmp <- subset(food_tmp, select = c("Food", "caloriesN", "Category"))
food_outcome <- cbind(food_tmp,food_reg_pred$cal_predict)
names(food_outcome)[4] <- "cal_predict"
food_outcome <- food_outcome[ ,c(1,3,2,4)]
diff_cal <- as.data.frame(round(food_outcome$caloriesN - food_outcome$cal_predict))
food_outcome <- cbind(food_outcome, diff_cal)
names(food_outcome)[5] <- "difference"
food_outcome <- food_outcome[ ,c(1,2,5,3,4)]
perc_diff <- as.data.frame(abs(round((food_outcome$difference/food_outcome$caloriesN)*100)))
food_outcome <- cbind(food_outcome, perc_diff)
names(food_outcome)[6] <- "percentage"

print(food_outcome, row.names = FALSE)
