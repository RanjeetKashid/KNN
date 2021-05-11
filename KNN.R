##### KNN #####

library("class")
library("caret")

##### Glass classification #####

glass <- read.csv(file.choose())
table(glass$Type)
str(glass)
sum(is.na(glass))
glass$Type <- factor(glass$Type)
str(glass)
str(glass$Type)
summary(glass)


# Normalising data

norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}

glass_n <- as.data.frame(lapply(glass[1:9], norm))


# Splitting data

library("caTools")

split <- sample.split(glass$Type, SplitRatio = 0.70)
split
table(split)
glass_train <- subset(glass_n,split == TRUE)
glass_test  <- subset(glass_n,split == FALSE)
glass_train_cat <- subset(glass[,10], split == TRUE)
glass_test_cat  <- subset(glass[,10], split == FALSE)

round(prop.table(table(glass_train_cat))*100,2)
round(prop.table(table(glass_test_cat))*100,2)


# Model

glass_train_acc <- NULL
glass_test_acc <- NULL

for (i in seq(3,100,2))
{
  train_glass_pred <- knn(train=glass_train,test=glass_train,cl=glass_train_cat,k=i)
  glass_train_acc <- c(glass_train_acc,mean(train_glass_pred==glass_train_cat))
  test_glass_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_cat, k=i)
  glass_test_acc <- c(glass_test_acc,mean(test_glass_pred==glass_test_cat))
}

par(mfrow=c(1,2)) # c(1,2) => indicates 1 row and 2 columns
plot(seq(3,100,2),glass_train_acc,type="l",main="Train_accuracy",col="blue")
plot(seq(3,100,2),glass_test_acc,type="l",main="Test_accuracy",col="red")

glass_acc_neigh_df <- data.frame(list(train_acc=glass_train_acc,test_acc=glass_test_acc,neigh=seq(3,100,2)))

# Final Model

final_glass_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_cat, k=3)
table(final_glass_pred,glass_test_cat)


##### Zoo - Animal classification #####

zoo <- read.csv(file.choose())
str(zoo)

# converting columns to factor
cols <- c(2:13,15:18)
zoo[cols] <- lapply(zoo[cols],factor)
zoo1 <- zoo [,-1]
zoo1 <- zoo1[,-17]
str(zoo1)

table(zoo$type)

# Splitting data

zoo_train <- zoo1[1:70,]
zoo_test  <- zoo1[71:101,]

zoo_train_type <- zoo[1:70,18]
zoo_test_type  <- zoo[71:101,18]

prop.table(table(zoo_train_type))
prop.table(table(zoo_test_type))

# Model

zoo_train_acc <- NULL
zoo_test_acc <- NULL

for (i in seq(3,69,2))
{
  train_zoo_pred <- knn(train=zoo_train,test=zoo_train,cl=zoo_train_type,k=i)
  zoo_train_acc <- c(zoo_train_acc,mean(train_zoo_pred==zoo_train_type))
  test_zoo_pred <- knn(train = zoo_train, test = zoo_test, cl = zoo_train_type, k=i)
  zoo_test_acc <- c(zoo_test_acc,mean(test_zoo_pred==zoo_test_type))
}

par(mfrow=c(1,2)) # c(1,2) => indicates 1 row and 2 columns
plot(seq(3,69,2),zoo_train_acc,type="l",main="Train_accuracy",col="blue")
plot(seq(3,69,2),zoo_test_acc,type="l",main="Test_accuracy",col="red")

zoo_acc_neigh_df <- data.frame(list(train_acc=zoo_train_acc,test_acc=zoo_test_acc,neigh=seq(3,69,2)))

# Final Model

final_zoo_pred <- knn(train = zoo_train, test = zoo_test, cl = zoo_train_type, k=3)
table(final_zoo_pred,zoo_test_type)
