setwd("D:/R programming/Titanic")
a = read.csv("train.csv")
b = read.csv("test.csv")
library(dplyr)
library(rpart)
library(randomForest)
library(ggplot2)
library(stringr)
library(rpart.plot)
library(caret)
library(pROC)
#creating a variable in test
b$Survived <- NA
#combine  train and test datas
full  <- rbind(a, b)
str(full)
#categorical
full$Pclass<-as.factor(full$Pclass)
full$Survived<-as.factor(full$Survived)
full$Embarked<-as.factor(full$Embarked)
full$Sex<-as.factor(full$Sex)
#checking missing values
colSums(is.na(full))
table(full[, 'Embarked'])
full[full$Embarked=='',"Embarked"]<-'S'
table(full[, 'Embarked'])
#check age missing 
table(is.na(full$Age))
age.median <- median(full$Age, na.rm = TRUE)
full[is.na(full$Age),"Age"] <- age.median
#checking fare missing 
table(is.na(full$Fare))
fare.median <- median(full$Fare, na.rm = TRUE)
full[is.na(full$Fare),"Fare"] <- fare.median
#categorical
full$Name <- as.character(full$Name)
full$Name[1]
#spliting the names using string function
strsplit(full$Name[1], split='[,.]')
strsplit(full$Name[1], split='[,.]')[[1]]
strsplit(full$Name[1], split='[,.]')[[1]][2]
full$Title <- sapply(full$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
head(full)
full$Title <- sub(' ', '', full$Title)
head(full)
table(full$Title)
ggplot(full, aes(Title))+geom_bar()
#combining 2 titles using temporary vector c
full$Title[full$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
# for men titles Captain, Don, Major and Sir so combining it as a sir 
full$Title[full$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
# for ladies titles dona,lady,the countess, joknheer so combining it as a lady 
full$Title[full$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
ggplot(full,aes(Title, fill = Survived))+geom_bar()
ggplot(full, aes(Sex, fill = Survived))+geom_bar()
ggplot(full, aes(Pclass, fill = Survived))+geom_bar()
#categorical vector to factor
full$Title <- factor(full$Title)
#two variables SibSb and Parch that indicate the number of family members the passenger is travelling with.
full$FamilySize <- full$SibSp + full$Parch + 1
#title extraction 
full$Surname <- sapply(full$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
head(full)
#We used the function paste to bring two strings together, 
#and told it to separate them with nothing through the sep argument. 
#This was stored to a new column called FamilyID. 
full$FamilyID <- paste(as.character(full$FamilySize), full$Surname, sep="")
full$FamilyID[full$FamilySize <= 2] <- 'Small'
table(full$FamilyID)
famIDs <- data.frame(table(full$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
#need to overwrite any family IDs in our dataset for groups that were not correctly identified and 
#finally convert it to a factor
full$FamilyID[full$FamilyID %in% famIDs$Var1] <- 'Small'
full$FamilyID <- factor(full$FamilyID)


# let’s break them apart and do some predictions on our new fancy engineered variables
train <- full[1:891,]
test <- full[892:1309,]
#Time to do our predictions! We have a bunch of new variables, so let’s send them to a new decision tree.
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + 
               Title + FamilySize, 
             data= full, 
             method="class")
library(rattle)
library(RColorBrewer)
fancyRpartPlot(fit)
pt1 <- predict(fit, full, type = "class")
pt1
ncol(pt1)
#confusion matrix
confusionMatrix(data=pt1, reference = full$Survived)

dt.train1 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + 
               Title + FamilySize, 
             data= train, 
             method="class")
fancyRpartPlot(dt.train1)
train.p <- predict(dt.train1, train, type = "class")
train.p
#confusion matrix
confusionMatrix(data=train.p, reference = train$Survived)

#dt.test1 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + 
                    # Title + FamilySize, 
                  # data= test, 
                  # method="class")
#fancyRpartPlot(dt.test1)
#test.p <- predict(fit, test, type = "class")
#test.p
#confusion matrix
#confusionMatrix(data=train.p, reference = train$Survived)


sample(1:10, replace = TRUE)
#check missing values
summary(full$Age)
summary(full$Embarked)
summary(full$Fare)

full$FamilyID2 <- full$FamilyID
full$FamilyID2 <- as.character(full$FamilyID2)
full$FamilyID2[full$FamilySize <= 3] <- 'Small'
full$FamilyID2 <- factor(full$FamilyID2)

set.seed(123)
#model
p1 <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                      Embarked + Title + FamilySize,
                    data=train, 
                    importance=TRUE, 
                    ntree=2000)

varImpPlot(p1)
P2 <- predict(p1, test)
P2

#purity measures
library(party)
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age
               + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                 data = train, 
                 controls=cforest_unbiased(ntree=2000, mtry=1))
fp <- predict(fit, test, OOB=TRUE, type = "response")
#confusion matrix
confusionMatrix(data=fp, reference = train$Survived)


table(a$Survived)