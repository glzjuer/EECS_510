# Titanic:using a Random Forest Model

# Use script to import data
setwd("~/Documents/github/EECS510/titanic")
train_original <- read.csv("~/Documents/github/EECS510/data/train_original.csv")
test <- read.csv("~/Documents/github/EECS510/data/test.csv")

# load and install packages for DT and Random Forest
# install.packages('randomForest')
# install.packages('party')
library(rpart)
library(randomForest)
library(party)

# use rbind join the test and train together for further operation
test$Survived <- NA
total <- rbind(train_original, test)

# let the Name become text string
total$Name <- as.character(total$Name)

# operation on titles
total$Title <- sapply(total$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
total$Title <- sub(' ', '', total$Title)
# combine some rare titles
total$Title[total$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
total$Title[total$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
total$Title[total$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
# back to the factor
total$Title <- factor(total$Title)

# operations on family size
total$FamilySize <- total$SibSp + total$Parch + 1
# operations based on Name
total$Surname <- sapply(total$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
total$FamilyID <- paste(as.character(total$FamilySize), total$Surname, sep="")
total$FamilyID[total$FamilySize <= 2] <- 'Small'
# clear wrong family IDs
famIDs <- data.frame(table(total$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
total$FamilyID[total$FamilyID %in% famIDs$Var1] <- 'Small'
# Back to factor
total$FamilyID <- factor(total$FamilyID)

# Fill the Age NAs
summary(total$Age)
# predict age in a continuous variable
Agemodel <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, data=total[!is.na(total$Age),], method="anova")
total$Age[is.na(total$Age)] <- predict(Agemodel, total[is.na(total$Age),])

# check the total now
# summary(total)
# operation on missing Embarked value
# summary(total$Embarked)

# Fill in Embarked blanks
summary(total$Embarked)
which(total$Embarked == '')
total$Embarked[c(62,830)] = "S"
total$Embarked <- factor(total$Embarked)

# Fill the missing value in Fare
summary(total$Fare)
which(is.na(total$Fare))
total$Fare[1044] <- median(total$Fare, na.rm=TRUE)

# Random Forest in R can only deal with up to 32 levels here we totalne family with small familysize
total$FamilyID2 <- total$FamilyID
total$FamilyID2 <- as.character(total$FamilyID2)
total$FamilyID2[total$FamilySize <= 3] <- 'Small'
total$FamilyID2 <- factor(total$FamilyID2)

#split total back to test and train
train_original <- total[1:891,]
test <- total[892:1309,]

#build RandomForest
set.seed(213)
model <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2,
                    data=train_original, importance=TRUE, ntree=2000)
# check variable importance in Random Forest method here, roughly 37% data will be left out, but they 
# are not wasted, we can use them to see how well each tree performs on unseen data
varImpPlot(model)

# Make a prediction, output submitting file
Prediction <- predict(model, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "~/Documents/github/EECS510/model/Ramdom_Forest.csv", row.names = FALSE)

# Build a condition inference tree Random Forest
# using a statistical test rather than a purity measure
set.seed(213)
model <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train_original, controls=cforest_unbiased(ntree=2000, mtry=3)) 
# Give the prediciton
Prediction <- predict(model, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "~/Documents/github/EECS510//model/Conditional_Inferencetree_forest.csv", row.names = FALSE)













