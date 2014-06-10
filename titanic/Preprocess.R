#Use script to import data
setwd("~/Documents/github/EECS510/titanic")
train_original <- read.csv("~/Documents/github/EECS510/data/train_original.csv")
test <- read.csv("~/Documents/github/EECS510/data/test.csv")
# Install and load required packages for fancy decision tree plotting
# install.packages('Amelia')
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

test$Survived <- NA
total <- rbind(train_original,test)

require(Amelia)
missmap(total, main="Titanic Data - Missings Map", col=c("black", "white"), legend=FALSE)

total$Name <- as.character(total$Name)
strsplit(total$Name[1],split='[,.]')
strsplit(total$Name[1],split='[,.]')[[1]]
strsplit(total$Name[1],split='[,.]')[[1]][2]

# take out title
total$Title <- sapply(total$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
total$Title <- sub(' ', '', total$Title)
table(total$Title)

# total similar titles
total$Title[total$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
total$Title[total$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
total$Title[total$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

# convert back to a factor
total$Title <- factor(total$Title)

# include the influence of family
total$FamilySize <- total$SibSp + total$Parch +1

# dealing with family
total$Surname <- sapply(total$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
total$FamilyID <- paste(as.character(total$FamilySize), total$Surname, sep="")
total$FamilyID[total$FamilySize <= 2] <- 'Small'

# Check FamilyID
table(total$FamilyID)

# delete the wrong familyID
famIDs <- data.frame(table(total$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
total$FamilyID[total$FamilyID %in% famIDs$Var1] <- 'Small'

# Back to factor
total$FamilyID <- factor(total$FamilyID)

# split back to two sets
train_original <- total[1:891,]
test <- total[892:1309,] 

# Build a new tree with our new features
model <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,data=train_original, method="class")
fancyRpartPlot(model)

# Make a prediction
Prediction <- predict(model, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "~/Documents/github/EECS510//model/improved_DTree.csv", row.names = FALSE)



