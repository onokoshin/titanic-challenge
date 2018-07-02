install.packages('rpart.plot')
install.packages("randomForest")
install.packages('party')
library(rpart)
library(rpart.plot)
library(randomForest)
library(party)


#Set directory where csv files are located
#setwd("~/Desktop")
setwd("~/Dropbox/Seattle University/Classes/CPSC 5610 Artificial Intelligence/Project/Project")

#To read data from csv files
titanic.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.test <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)

#To assign boolean and set train as trainset and test as testset 
titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE

#To add survived column to testSet and fill it with NA
titanic.test$Survived <- NA

#Conduct vertical join/union to create a full
titanic.full <- rbind(titanic.train, titanic.test)


#DATA CLEANING -- EMBARKED
#select two null values in Embarked and replace them with mode(most frequent one) -- S 914 
titanic.full[titanic.full$Embarked=='', "Embarked"] <- 'S'


###DATA CLEANING-- AGE  DEMO: titanic.full$Age

#Let's build a predictive model to make educated guess for data cleaning
#Use Linerar Regression Model - lm() function
#To use linear model, we must filter out outliers 

boxplot.stats(titanic.full$Age)$stats[5]

upper.whisker <- boxplot.stats(titanic.full$Age)$stats[5]
ageOutlier.filter <- titanic.full$Age < upper.whisker
titanic.full[ageOutlier.filter,] #rows that are not outliers

age.equation = "Age ~ Pclass + Sex + Fare + SibSp + Parch + Embarked"
age.model <- lm( formula = age.equation, data = titanic.full[ageOutlier.filter,])


#creates a vector with specified columns where NA values are located and assign them to NA rows
age.row <- titanic.full[is.na(titanic.full$Age), c("Pclass", "Sex", "Fare", "SibSp", "Parch", "Embarked")]
age.prediction <- predict(age.model, newdata = age.row)

#titanic.full[row, column]
titanic.full[is.na(titanic.full$Age), "Age"] <- age.prediction

#if linear regression assigns any negative values, we will just assign median instead here
age.median <- median(titanic.full$Age, na.rm = TRUE)
titanic.full[titanic.full$Age < 0, "Age"] <- age.median


###DATA CLEANING -- FARE

##User Linerar Regression Model to clean data - Fare
fare.upper.whisker <- boxplot.stats(titanic.full$Fare)$stats[5]

fareOutlier.filter <- titanic.full$Fare < fare.upper.whisker
fare.equation = "Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked"
fare.model <- lm( formula = fare.equation, data = titanic.full[fareOutlier.filter,])

#creates a vector with specified columns where NA values are located and assign them to NA rows
fare.row <- titanic.full[is.na(titanic.full$Fare), c("Pclass", "Sex", "Age", "SibSp", "Parch", "Embarked")]
fare.prediction <- predict(fare.model, newdata = fare.row)

#titanic.full[row, column]
titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.prediction


###Work on Title - extract everyone's title and apply it to a new column
titanic.full$Title <- sapply(titanic.full$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
titanic.full$Title <- sub(' ', '', titanic.full$Title)
table(titanic.full$Title)
##Clean up the data a little bit 
titanic.full$Title[titanic.full$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
titanic.full$Title[titanic.full$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
titanic.full$Title[titanic.full$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
titanic.full$Title <- as.factor(titanic.full$Title)


###Assuming that large family would have a hard time finding each other, I will combine SibSp and Parch
titanic.full$FamilySize <- titanic.full$SibSp + titanic.full$Parch + 1 #+1 for their existance

###Combining surname with 
titanic.full$Surname <- sapply(titanic.full$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
titanic.full$FamilyID <- paste(as.character(titanic.full$FamilySize), titanic.full$Surname, sep="")
titanic.full$FamilyID[titanic.full$FamilySize <= 2] <- 'Small'
table(titanic.full$FamilyID)

famIDs <- data.frame(table(titanic.full$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]

titanic.full$FamilyID[titanic.full$FamilyID %in% famIDs$Var1] <- 'Small'
titanic.full$FamilyID <- factor(titanic.full$FamilyID)

##reduce factor lelve so that it can fit in random forest 
titanic.full$FamilyID2 <- titanic.full$FamilyID
titanic.full$FamilyID2 <- as.character(titanic.full$FamilyID2)
titanic.full$FamilyID2[titanic.full$FamilySize <= 3] <- 'Small'
titanic.full$FamilyID2 <- factor(titanic.full$FamilyID2)

#Categorical casting -- cast everything except Survived cuz Survived has a bunch of NA values str
titanic.full$Pclass <- as.factor(titanic.full$Pclass) #3 lvl - 1,2,3
titanic.full$Sex <- as.factor(titanic.full$Sex) #2 lvl - female, male
titanic.full$Embarked <- as.factor(titanic.full$Embarked) #3 lvl - C, Q, S


###Start working on features and identifying them

prop.table(table(titanic.full$Sex, titanic.full$Survived),1)
##create a child variable
titanic.full$Child <- 0
titanic.full$Child[titanic.full$Age < 18] <- 1

##create aggregate function to check the data
aggregate(Survived ~ Child + Sex, data=titanic.full, FUN=function(x){sum(x)/length(x)})

##add fare and class into consideration
titanic.full$Fare2 <- '30+'
titanic.full$Fare2[titanic.full$Fare < 30 & titanic.full$Fare >= 20] <- '20-30'
titanic.full$Fare2[titanic.full$Fare < 20 & titanic.full$Fare >= 10] <- '10-20'
titanic.full$Fare2[titanic.full$Fare < 10] <- '<10'

titanic.full$Child <- as.factor(titanic.full$Child)
titanic.full$Fare2 <- as.factor(titanic.full$Fare2)
titanic.full$Survived <- as.factor(titanic.full$Survived)

#Data CLEANING -- splitting data back into Train and Test data
titanic.train <- titanic.full[titanic.full$IsTrainSet==TRUE, ]
titanic.test <- titanic.full[titanic.full$IsTrainSet==FALSE, ]

##Aggregate again to see the result
#aggregate(Survived ~ Fare2 + Pclass + Sex, data=titanic.train, FUN=function(x){sum(x)/length(x)})

##Random Forest
set.seed(415)

fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + Fare + Fare2 + Embarked + Title + FamilySize + FamilyID + Child,
               data=titanic.train, 
               controls=cforest_unbiased(ntree=2000, mtry=3))

#Make prediction and create a submission format
Prediction <- predict(fit, newdata=titanic.test)
submit <- data.frame(PassengerId = titanic.test$PassengerId, Survived = Prediction)
write.csv(submit, file = "FinalResult.csv", row.names = FALSE)


#useful commands
#tail(tatanic.train) -- shows last 6 rows 
#str(taitanic.test) -- shows columns and first few lines
#median(titanic.train$Age, na.rm=TRUE) -- shows median of an column and removes  empty
#ncol(titanic.train) -- shows number of columns
#names(titanic.train) -- shows the headers of columns
#rbind(titanic.train, titanic.test) -- row bind creates vertical join/union 
#table(titanic.full$IsTrainSet) -- shows values in one column in table manner 

#playground - scatter plot and lr line
#plot(titanic.full$Fare, titanic.full$Age)
#abline(lm(titanic.full$Fare ~ titanic.full$Age))

#prop.table(table(titanic.train$Sex, titanic.train$Survived),1)
##create a child variable
#titanic.full$Child <- 0
#titanic.full$Child[titanic.full$Age < 18] <- 1
##create aggregate function to check the data
#aggregate(Survived ~ Child + Sex, data=titanic.full, FUN=length)
#aggregate(Survived ~ Child + Sex, data=titanic.full, FUN=function(x){sum(x)/length(x)})

##add fare and class into consideration
#titanic.full$Fare2 <- '30+'
#titanic.full$Fare2[titanic.full$Fare < 30 & titanic.full$Fare >= 20] <- '20-30'
#titanic.full$Fare2[titanic.full$Fare < 20 & titanic.full$Fare >= 10] <- '10-20'
#titanic.full$Fare2[titanic.full$Fare < 10] <- '<10'

##Aggregate again to see the result
#aggregate(Survived ~ Fare2 + Pclass + Sex, data=titanic.full, FUN=function(x){sum(x)/length(x)})


##Decision Tree time
#rpart works similarly to aggregate 
#fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=titanic.full, method="class")

#to create a graph
#rpart.plot(fit)
