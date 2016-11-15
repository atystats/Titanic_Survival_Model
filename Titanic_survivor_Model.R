#fucntion for loading data into R
readData <- function(path.name, file.name, column.types, missing.types){
  read.csv(url(paste(path.name,file.name, sep = "")),
           colClasses = column.types, 
           na.strings = missing.types )
}

#Function arguments
Titanic.path <- "https://raw.github.com/wehrley/Kaggle_Titanic/master/"
train.data.file <- "train.csv"
test.data.file <- "test.csv"
missing.types <- c("NA", "")
train.column.types <- c('integer',   # PassengerId
                        'factor',    # Survived 
                        'factor',    # Pclass
                        'character', # Name
                        'factor',    # Sex
                        'numeric',   # Age
                        'integer',   # SibSp
                        'integer',   # Parch
                        'character', # Ticket
                        'numeric',   # Fare
                        'character', # Cabin
                        'factor'     # Embarked
)
test.column.types <- train.column.types[-2]
train.raw <- readData(Titanic.path, train.data.file, train.column.types, missing.types)

df.train <- train.raw

test.raw <- readData(Titanic.path, test.data.file, test.column.types, missing.types)

df.test <- test.raw

require(Amelia)

#Plot missing values
missmap(df.train, main = "Titanic Training Data - Missing value", 
        col = c("yellow","black"), legend = FALSE)


barplot(table(df.train$Survived),
        names.arg = c("Perished", "Survived"),
        main="Survived (passenger fate)", col="black")
barplot(table(df.train$Pclass), 
        names.arg = c("first", "second", "third"),
        main="Pclass (passenger traveling class)", col="firebrick")
barplot(table(df.train$Sex), main="Sex (gender)", col="darkviolet")
hist(df.train$Age, main="Age", xlab = NULL, col="brown")
barplot(table(df.train$SibSp), main="SibSp (siblings + spouse aboard)", 
        col="darkblue")
barplot(table(df.train$Parch), main="Parch (parents + kids aboard)", 
        col="gray50")
hist(df.train$Fare, main="Fare (fee paid for ticket[s])", xlab = NULL, 
     col="darkgreen")
barplot(table(df.train$Embarked), 
        names.arg = c("Cherbourg", "Queenstown", "Southampton"),
        main="Embarked (port of embarkation)", col="sienna")

require(vcd)

#mosaic plot
mosaicplot(df.train$Pclass ~ df.train$Survived, main = "Passanger fate by travelling", 
           color = TRUE, shade = FALSE, xlab = "Pclass", ylab = "Survived")

mosaicplot(df.train$Sex ~ df.train$Survived, main = "Passanger fate by Gender", 
           color = TRUE, shade = FALSE, xlab = "Sex", ylab = "Survived")

boxplot(df.train$Age ~ df.train$Survived, main = "Passenger fate by Age",
        xlab = "Survived", ylab = "Age")

mosaicplot(df.train$Embarked ~ df.train$Survived, 
           main = "Passenger fate by Point of embarkation", color = TRUE, 
           shade = FALSE, xlab = "Point of Embarkation", ylab = "Survived")

require(corrgram)
corrgram.data <- df.train

## change features of factor type to numeric type for inclusion on correlogram
corrgram.data$Survived <- as.numeric(corrgram.data$Survived)
corrgram.data$Pclass <- as.numeric(corrgram.data$Pclass)

require(plyr)
corrgram.data$Embarked <- revalue(corrgram.data$Embarked, 
                                  c("C" = 1, "Q" = 2, "S" = 3))
## generate correlogram
corrgram.vars <- c("Survived", "Pclass", "Sex", "Age", 
                   "SibSp", "Parch", "Fare", "Embarked")
corrgram(corrgram.data[,corrgram.vars], order=FALSE, 
         lower.panel=panel.ellipse, upper.panel=panel.pie, 
         text.panel=panel.txt, main="Titanic Training Data")

boxplot(df.train$Age ~ df.train$Pclass,main = "Passenger Age by Passenger class", 
        xlab = "Pclass", ylab = "Age")

head(df.train$Name, n = 10L)

getTitle <- function(data) {
  title.dot.start <- regexpr(", [A-Z]{1-20}.", data$Name, TRUE)
  title.comma.end <- regexpr(". ",substr(data$Name, title.dot.start + 2,
                             nchar(data$Name)), fixed = TRUE)
  data$Title <- substr(substr(data$Name, title.dot.start + 2,
                              nchar(data$Name)),
                       1, title.comma.end-1)
  return (data$Title)
}   
df.train$Title <- getTitle(df.train)
unique(df.train$Title)

options(digits = 2)
require(Hmisc)
bystats(df.train$Age, df.train$Title, fun = function(x)c(Mean = mean(x),
                                                         Median = median(x)))


#Imputation function
missing.val.levels <- c("Dr","Master","Miss","Mr","Mrs")

imputemedian <- function(impute.var, filter.var, var.levels){
  for (v in var.levels){
    impute.var[which(filter.var == v)] <- impute(impute.var[which(filter.var == v)])
    
  } 
  return(impute.var)
}

df.train$Age <- imputemedian(df.train$Age, df.train$Title, missing.val.levels)

df.train$Embarked[which(is.na(df.train$Embarked))] <- 'S'

#Subsetting the customers with fare less than 7
subset(df.train, Fare < 7)[order(subset(df.train, Fare < 7)$Fare, 
                                 subset(df.train, Fare < 7)$Pclass),
                           c("Age", "Title", "Pclass", "Fare")]

#imputing the missing values in fare
df.train$Fare[which(df.train$Fare == 0 )] <- NA

df.train$Fare <- imputemedian(df.train$Fare, df.train$Pclass,
                              as.numeric(levels(df.train$Pclass)))

#boxplot for age vs Title 
boxplot(df.train$Age ~ df.train$Title, main = "Passanger Age by Title", 
        xlab = "Title", ylab = "Age")


#function for assigning a new title for the pld title

changeTitles <- function(data, old.titles, new.title) {
  for (honorific in old.titles) {
    data$Title[ which( data$Title == honorific)] <- new.title
  }
  return (data$Title)
}
## Title consolidation
df.train$Title <- changeTitles(df.train, 
                               c("Capt", "Col", "Don", "Dr", 
                                 "Jonkheer", "Lady", "Major", 
                                 "Rev", "Sir"),
                               "Noble")
df.train$Title <- changeTitles(df.train, c("the Countess", "Ms"), 
                               "Mrs")
df.train$Title <- changeTitles(df.train, c("Mlle", "Mme"), "Miss")
df.train$Title <- as.factor(df.train$Title)


#Function for feature engineering

require(plyr)
require(stringr)

isEven <- function(x) x %in% c("0","2","4","6","8")
isOdd <- function(x) x %in% c("1","3","5","7","9")

featureeng <- function(data){
  data$Fate <- data$Survived
  
  data$Fate <- revalue(data$Fate, c("1" = "Survived", "0" = "Perished"))
  
  data$Boat.dibs <- "No"
  
  data$Boat.dibs[which(data$Sex == "female" | data$Age < 15)] <- "Yes"
  
  data$Boat.dibs <- as.factor(data$Boat.dibs)
  
  data$Family <- data$SibSp + data$Parch
  
  data$Fare.pp <- data$Fare/(data$Family + 1)
  
  data$Class <- revalue(data$Pclass, c("1" = "First", "2" = "Second", "3" = "Third"))
  
  data$Deck <- substring(data$Cabin, 1, 1)
  
  data$Deck[which(is.na(data$Deck))] <- "UNK"
  
  data$Deck <- as.factor(data$Deck)
  
  data$cabin.last.digit <- str_sub(data$Cabin,-1)
  
  data$Side <- "UNK"
  
  data$Side[which(isEven(data$cabin.last.digit))] <- "Starboard"
  
  data$Side[which(isOdd(data$cabin.last.digit))] <- "Port"
  
  data$Side <- as.factor(data$Side)
  
  data$cabin.last.digit <- NULL
  
  return(data)
}

df.train <- featureeng(df.train)

#variables for the model
train.keeps <- c("Fate", "Class", "Title", "Sex", 
                 "Age", "Family", "Fare.pp", "Fare",
                 "Deck", "Side", "Embarked", "Boat.dibs")

df.train.munged <- df.train[train.keeps]

#Fitting of Model
require(caret)

#splitting data into test and training dataset
set.seed(23)
training.rows <- createDataPartition(df.train.munged$Fate, p = 0.8, list = FALSE)
train.batch <- df.train.munged[training.rows,]
test.batch <- df.train.munged[-training.rows,]

Titanic.logit.1 <- glm(Fate ~ Sex + Class + Age + Family + Embarked + Fare,
                       data = train.batch, 
                       family = binomial("logit"))

Titanic.logit.1

1- pchisq(320,df = 8)
anova(Titanic.logit.1, test = "Chisq")

Titanic.logit.2 <- glm(Fate ~ Sex + Class + Age + Family + Embarked + Fare.pp,
                       data = train.batch, 
                       family = binomial("logit"))

anova(Titanic.logit.2, test = "Chisq")

glm(Fate ~ Sex + Class + Age + Family + Embarked,
    data = train.batch, 
    family = binomial("logit"))


#Computational nuances for train Function
#3 * 10-fold Cross Validation
cv.ctrl <- trainControl(method = "repeatedcv", repeats = 3, 
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE)

set.seed(35)
glm.tune.1 <- train(Fate ~ Sex + Class + Age + Family + Embarked,
                    data = train.batch,
                    method = "glm",
                    metric = "ROC",
                    trControl = cv.ctrl)

glm.tune.1

summary(glm.tune.1)


set.seed(35)
glm.tune.2 <- train(Fate ~ Sex + Class + Age + Family + I(Embarked == "S"),
                    data = train.batch,
                    method = "glm",
                    metric = "ROC",
                    trControl = cv.ctrl)

summary(glm.tune.2)


set.seed(35)

glm.tune.3 <- train(Fate ~ Sex + Class + Age + Family + Title 
                    + I(Embarked == "S"),
                    data = train.batch,
                    method = "glm",
                    metric = "ROC",
                    trControl = cv.ctrl)

summary(glm.tune.3)

glm.tune.4 <- train(Fate ~ Class + Age + Family + I(Embarked == "S")
                    + I(Title == "Mr") + I(Title == "Noble")
                    + I(Title == "Mr"&Class == "Third"),
                    data = train.batch,
                    method = "glm",
                    metric = "ROC",
                    trControl = cv.ctrl)

summary(glm.tune.4)

#Adaptive Boosting

ada.grid <- expand.grid(.iter = c(50,100),
                        .maxdepth = c(4,8),
                        .nu = c(0.1,1))


set.seed(35)
ada.tune <- train(Fate ~ Sex + Class + Age + Family + Embarked,
                  data = train.batch,
                  method = "ada",
                  metric = "ROC",
                  tuneGrid = ada.grid,
                  trControl = cv.ctrl)

plot(ada.tune)

#random forest for the training set
rf.grid = data.frame(.mtry = c(2,3))

set.seed(35)
rf.tune <- train(Fate ~ Sex + Class + Age + Family + Embarked,
                 data = train.batch,
                 method = "rf",
                 metric = "ROC",
                 tuneGrid = rf.grid,
                 trControl = cv.ctrl)

#support vector machine

set.seed(35)
svm.tune <- train(Fate ~ Sex + Class + Age + Family + Embarked,
                  data = train.batch,
                  method = "svmRadial",
                  tuneLength = 9,
                  preProcess = c("center","scale"),
                  metric = "ROC",
                  trControl = cv.ctrl)

svm.tune

plot(svm.tune)

#Confusion Matrix

#Logistic regression model
glm.pred <- predict(glm.tune.4, test.batch)
confusionMatrix(glm.pred, test.batch$Fate)

#Boosted Model

ada.pred <- predict(ada.tune, test.batch)
confusionMatrix(ada.pred, test.batch$Fate)


#Random Forest

rf.pred <- predict(rf.tune, test.batch)
confusionMatrix(rf.pred, test.batch$Fate)

# Support vector machines

svm.pred <- predict(svm.tune, test.batch)
confusionMatrix(svm.pred , test.batch$Fate)


#ROC curve for each algorithms

#Logistic regresion model
glm.probs <- predict(glm.tune.4, test.batch, type = "prob")
glm.ROC <- roc(response = test.batch$Fate, predictor = glm.probs$Survived, levels = levels(test.batch$Fate))

plot(glm.ROC, type = "S")
#Ada Boosting
ada.probs <- predict(ada.tune, test.batch, type = "prob")
ada.ROC <- roc(response = test.batch$Fate, predictor = ada.probs$Survived, levels = levels(test.batch$Fate))

plot(ada.ROC, add = TRUE, col = "green")

#random forest
rf.probs <- predict(rf.tune, test.batch, type = "prob")
rf.ROC <- roc(response = test.batch$Fate, predictor = rf.probs$Survived, levels = levels(test.batch$Fate))

plot(rf.ROC, add = TRUE, col = "red")

#SVM Algo
svm.probs <- predict(svm.tune, test.batch, type = "prob")
svm.ROC <- roc(response = test.batch$Fate, predictor = svm.probs$Survived, levels = levels(test.batch$Fate))

plot(svm.ROC, add = TRUE, col = "blue")


cv.values <- resamples(list(Logit = glm.tune.4, Ada = ada.tune, 
                            RF = rf.tune, SVM = svm.tune))
dotplot(cv.values, metric = "ROC")


#predicting the test data
df.test$Title <- getTitle(df.test)

# impute missing Age values
df.test$Title <- changeTitles(df.test, c("Dona", "Ms"), "Mrs")
titles.na.test <- c("Master", "Mrs", "Miss", "Mr")
df.test$Age <- imputemedian(df.test$Age, df.test$Title, titles.na.test)

# consolidate titles
df.test$Title <- changeTitles(df.test, c("Col", "Dr", "Rev"), "Noble")
df.test$Title <- changeTitles(df.test, c("Mlle", "Mme"), "Miss")
df.test$Title <- as.factor(df.test$Title)

# impute missing fares
df.test$Fare[ which( df.test$Fare == 0)] <- NA
df.test$Fare <- imputemedian(df.test$Fare, df.test$Pclass, 
                              as.numeric(levels(df.test$Pclass)))
# add the other features
df.test <- featureeng(df.test)

# data prepped for casting predictions
test.keeps <- train.keeps[-1]
pred.these <- df.test[test.keeps]

# use the logistic regression model to generate predictions
Survived <- predict(ada.tune, newdata = pred.these)

# reformat predictions to 0 or 1 and link to PassengerId in a data frame
Survived <- revalue(Survived, c("Survived" = 1, "Perished" = 0))
predictions <- as.data.frame(Survived)
predictions$PassengerId <- df.test$PassengerId

# write predictions to csv file for submission to Kaggle
write.csv(predictions[,c("PassengerId", "Survived")], 
          file="E:/Titanic_predictions.csv", row.names=FALSE, quote=FALSE)