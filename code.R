
# A PROJECT MADE BY:
# MARINA GÓMEZ REY AND MARÍA ÁNGELES MAGRO GARROTE

# set your working directory -> setwd("")
rm(list=ls())

load("titanic_train.RDATA")
# FIXING THE DATA:
  # Noticing that the missing values of Cabin correspond to "". 
  # How many empty values in Cabin?
aux=which(titanic.train$Cabin =="")
length(aux)
rm(aux)

# As there are missing values, instead of erasing them, they are treated as if
# they were another value.

levels(titanic.train$Cabin)[grepl("A", levels(titanic.train$Cabin))] <- "A"
levels(titanic.train$Cabin)[grepl("B", levels(titanic.train$Cabin))] <- "B"
levels(titanic.train$Cabin)[grepl("C", levels(titanic.train$Cabin))] <- "C"
levels(titanic.train$Cabin)[grepl("D", levels(titanic.train$Cabin))] <- "D"
levels(titanic.train$Cabin)[grepl("E", levels(titanic.train$Cabin))] <- "E"
# Although there are some cabins with the "F" letters that have more 
# letters (F G73, F G63, F G73) they are saved also with a letter F.
levels(titanic.train$Cabin)[grepl("F", levels(titanic.train$Cabin))] <- "F"
levels(titanic.train$Cabin)[grepl("G", levels(titanic.train$Cabin))] <- "G"

titanic.train$Cabin=factor(titanic.train$Cabin, levels=
                             c("A","B","C","D","E","F","G","T",""),
                           labels=c(1,2,3,4,5,6,7,8,9))

# As the ticket variables is not going to be used, it is eliminated.
titanic.train$Ticket=NULL

# Sex variable into numbers:
titanic.train$Sex=factor(titanic.train$Sex, levels=
                             c("female","male"), labels=c(1,2))
# Embarked variable into numbers:
titanic.train$Embarked=factor(titanic.train$Embarked, levels=
                           c("C","Q","S"), labels=c(1,2,3))

# Making sure all the numbers are treated as integers and not as string.
titanic.train$Pclass=as.integer(titanic.train$Pclass)
titanic.train$Sex=as.integer(titanic.train$Sex)
titanic.train$Cabin=as.integer(titanic.train$Cabin)
titanic.train$Embarked=as.integer(titanic.train$Embarked)

set.seed(123)

# Importing the libraries
if (!require("caret")){
  install.packages("caret")
}
library(caret)

if (!require("rpart")){
  install.packages("rpart")
}
library("rpart")


if (!require("rattle")){
  install.packages("rattle")
}
library("rattle")

if (!require("rpart.plot")){
  install.packages("rpart.plot")
}
library("rpart.plot")

if (!require("randomForest")){
  install.packages('randomForest')
}
library("randomForest")

if (!require(ggplot2)){
  install.packages(ggplot2)
}
library(ggplot2)

# CHOOSING A TECHNIQUE

# Tree
folds = createFolds(titanic.train$Survived, k = 10)
# Hyper parameter selection
d_minsplit=seq(from=2,to=40,by=2)
d_maxdepth=seq(from=1,to=5,by=1)
d_cp=seq(from=0,to=1,by=0.1)
parametros=expand.grid(minsplit=d_minsplit,maxdepth=d_maxdepth,cp=d_cp)
grid = expand.grid(minsplit = seq(2, 40, 2), maxdepth = seq(1, 5, 1), cp = seq(0, 1, 0.1))


cv_hyper = apply(parametros,1,function(y){
  cv = lapply(folds, function(x) {
    # Select training and test set according to current split
    training_set = titanic.train[-x,]
    test_set = titanic.train[x,]
    mytree=rpart(formula=Survived ~., data=training_set, method="class",
                 control = rpart.control(minsplit=y[1],minbucket=y[2],cp=y[3]))
    # Use the function predict to apply the classification algorithm
    # with test set
    pred = predict(mytree,test_set,type="class")
    # Compute the confusion matrix
    conf_matrix = table(test_set$Survived,pred,dnn=c("Actual value","Classifier prediction"))
    conf_matrix_prop = prop.table(conf_matrix)
    
    # Compute error estimates
    accuracy = sum(diag(conf_matrix))/sum(conf_matrix)
    precision = conf_matrix[1,1]/sum(conf_matrix[,1])
    specificity = conf_matrix[2,2]/sum(conf_matrix[,2])
    
    return(c(accuracy,precision,specificity))
  })
  cv = t(matrix(unlist(cv),nrow=3))
  accuracies = apply(X=cv,MARGIN=2,FUN = "mean")  
  
  return(accuracies)
  
})

plot(cv_hyper[1,])
cv_hyper[1,which.max(cv_hyper[1,])]
parametros[which.max(cv_hyper[1,]),]


ggplot(grid)+aes(x = cp, y = cv_hyper[1,], color = as.factor(minsplit))+
  geom_point(size = 0.5) + geom_line(size = 0.5) +
  theme(text = element_text(size = 14))

# Preprocessing again in order to avoid mistakes and start
# testing the random forest
rm(list=ls())
load("titanic_train.RDATA")
levels(titanic.train$Cabin)[grepl("A", levels(titanic.train$Cabin))] <- "A"
levels(titanic.train$Cabin)[grepl("B", levels(titanic.train$Cabin))] <- "B"
levels(titanic.train$Cabin)[grepl("C", levels(titanic.train$Cabin))] <- "C"
levels(titanic.train$Cabin)[grepl("D", levels(titanic.train$Cabin))] <- "D"
levels(titanic.train$Cabin)[grepl("E", levels(titanic.train$Cabin))] <- "E"
levels(titanic.train$Cabin)[grepl("F", levels(titanic.train$Cabin))] <- "F"
levels(titanic.train$Cabin)[grepl("G", levels(titanic.train$Cabin))] <- "G"
titanic.train$Cabin=factor(titanic.train$Cabin, levels=
                             c("A","B","C","D","E","F","G","T",""),
                           labels=c(1,2,3,4,5,6,7,8,9))
titanic.train$Ticket=NULL
titanic.train$Sex=factor(titanic.train$Sex, levels=
                           c("female","male"), labels=c(1,2))
titanic.train$Embarked=factor(titanic.train$Embarked, levels=
                                c("C","Q","S"), labels=c(1,2,3))
titanic.train$Pclass=as.integer(titanic.train$Pclass)
titanic.train$Sex=as.integer(titanic.train$Sex)
titanic.train$Cabin=as.integer(titanic.train$Cabin)
titanic.train$Embarked=as.integer(titanic.train$Embarked)

set.seed(123)

d_mtry = seq(from=2, 6, by = 1)
d_ntree = seq(from = 100, to = 600, by = 100)
parameters = expand.grid(mtry = d_mtry, ntree = d_ntree)

y = parameters[1,]
folds = createFolds(titanic.train$Survived, k = 10)


cv_hyper = apply(parameters, MARGIN = 1, function(y){
  cv = lapply(folds, function(x){
    training_set  = titanic.train[-x,]
    test_set = titanic.train[x,]
    classifier = randomForest(formula = Survived~.,
                              data = training_set,
                              mtry = y[1],
                              ntree = y[2])
    pred=predict(classifier, test_set)
    conf_matrix = table(test_set$Survived,pred,dnn=c("Actual value","Classifier prediction"))
    conf_matrix_prop = prop.table(conf_matrix)
    accuracy = sum(diag(conf_matrix))/sum(conf_matrix)
    precision = conf_matrix[1,1]/sum(conf_matrix[,1])
    specificity = conf_matrix[2,2]/sum(conf_matrix[,2])
    return(c(accuracy, precision, specificity))
  })
  
  cv = t(matrix(unlist(cv), nrow=3))
  accuracies = apply(cv, MARGIN = 2, FUN = "mean")
  return(accuracies)
})
plot(cv_hyper[1,])
posbest = which.max(cv_hyper[1,])
parameters[posbest,]
cv_hyper[1,posbest]

grid = expand.grid(mtry = seq(2, 6, 1),
                   ntree = seq(100, 600, 100))

ggplot(grid)+aes(x = mtry, y = cv_hyper[1,], color = as.factor(ntree))+
  geom_point(size = 0.5) + geom_line(size = 0.5) +
  theme(text = element_text(size = 12))


bestclassifier = randomForest(formula = Survived~., 
                              data = titanic.train, mtry = parameters[posbest, 1], 
                              ntree = parameters[posbest, 2])

varImpPlot(bestclassifier)

mymodel = function(test_set){
  
  levels(test_set$Cabin)[grepl("A", levels(test_set$Cabin))] <- "A"
  levels(test_set$Cabin)[grepl("B", levels(test_set$Cabin))] <- "B"
  levels(test_set$Cabin)[grepl("C", levels(test_set$Cabin))] <- "C"
  levels(test_set$Cabin)[grepl("D", levels(test_set$Cabin))] <- "D"
  levels(test_set$Cabin)[grepl("E", levels(test_set$Cabin))] <- "E"
  levels(test_set$Cabin)[grepl("F", levels(test_set$Cabin))] <- "F"
  levels(test_set$Cabin)[grepl("G", levels(test_set$Cabin))] <- "G"
  test_set$Cabin=factor(test_set$Cabin, levels=
                          c("","A","B","C","D","E","F","G","T"),
                        labels=c(0,1,2,3,4,5,6,7,8))
  
  test_set$Sex=factor(test_set$Sex, levels=
                        c("female","male"), labels=c(1,2))
  test_set$Embarked=factor(test_set$Embarked, levels=
                             c("C","Q","S"), labels=c(1,2,3))
  
  test_set$Ticket=NULL
  
  test_set$Pclass=as.integer(test_set$Pclass)
  test_set$Sex=as.integer(test_set$Sex)
  test_set$Cabin=as.integer(test_set$Cabin)
  test_set$Embarked=as.integer(test_set$Embarked)
  
  
  pred = predict(bestclassifier, test_set, type = "class")
  conf_matrix = table(test_set$Survived, pred)
  accuracy = sum(diag(conf_matrix))/sum(conf_matrix)
  precision = conf_matrix[1,1]/sum(conf_matrix[,1])
  specificity = conf_matrix[2,2]/sum(conf_matrix[,2])
  return(list(prediction = pred, conf_matrix = conf_matrix, accuracy, 
              precision, specificity))
}

save(bestclassifier, mymodel, file = "BestModel.RData")

#Graphics


# New graphics learning from the errors of the past assignment.
cabin = substr(titanic.train$Cabin, 1,1)
ggplot(titanic.train)+aes(cabin, y = Fare, fill = Survived)+
  geom_boxplot() 

ggplot(titanic.train) + aes(x = Age, y = Pclass, color = Survived, cex.lab=1, cex.axis = 2)+
  geom_jitter() + theme(text = element_text(size = 16))

ggplot(titanic.train) + aes(x = Fare,color = Survived)+
  geom_density()+facet_wrap(~Pclass, ncol = 1, scales = "free_y") + theme(text = element_text(size = 16))

ggplot(titanic.train) + aes(x = Fare,color = Survived)+
  geom_density()+facet_wrap(~Embarked, ncol = 1, scales = "free_y") + theme(text = element_text(size = 16))

ggplot(titanic.train) + aes(x = SibSp, y = Parch, color = Survived, cex.lab=1, cex.axis = 2)+
  geom_jitter() + theme(text = element_text(size = 16))


# Making a tree from the titanic train data to draw conclusions from it.

# Run the classification algorithm implemented in package rpart with default hyperparameters values

# Embarked was changed the name in order to avoid over lapping.
names(titanic.train)[9]="E."
# The tree is made
mytree=rpart(formula=Survived~., data=titanic.train, method="class")
# The library rattle is used in order to give it batter aesthetics.
fancyRpartPlot(mytree,tweak=1.2)
# The name Embarked was restored
names(titanic.train)[9]="Embarked"

