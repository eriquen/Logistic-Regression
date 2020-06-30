#import library
library(class)
library(ggplot2)
library(caret)
library(caTools)
#set the environment location
setwd("SET WORK DIRECTORY")


#read .csv file
vertebralData <- read.csv("column_2C.csv", header = TRUE)


#shuffle row of dataset
set.seed(5555) 
rows <- sample(nrow(vertebralData))
vertebralData <- vertebralData[rows, ]


#split data into training and testing
set.seed(3033)
intrain <- createDataPartition(y = vertebralData$class, p= 0.7, list = FALSE)
training <- vertebralData[intrain,]
testing <- vertebralData[-intrain,]


dim(training); dim(testing)


#check missing data by using anyNA. just supply dataset to the function
#the function will return TRUE if there missind data
anyNA(vertebralData)



############################# rcaret ####################################################

#Training and train control
#method specifies how resampling will be done  
#number specifies the number of times resampling should be done for methods 
#repeats specifies the number of times to repeat resampling for methods such as repeatedcv
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)


#model train, the function take class and the relation to all
#variable, training data, N-Fold, method and family
#family binomial is refering to logistic regression
model = train(form = class ~ ., data = training, 
               trControl = trctrl, method = "glm",
               family = "binomial")



#make prediction
#function predict() was used to do a predictiom
#the function need the trained model and testing data
lg_predict = predict(model, newdata = testing)
lg_predict


confusionMatrix(lg_predict, testing$class)

#res <- evalm(list(model),gnames=c('glm'))
#find roc
myControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)

model<-train(method="glm",data=training,class~.,trControl=myControl)


print(model)

############################# rcaret ####################################################



