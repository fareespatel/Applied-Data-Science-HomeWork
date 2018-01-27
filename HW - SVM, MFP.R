#FareesPatel
#IST687 - HW: Support Vector Machines

install.packages("kernlab")
library(kernlab)
install.packages("Metrics")
library(Metrics)
install.packages("ggplot2")
library(ggplot2)
install.packages("e1071")
library(e1071)
install.packages("gridExtra")
library(gridExtra)


#Step-1
#Loading rhe data and eliminating all the fields having NA's
aq<- airquality

aq<- aq[complete.cases(aq),]

#Step-2
#Creating training and testing data for producing a support vector machine model
index<- sample(1:dim(aq)[1])
cutPoint2_3<- floor(2*dim(aq)[1]/3)

trainData<- aq[index[1:cutPoint2_3], ]

testData<- aq[index[(cutPoint2_3+1) : dim(aq)[1]], ]

#Step-3(1)
#Building  a kvsm model
model.ksvm	<- ksvm(Ozone	~.,	data=trainData,	kernel="rbfdot",kpar="automatic",C=5,cross=3,	prob.model=TRUE)
model.ksvm

#Step-3(2)
#Evaluating the model on the testing dataset
modelprediction<- predict(model.ksvm, testData)
#Calculating the root mean squared error
modelpredictionRMSE<- rmse(testData$Ozone,modelprediction)

#Step-3(3)
#Plotting the results using the scatterplot
a<- ggplot(testData, aes(x=Temp, y=Wind))
a<- a+ geom_point(aes(size=testData$Ozone-modelprediction,color=testData$Ozone-modelprediction))
a


#Step-3(4)
#Generating the svm model
model.svm <- svm(as.factor(Ozone)~., data = trainData,type="C-classification", kernel="radial", cost = 50, gamma = 1)
model.svm
#Plotting the svm model
svmplot<-plot(model.svm,data=testData, Ozone~Temp)

#Generating the linear model
lmModel<- lm(formula = Ozone~Temp,data=trainData)
#Plotting the linear model
lmplot<- abline(lmModel)
#Showing all the plots in one window with three plots in the same row
grid.arrange(a, svmplot,lmplot, ncol=3, nrow=1)

#Step-4
#Creating a goodOzone variable where it's value is 0 if the ozone value is less than the mean value else the value is 1
m<- data.frame(aq$Ozone)
h<-apply(m, 1, function(x) ifelse(x > mean(aq$Ozone), 1, 0))

h<- as.integer(h)
aq$goodOzone<- h

#Building new dataset for training and testing data
trainData1<- aq[index[1:cutPoint2_3], ]

testData1<- aq[index[(cutPoint2_3+1) : dim(aq)[1]], ]

#Step-5(1)
# Generating kvsm model for goodOzone variable
model.kvsm2<- ksvm(goodOzone ~.,	data=trainData1,	kernel="rbfdot",kpar="automatic",C=5,cross=3,	prob.model=TRUE)

model.kvsm2

#Step-5(2)
#Evaluating the model on the testing dataset
modelprediction2<- predict(model.kvsm2, testData1, type="votes")

#Calculating the percent of 'goodOzone' that was correctly predicted.
m1<- modelprediction2

h1<-apply(m1, 1, function(x) ifelse(x > mean(modelprediction2), 1, 0))
modelprediction21<- as.integer(h1)
modelprediction21<- data.frame(modelprediction21)

ScatplotSize<- modelprediction21
results<- table(testData1$goodOzone,modelprediction21$modelprediction21)

percentCorrect<- (results[1,1]+results[2,2])/(results[1,1]+results[1,2]+results[2,1]+results[2,2]) *100

#Step-5(3)
# Defining certain variables to representing the actual value of 'goodOzone' (i.e. if the actual ozone level  was good)
#and defining size variable to evaluate if the prediction was correct (larger symbols should be the observations the model got wrong)
ValuesForTrue<-apply(ScatplotSize, 1, function(x) ifelse(x == 1, modelprediction2[x, ], 0))

ValuesForTrue<- data.frame(ValuesForTrue)  
datast<- testData
datast$modelprediction21<- modelprediction21
datast$ValuesForTrue<- ValuesForTrue

s<-as.integer(unlist(datast$modelprediction21))
g<- data.frame(testData1$goodOzone, s)
g$diff<- 1

g$diff[g$testData1.goodOzone==g$s]<- 0


#Plotting scatter plot
scatplot<- ggplot(datast, aes(x=Temp, y=Wind)) #defining the plot
scatplot<-scatplot+ aes(shape=factor(g$diff))
scatplot<- scatplot+ geom_point(aes(size=factor(unlist(modelprediction21)),color=ValuesForTrue))
scatplot


#Step-5(4)
#Preparing data to generate the svm model
datast$modelprediction21<- as.numeric(unlist(datast$modelprediction21))
datast$ValuesForTrue<- as.numeric(unlist(datast$ValuesForTrue))

#Generating the svm model
svm.model2 <- svm(goodOzone~., data = trainData1,type="C-classification", kernel="radial", cost = 5, gamma = 1)
svm.model2

#plotting the svm model
svmplot2<-plot(svm.model2,data=trainData1, Ozone~Temp)

#Calculating the Bayes model
nb.model <- NaiveBayes(as.factor(goodOzone) ~ ., data = trainData1) 
nb.model

#plotting the Bayes model
nbplot<-plot(nb.model)

#Step-5(5)
#plotting all three results (charts) in one window having two charts in one row.
grid.arrange(scatplot, svmplot2,nbplot, ncol=2, nrow=2)


#Step-6
#Result and comparision:I think the linear model and Naive Bayes models are giving approximately the same results with some error.
#Whereas svm and ksvm are better models because the plots (visualizations) shows that most of the values predicted are correct.