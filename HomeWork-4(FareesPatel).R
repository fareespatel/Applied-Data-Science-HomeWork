#Farees patel
# Homework-4(Samples Homework)

#1 and #2
printVecInfo <- function(vect) #creating a function
{
  
  avg<-mean(vect) #calculating mean
  med<-median(vect) #calculating median
  minimum<-min(vect) #calculating minimum value
  maximum<-max(vect) #calculating maximum value
  stddev<-sd(vect) #calculating standard deviation value
  quant1<-quantile(vect,probs = c(0.05)) #calculating 5% quantile value
  quant2<-quantile(vect,probs = c(0.95)) #calculating 95% quantile value
  skewvalue<- skewness(vect) #calculating skewness value
  Functions<-c("Mean","Median","Min","Max", "Standard deviation","Quantile1","Quantile2"," Skewness") #defining a function vector
  Values<-c(avg,med,minimum,maximum,stddev,quant1,quant2,skewvalue) #defining a value vector
  df<-data.frame(Functions,Values) #defining a dataframe
  print(df) #printing the dataframe
}

#3
testvector<- c(1,2,3,4,5,6,7,8,9,10,50) #creating a vector for testing the function
printVecInfo(testvector) # calling the function

#4
jar<-rep(c(0,1),50) #creating a vector of 50 0s and 1s. (1=Red Marbles, 0=Blue marbles)

#5
TotalRedMarbles<- sum(jar) #calculating the total number of red marbles using sum functions
print("Total number of red marbles: ") #printing the total number of red marbles
print(TotalRedMarbles) #printing the total number of red marbles

#6
testsample<-sample(jar,size=10,replace=TRUE) #sample 10 marbles from the jar
TotalRed_testsample<- sum(testsample) #Total number of Red marbles in the sampled marbles
PercentRedMarble<- (TotalRed_testsample/length(testsample))*100 #calculating percentage of red marbles from the sampled marbles
print("Percentage of Red marbles: ") #print the percentage of red marbles
print(PercentRedMarble) #print the percentage of red marbles

#7
SampleFunction<-function(SampleTime,JarSampling,jar)
{
SampleTest<- replicate(SampleTime, mean(sample(jar,size=JarSampling,replace=TRUE),simplify=TRUE)) # replicating the sampling process for 10 times and reporting the mean for every sampling instance
print(SampleTest) #printing the vector
printVecInfo(SampleTest) #printing the summary of the vector using a function
hist(SampleTest) #generating a histogram for the specified sampleTime
}
SampleFunction(20,10,jar) #Sample the jar 10 times and finding the mean of red balls from the samples and replicating this process for 20 times

#8
SampleFunction(20,100,jar) #Sample the jar 100 times and finding the mean of red balls from the samples and replicating this process for 20 times

#9
SampleFunction(100,100,jar) #Sample the jar 100 times and finding the mean of red balls from the samples and replicating this process for 100 times

#10
AQdataset<- airquality #Loading the airquality dataset into a AQdataset variable

#11
Cleaned_AQdataset<-AQdataset[complete.cases(AQdataset),] #For every NA in the dataset the variable will store FALSE and for every NA it will store TRUE. It will remove the rows having FALSE value

#12
printVecInfo(Cleaned_AQdataset$Ozone) #calling the function for exploring the ozone
hist(Cleaned_AQdataset$Ozone) #generating a histogram on the basis of Ozone coloumn

printVecInfo(Cleaned_AQdataset$Wind) #calling the function for exploring the Wind
hist(Cleaned_AQdataset$Wind) #generating a histogram on the basis of Wind coloumn

printVecInfo(Cleaned_AQdataset$Temp) #calling the function for exploring the Temp
hist(Cleaned_AQdataset$Temp) #generating a histogram on the basis of Temp coloumn

#End of Homework-4
