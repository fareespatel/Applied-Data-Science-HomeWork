# Farees Patel
# HomeWork-3 (Cleaning/ Munging Dataframes)

#Task

#1
WebUrl<- "http://www.census.gov/popest/data/state/totals/2011/tables/NST-EST2011-01.csv" # Load the web url into a variable

#2
readStates<-read.csv(url(WebUrl)) #Read the URL and load the dataset into a data frame

#3
str(readStates) #To check the structure of the dataset

#4
readStates<-readStates[-1:-8,] #Removing the first eight rows of irrelevant data from the dataframe

readStates<-readStates[,1:5] #Removing the last 5 blank columns from the dataframe

readStates<-readStates[-52:-58,] #Removing the last 7 irrelevant rows from the dataframe 

readStates$StateName<-readStates[,1] #creating a new variable and loading the data from the first coloumn which represents the States

readStates<-readStates[,-1] #Remove the first coloumn to avoid redundancy

readStates$StateName<-gsub("\\.","",readStates$StateName) # Removing the 'dot'from the StateName coloumn

readStates$Jul2010<-gsub(",","",readStates$X) #creating a new variable, cleaning and loading the data from the second coloumn which represents the July 2010 population

readStates$Jul2011<-gsub(",","",readStates$X.1) #creating a new variable, cleaning and loading the data from the third coloumn which represents the July 2011 population

readStates$base2010<-gsub(",","",readStates$X.2)#creating a new variable, cleaning and loading the data from the fourth coloumn which represents the base 2010 population

readStates$base2011<-gsub(",","",readStates$X.3)#creating a new variable, cleaning and loading the data from the fifth coloumn which represents the base 2011 population

readStates<-readStates[,-1:-4] #Remove the first four coloumn to avoid redundancy

#5
readStates$Jul2010<-as.numeric(gsub(" ","",readStates$Jul2010)) #Getting rid of numbers and converting to numbers

readStates$Jul2011<-as.numeric(gsub(" ","",readStates$Jul2011)) #Getting rid of numbers and converting to numbers

readStates$base2010<-as.numeric(gsub(" ","",readStates$base2010)) #Getting rid of numbers and converting to numbers

readStates$base2011<-as.numeric(gsub(" ","",readStates$base2011)) #Getting rid of numbers and converting to numbers

rownames(readStates)<-NULL #Removing the inappropriate indexing

#6
dfStates<- readStates #Storing the dataset into a new dataframe

#7
MeanJuly2011<-mean(dfStates$Jul2011) #Computing mean for July 2011 data

MeanJuly2011 #Display the mean for July 2011 data

#8
index<- which.max(dfStates$Jul2011) #Get index of the highest population in July 2011

StateMaxPop<- dfStates$StateName[index]

MaxPop<- dfStates$Jul2011[index]

#9
dfStates<-dfStates[order(dfStates$Jul2011),] #sort the data in an increasing order based on July 2011 data

#10 and #11
CumDistri<-function(myvect,number) #write a function giving two parameters: vector and a number
{
  c=0 #initialize the count
  
  for (i in 1:length(myvect)) #using fall loop to calculate cumulative distribution
 {   if( number> myvect[i]) # using if loop to calculate the no of 'number' below vector[i]
      c=c+1 
}
  
    
  
  return (c/length(myvect)) #return the cumulative distribution
}
 
#12
v <- c(1,2,3,4,5) #defining a vector to pass to the function

o <- 2 # defining a number to pass to the function

cal <- CumDistri(v,o) #calling a function

#13

Cal_Cum_Distrib<- CumDistri(dfStates$Jul2011,MeanJuly2011) #calling a function

#End of HomeWork-3
