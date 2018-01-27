#Farees Patel

#HomeWork-5 (JSON & tapply Lab: Accident Analysis)

#Step-1(Load the data)

install.packages("sqldf") #installing and activating package
library("sqldf")
install.packages("RCurl") #installing and activating package
library("RCurl")
install.packages("jsonlite") #installing and activating package
library("jsonlite")
datasetURL<- "https://data.maryland.gov/api/views/pdvh-tf2u/rows.json"  # store the url address into a variable
data<- getURL(datasetURL) #loading the data from the web URL
result<- fromJSON(data) #transforming the data into an object using JSON function

#Step-2 (Clean the data)
sample<-result[[2]]  #Loading the actual data in a variable
sample<-sample[,-(1:8)] #removing the unwanted data
df<-as.data.frame(sample) #converting a list into a dataframe
colnames(df)<- c("CASE_NUMBER","BARRACK","ACC_DATE","ACC_TIME","ACC_TIME_CODE","DAY_OF_WEEK","ROAD","INTERSECT_ROAD","DIST_FROM_INTERSECT","DIST_DIRECTION","CITY_NAME","COUNTY_CODE","COUNTY_NAME","VEHICLE_COUNT","PROP_DEST","INJURY","COLLISION_WITH_1","COLLISION_WITH_2") #Setting appropriate coloumn names for all the instances in the dataset
df$DAY_OF_WEEK<-gsub("\\ ","",df$DAY_OF_WEEK) #Removing the unnecessary spaces
df$INJURY<-gsub("\\ ","",df$INJURY) #Removing the unnecessary spaces
df$ACC_DATE<-gsub("\\T00:00:00","",df$ACC_DATE) #Removing the unnecessary spaces
df$ACC_TIME<-gsub("\\ ","",df$ACC_TIME) #Removing the unnecessary spaces
df$ACC_TIME_CODE<-gsub("\\ ","",df$ACC_TIME_CODE) #Removing the unnecessary spaces

#Step-3 (Understanding data using SQL)
#1
NumOfAcc<- sqldf("select count(df.DAY_OF_WEEK) from df where df.DAY_OF_WEEK ='SUNDAY'") #Calculating the number of accidents on SUNDAY
sprintf("Number of accidents on Sunday: %i",NumOfAcc[1,1]) # displaying the number of accidents on SUNDAY

#2
NumOfInjury<- sqldf("select count(df.INJURY) from df where df.INJURY ='YES'") #Number of accidents having injuries
sprintf("Number of injuries: %i",NumOfInjury[1,1]) # Number of accidents having injuries

#3
sdf<-df #Loading the original dataframe into a new variable
dayfunction<- function (v) #creating a function to assign each day a unique number
{                      
                  
for(i in 1:length(v$DAY_OF_WEEK)) #defining the scope of the search
{
if (v$DAY_OF_WEEK[i] =='SUNDAY')
  v$new[i]<- 1                            # assign a new coloumn 'new' in the datarame and insert '1' when the day is SUNDAY
if (v$DAY_OF_WEEK[i] =='MONDAY')
  v$new[i]<- 2                            # assign a new coloumn 'new' in the datarame and insert '2' when the day is MONDAY
if (v$DAY_OF_WEEK[i] =='TUESDAY')
  v$new[i]<- 3                           # assign a new coloumn 'new' in the datarame and insert '3' when the day is TUESDAY
if (v$DAY_OF_WEEK[i] =='WEDNESDAY')
  v$new[i]<- 4                           # assign a new coloumn 'new' in the datarame and insert '4' when the day is WEDNESDAY
if (v$DAY_OF_WEEK[i] =='THURSDAY')
  v$new[i]<- 5                           # assign a new coloumn 'new' in the datarame and insert '5' when the day is THURSDAY
if (v$DAY_OF_WEEK[i] =='FRIDAY')
  v$new[i]<- 6                           # assign a new coloumn 'new' in the datarame and insert '6' when the day is FRIDAY
if (v$DAY_OF_WEEK[i] =='SATURDAY')
  v$new[i]<- 7                           # assign a new coloumn 'new' in the datarame and insert '7' when the day is SATURDAY
}
  return(v)                              #returning the updated dataframe
}
sdf<-dayfunction(sdf)   #calling the function
sdf<-sqldf("select * from sdf ORDER BY sdf.new") #ordering the dataframe on the basis of injuries by day using sql statement


#Step-4(Understanding data using tapply):

#1

df$constant<-" " #adding a new coloumn to the original dataframe

myfunction<- function (v) #creating a function to calculate the number of accidents happened on SUNDAY
{
  j=0   # initializing the index variable
  for(i in 1:length(v)) #defining the scope of the search
  {
    if (v[i] =='SUNDAY')  #Checking if the day is SUNDAY
      j=j+1 # Counting the Number of SUNDAYS
  }
  sprintf("Number of accidents on Sunday: %i",j[1]) #Displaying the number of accidents on SUNDAY
  
}
tapply(df$DAY_OF_WEEK,df$constant, myfunction) #Calculating the number of accidents using tapply function


#2
newdf<-df #loading the original dataframe in a new variable
newdf<-newdf[complete.cases(newdf$INJURY),] #removing NA's from the INJURY coloumn
countfunction<- function (v) #creating a function to calculate the number of injuries
{
  j=0 # initializing the index variable
  for(i in 1:length(v)) #defining the scope of the search
  {
    if (v[i] =='YES') #Checking for injury
      j=j+1 #counting the number of injuries
  }
  sprintf("Number of injuries: %i",j[1]) #Displaying the number of injuries
  
}
tapply(newdf$INJURY,newdf$constant, countfunction) #Calculating the number of injuries using tapply function

#3
q<-df #Loading the original dataframe into a new variable 
q<-dayfunction(q) #calling a function to assign each day a unique number
orderfunction<- function (v) #creating a function to sort the dataframe on the basis of injuries by day
{
  q<<-q[order(v),] #ordering the dataframe on the basis of injuries by day
   q<<-q[,-(19:20)] #removing the unnecessary coloumn from the dataframe
}
tapply(q$new, q$constant,orderfunction) #Using tapply function to order the dataframe on the basis of injuries by day

#End of HomeWork-5