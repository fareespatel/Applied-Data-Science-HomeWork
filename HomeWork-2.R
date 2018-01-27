# Farees Patel
# Homework-2 (Data Frames and Sorting)

#Task-1

my_mtcars<- mtcars # Copy the 'mtcars' dataframe into a new dataframe 'my_mtcars'

my_mtcars$DispPerCyl<- my_mtcars$disp/my_mtcars$cyl # Calculate and add a new variable to the dataframe that represents engine displacement per cyclinder 

my_mtcars #Display the new data frame with additional variable

summary(my_mtcars$DispPerCyl) # Summarizing the new variable using the summary function

# Task-2

pets<- c(2,1,3,1,2) # A vector representing the number of pets each individual has

Order<-c(3,2,2,1,2) # A vector representing each individual's birth order in their family

Siblings<-c(2,1,3,1,1) # A vector representing the number of siblings each individual has

userID<- c(471871882819, 209470519541, 703545243830, 548550008000, 775481217495) #A vector representing the user ID of each individual

myFriends<-data.frame(userID,pets,Order,Siblings) # Binding all four vectors together into a new dataframe


str(myFriends) #To report the structure of the data frame

summary(myFriends) #To report data modes or types of the data frame

myFriends$userID # Display the user ID of each individual

myFriends$pets # Display the number of pets each individual has

myFriends$Order # each individual's birth order in their family

myFriends$Siblings #Display the number of siblings each individual have

#End of HomeWork-2