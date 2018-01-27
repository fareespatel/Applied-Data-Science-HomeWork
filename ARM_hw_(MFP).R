#HomeWork-8
#Association Rules mining
#Farees Patel

install.packages('arules') #installing arules package
library(arules) #Loading the arules package

install.packages('arulesViz') #installing arulesViz package
library(arulesViz) #loading the package

install.packages(Matrix) #installing Matrix package
library(Matrix)#loading the package

install.packages("distr") #installing distr package
library(distr)#loading the package

load("C:/Users/Farees Patel/Downloads/titanic.raw(1).rdata") #load dataset

t <- titanic.raw # creating a copy of dataset to perform analysis

#Step-1(1)
t$Survived<- gsub("\\s+","", t$Survived) # Eliminate empty spaces
Survived<- t[(t$Survived=="Yes"), ] #Get subset of titanic dataset depending on the people who survived
PercentPeopleSurvived<- (nrow(Survived)/ nrow(t)) *100 # Calculating the percentage of people survived

#Step-1(2)
t$Age<- gsub("\\s+","", t$Age) # Eliminate empty spaces
Child<- t[(t$Age=="Child"), ] #Get subset of titanic dataset of people who are children
PercentChild<- (nrow(Child)/ nrow(t)) *100 # Calculating the percentage of people who were children

#Step-1(3)
t$Sex<- gsub("\\s+","", t$Sex) # Eliminate empty spaces
Females<- t[(t$Sex=="Female"), ] #Get subset of titanic dataset of people who are females
PercentFemales<- (nrow(Females)/ nrow(t)) *100 # Calculating the percentage of people who were females

#Step-1(4)
t$Class<- gsub("\\s+","", t$Class) # Eliminate empty spaces
FirstClass<- t[(t$Class=="1st"), ] #Get subset of titanic dataset of 1st class people
PercentFirstClass<- (nrow(FirstClass)/ nrow(t)) *100 # Calculating the percentage of people who were from the first class

#Step-2(1)

ChildrenSurvived<- Survived[(Survived$Age=="Child"), ]
PercentChildrenSurvived<- (nrow(ChildrenSurvived)/ nrow(Survived)) *100 # Calculating the percentage of children who survived

#Step-2(2)

FemaleSurvived<- Survived[(Survived$Sex=="Female"), ]
PercentFemaleSurvived<- (nrow(FemaleSurvived)/ nrow(Survived)) *100 # Calculating the percentage of females who survived

#Step-2(3)

FirstClassSurvived<- FirstClass[(FirstClass$Survived=="Yes"), ]
PercentFirstClassSurvived<- (nrow(FirstClassSurvived)/ nrow(Survived)) *100 # Calculating the percentage of people who were from the first class and who survived

#Step-2(4)

ThirdClass<- t[(t$Class=="3rd"), ]
ThirdClassSurvived<- ThirdClass[(ThirdClass$Survived=='Yes'), ]
PercentThirdClassSurvived<- (nrow(ThirdClassSurvived)/ nrow(ThirdClass)) *100 # Calculating the percentage of people who were from the first class

#Step-3(1)

#Writing function to return dataframe of specified Age, sex, class and survive
SpecifiedDataframe<- function(Sex, Age, Class, Survive)
{
  df<- t[(t$Sex==Sex & t$Age==Age & t$Class==Class & t$Survived==Survive), ]
  return (df)
}

#Step-3(2)

#Return percentage of people who lives, who dies for a specified (parameters) of age, class and sex
getdataframe<- function(Sex, Age, Class)
{
  SurviveYes<- SpecifiedDataframe(Sex, Age, Class,'Yes')
  SurviveNo<- SpecifiedDataframe(Sex, Age, Class,'No')
  percal<- nrow(SurviveNo)/ (nrow(SurviveYes) + nrow(SurviveNo)) * 100
  return(percal)
}

#Step-3(3)

#Use the function to compare age & 3rd class male survival rates
getdataframe("Male","Adult","3rd")
getdataframe("Male","Child","3rd")
#Results: Comparatively more Adults died than children in the category of 3rd class Males

#Step-3(4)

#More Adults died than children in the category of 3rd class and Male
getdataframe("Female","Adult","1st")
getdataframe("Female","Child","1st")
#Results: Comparatively more Adults died than children in the category of 1st class Females



#Step-4(1)

ruleset<- apriori(titanic.raw, parameter=list(support=0.05, confidence= 0.5)) # Use algorithm to analyze the transaction dataset and generate rules

summary(ruleset) # provides an overview of the content in the ruleset

q<-inspect(ruleset) # displaying the rules in the ruleset along with values of support, confidence and lift

q<- q[order(-q$lift), ] #sort in descending order based on the lift values to find the best rule

q # display the rules

#Step-4(2)

plot(ruleset) # Plotting the scatterplot of the ruleset depending upon support, confidence and lift values.

#Step-4(3)

head(q,1) #Display the best rule
#I found that Female Adults from the first class survived during the titanic fall

q[7, ]
#I found there are high chances of first class people surving the titanic fall

q[14,]
#I found there are high chances of male crew member not surviving the titanic fall

#Step-4(4)

# Predictive analysis is always better as we can see we had to write long block of codes for descriptive analysis but using 'arules' we can easily predict patterns with quantifiable results in two lines of code
#I got the similar analysis with both the descriptive analysis and predictive analysis usinf 'arules'.