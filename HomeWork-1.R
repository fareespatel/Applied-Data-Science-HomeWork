# Farees Patel
#HomeWork-1 (Intro_Lab)

Height<-c(59,60,61,58,67,72,70) #define a vector which represests height of 7 people
Weight<-c(150,140,180,220,160,140,130) #define a vector which represests weight of 7 people
a<-150 #define a variable

# Task #
#1
MeanHeight<- mean(Height) # Computing average height using mean function

#2
MeanWeight<- mean(Weight) # Computing average weight using mean function

#3
LenHeight<- length(Height) # Computing length of the Height vector
LenWeight<- length(Weight) # Computing length of the Weight vector

#4
SumHeights<- sum(Height) # Computing sum of all the elements in the height vector
SumWeights<- sum(Weight) # Computing sum of all the elements in the weight vector

#5
AvgHeight<- SumHeights/LenHeight # Computing average height using mean defination
AvgWeight<- SumWeights/LenWeight # Computing average weight using mean defination

#6 
maxH<-max(Height) # Computing maximum height from the height vector

#7
minW<-min(Weight) # Computing minimum weight from the weight vector

#8
newWeight<- Weight+5 #Adding 5 to every element in the weight vector

#9
PoundsPerInch<- newWeight/Height # Computing pound per inch for every person

#10
if(maxH>60) "Yes" else "No" # Computing if maximum height is greater than 60

#11
if(minW<a) "Yes" else "No" # Computing if minimum weight is greater than variable 'a'

#End of HomeWork-1
