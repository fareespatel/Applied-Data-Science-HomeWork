#Farees Patel
#HomeWork 6

install.packages('ggplot2') #package installation
library('ggplot2') #including package
#Step-1

AQ_dataset<- airquality #loading the dataset into a new variable

#Step-2

AQ<- na.omit(AQ_dataset) #removing NA's from the dataset

#Step-3 (1)

#1
x <- ggplot(AQ, aes(x=Ozone)) #defining plot and aesthetics
x<- x+geom_histogram(bins =5, colour='black', fill='white') # defining the shape and structure of the histogram
x # displaying the plot

x <- ggplot(AQ, aes(x=Solar.R)) #defining plot and aesthetics
x<- x+geom_histogram(bins =5, colour='black', fill='white') # defining the shape and structure of the histogram
x # displaying the plot

x <- ggplot(AQ, aes(x=Wind)) #defining plot and aesthetics
x<- x+geom_histogram(bins =5, colour='black', fill='white') # defining the shape and structure of the histogram
x # displaying the plot

x <- ggplot(AQ, aes(x=Temp)) #defining plot and aesthetics
x<- x+geom_histogram(bins =5, colour='black', fill='white') # defining the shape and structure of the histogram
x # displaying the plot

x <- ggplot(AQ, aes(x=Day)) #defining plot and aesthetics
x<- x+geom_histogram(bins =5, colour='black', fill='white') # defining the shape and structure of the histogram
x # displaying the plot

AQ$Wind<-round(AQ$Wind) #Rounding values for the Wind attribute


#2
o<- ggplot(AQ, aes(factor(Month),Ozone)) # defining the group by varible and the variable to be plotted
o<-o+geom_boxplot() #Adding the the boxplot function
o #displaying the box plot

w<- ggplot(AQ, aes(x=factor(Month),Wind, fill=factor(Month))) # defining the group by varible and the variable to be plotted
w<-w+geom_boxplot() #Adding the the boxplot function
w #displaying the box plot

boxplot(AQ$Ozone)


#Step-3(2)

AQ$date<-ISOdate(year=1973, month=AQ$Month, day=AQ$Day) #Creating a date and time using existing coloumn
AQ$date<-gsub("\\ 12:00:00","",AQ$date) #removing the time factor from the attribute
AQ$date<-as.Date(AQ$date) #converting the date format from 'character' to 'date' format

ol<-ggplot(AQ,aes(x=date, y=Ozone)) #defining plot and aesthetics
ol<- ol+geom_line() #Adding the the line function
ol #displaying the line chart

tl<-ggplot(AQ,aes(x=date, y=Temp)) #defining plot and aesthetics
tl<- tl+geom_line()  #Adding the the line function
tl #displaying the line chart

wl<-ggplot(AQ,aes(x=date, y=Wind)) #defining plot and aesthetics
wl<- wl+geom_line()  #Adding the the line function
wl #displaying the line chart

sl<-ggplot(AQ,aes(x=date, y=Solar.R)) #defining plot and aesthetics
sl<- sl+geom_line() #Adding the the line function
sl #displaying the line chart


t<- ggplot(AQ, aes(x=date)) #defining plot and aesthetics
t<- t+geom_line(aes(y=Ozone,colour='Ozone')) #Adding a line function for Ozone attribute
t<- t +geom_line(aes(y=Temp, colour = 'Temp')) #Adding a line function for Temp attribute
t<- t+geom_line(aes(y=Wind, colour = 'Wind')) #Adding a line function for Wind attribute
t<- t+ geom_line(aes(y=Solar.R, colour = 'Solar.R')) #Adding a line function for Solar.R attribute
t<- t+ scale_colour_manual("", values = c("Ozone"="Red", "Temp"="Green","Wind"="blue", "Solar.R"="Orange")) # Setting different colour for attributes
t<- t+ ylab('Values') # defining the y axis
t # displaying the multiline chart

#Step-4
g<- ggplot(AQ, aes(x=Day, fill=factor(Day))) #defining the plot
g<-g+ geom_tile(aes(y=Ozone, colour='Ozone'))#defining the heatmap
g<-g+ geom_tile(aes(y=Temp, colour='Temp')) #defining the heatmap
g<-g+ geom_tile(aes(y=Wind, colour='Wind')) #defining the heatmap
g<-g+ geom_tile(aes(y=Solar.R, colour='Solar.R')) #defining the heatmap
g<- g+ scale_colour_manual("", values = c("Ozone"="Red", "Temp"="Green","Wind"="blue", "Solar.R"="Orange")) # Setting different colour for attributes
g<- g+ ylab('Values of Variable') # defining the y axis
g #displaying the heatmap


# Step-5
a<- ggplot(AQ, aes(x=Wind, y=Temp)) #defining the plot
a<- a+ geom_point(aes(size=Ozone,color=Solar.R)) #Adding the scatter chart plot
a # displaying the plot

#Step-6

#1
# I could witness one pattern after exploring the airquality dataset
#If we view the grapgh of Ozone, Temp, Wind and Solar.R on a single graph, we can infer that from May to October, the values of Solar.R and Ozone have large deviations relative to Temp and Wind.

#2
#I think the multi-line chart was the most informative visualization because it helped in understanding various attribute's value relatively to other value of attributes.

#End of Homework-6