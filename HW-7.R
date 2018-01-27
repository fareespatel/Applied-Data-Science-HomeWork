#Farees Patel
#HomeWork-7

install.packages('readxl')
library('readxl')
install.packages('zipcode')
library('zipcode')
install.packages('gdata')
library('gdata')
install.packages('ggmap')
library('ggmap')
install.packages('openintro')
library('openintro')

#Step-1
file<- read_excel('C:/Users/Farees Patel/Desktop/IST 687/HomeWork/MedianZIP-3.xlsx') #Read excel file
colnames(file) <- (c('Zipcode', 'Median', 'Mean', 'Population')) #Set coloumn names

install.packages("zipcode") #installing new package
library('zipcode') #including new package


file$Zipcode<- clean.zipcodes(file$Zipcode) #Converting the zipcode from char to zipcode

file<- merge(file,zipcode, by.x = 'Zipcode', by.y = 'zip') #merging dataset to include additional coloumns for the original dataset

newfile<- file[file$state!='AK',] #Remove the data of Alaska State
newfile<- newfile[file$state!='HI',] #Remove the data of Hawaii State
newfile<- newfile[complete.cases(newfile),] #Remove any NA's in the dataset


#Step-2(1,2)
newfile$stateName<- abbr2state(newfile$state) #Adding a coloumn for state name
Simpledf<- data.frame(newfile$Median, newfile$Population, newfile$state, newfile$stateName) # Creating a new dataframe
colnames(Simpledf) <- (c('Median', 'Population', 'StateAbbr', 'StateName')) #Defining the cloumn names


Simpledf$StateName<- tolower(Simpledf$StateName) #concerting to lower case for using in ggplot

us<- map_data("state") #load map data 

#Step-2(3)
map.medcolor<- ggplot(Simpledf, aes(map_id= StateName)) # creating a structure using ggplot
map.medcolor<- map.medcolor + geom_map(map = us, aes(fill=Median)) # defining the graph on the basis of median income
map.medcolor<- map.medcolor + expand_limits(x=us$long, y=us$lat) # Making graph visible to the user
map.medcolor<- map.medcolor+ coord_map() + ggtitle('Average Median Income') # Customizing the graph
map.medcolor #displaying the graph

#Step-2(4)
map.popcolor<- ggplot(Simpledf, aes(map_id= StateName)) # creating a structure using ggplot
map.popcolor<- map.popcolor + geom_map(map = us, aes(fill=Population)) # defining the graph on the basis of population
map.popcolor<- map.popcolor + expand_limits(x=us$long, y=us$lat) # Making graph visible to the user
map.popcolor<- map.popcolor+ coord_map() + ggtitle('Average Population') # Customizing the graph
map.popcolor #displaying the graph

#Step-3


newfile$stateName<- tolower(newfile$stateName) #convert statename to lower case
map.zip<- ggplot(newfile, aes(map_id= stateName)) # creating a structure using ggplot
map.zip<- map.zip + geom_map(map = us, aes(color="black")) # defining the graph
map.zip<- map.zip + expand_limits(x=us$long, y=us$lat) # Making graph visible to the user
map.zip.ny<- map.zip #Loading the graph in a new variable
map.zip<- map.zip+ coord_map() + ggtitle('Income per Zipcode')# Customizing the graph

map.zip<- map.zip+ geom_point(data=newfile, aes(x=longitude, y=latitude), color=newfile$Median) #plotting points on the graph
map.zip #displaying the graph

#Step-4
DummyDF<- data.frame(state.name, stringsAsFactors = FALSE) #create a dataframe
DummyDF$state<- tolower(DummyDF$state.name) #convert to lower case
map.simple<-ggplot(DummyDF, aes(map_id=state)) # creating a structure using ggplot

map.simple<- map.simple + geom_map(map=us, fill='white', color='blue') # defining the graph
map.simpleNY<- map.simple #Loading the graph in a new variable
map.simple<- map.simple + expand_limits(x=us$long, y= us$lat) # Making graph visible to the user
map.simple #displaying the graph



gg <- map.simple + stat_density2d(aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..), 
                          data = newfile,
                          geom  = 'polygon')+
  scale_fill_gradient(low = 'red', high = 'green') +
  scale_alpha(range = c(0.05,1)) +
  ggtitle('Zipcode Density') + 
  theme(plot.title = element_text(lineheight = 3.5, face = 'bold')) #plotting density

gg # show the density map

#Step-5(1)
NYdf<- newfile[newfile$state=='NY',] #creating a new dataframe
Zoom<- geocode("NY state") #Getting the geocode
ZoomAmt<- 3

xlimit<- c(Zoom$lon-ZoomAmt, Zoom$lon+ZoomAmt) #Setting new x limit
ylimit<- c(Zoom$lat-ZoomAmt, Zoom$lat+ZoomAmt) #Setting new y limit

map.ny<- map.ny + expand_limits(x=xlimit, y=ylimit) # Making graph visible to the user
map.ny<- map.ny+ coord_map() + ggtitle('Income per Zipcode') #Customozing the graph
map.ny<- map.ny+ geom_point(data=NYdf, aes(x=longitude, y=latitude), color=NYdf$Median) #plotting points on the graph
map.ny # show the map

#Step-5(2)
map.simpleNY<- map.simpleNY + expand_limits(x=xlimit, y=ylimit) # Making graph visible to the user


map.density <- map.simpleNY + stat_density2d(aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..), 
                                  data = NYdf,
                                  geom  = 'polygon')+
  scale_fill_gradient(low = 'red', high = 'green') +
  scale_alpha(range = c(0.05,1)) +
  ggtitle('Zipcode Density') + 
  theme(plot.title = element_text(lineheight = 3.5, face = 'bold')) #plotting density

map.density # show the density map

#End of HW-7