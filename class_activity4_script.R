#use built in iris dataset
#take a look at it 
head(iris)
#load in some tidyverse packages
library(dplyr)
library(ggplot2)

#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length

#create subset of data with only versicolor species
versicolor<-iris[iris$Species=="versicolor",]

#create vectors for x and y variables to be iterated over in for loop
variablesy<-c(versicolor[1], versicolor[3], versicolor[1])
variablesx<-c(versicolor[2], versicolor[2], versicolor[4])
#create list
l<-list()
#go through for loop and perform 3 regressions in accordance with the variables
#specified in the variablesy and variablesx vectors
for( n in 1:3){
  l[[n]]<-lm(formula = variablesy[[n]] ~ variablesx[[n]])
}
l
#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
					MaxHeight.cm = c(60,100,11.8))

#assign max height to each observation by species 
newframe<-left_join(iris, height)
newframe
#####################################
##### Part 3: plots in ggplot2  #####
#####################################
#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot
ggplot(iris, aes(y=Sepal.Width, x=Sepal.Length)) + geom_point(size=2, shape=21)

#3b. make a scatter plot with ggplot and get rid of  busy grid lines

ggplot(iris, aes(y=Sepal.Width, x=Sepal.Length)) + geom_point(size=2, shape=21)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#3c.make a scatter plot with ggplot and get rid of grid lines,
#show species by color, and increase the point size
ggplot(iris, aes(y=Sepal.Width, x=Sepal.Length, color=Species)) + geom_point(size=3, shape=21)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+geom_point()


#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################		

#In handed in document on moodle