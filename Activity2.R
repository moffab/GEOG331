
#Activity 2
#imports data included in noaa_weather file and assigns it to datW
datW <- read.csv("/Users/benjaminmoffa/Documents/Github/noaa_weather/2011124.csv")
#Provides more info on dataframe
str(datW)

#Set up dates...creating new date variables dateF and year
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")
datW$year <- as.numeric(format(datW$dateF,"%Y"))

#Question 2 answer note: below are an example of a vector with character, numeric, factor,
#and integer data

#character vector
cvector<-c("a","b","c","d","e")
cvector

#Integer vector (L next to the integer specifies the number as an integer, R default is 
#numeric assignment)
ivector<-c(1L,2L,3L,4L,5L)
ivector

#Numeric vector
nvector<-c(1.1, 2.2, 3.3, 4.4, 5.5)

#Factor vector, factor() specifies factor vector
fvector<-factor(c("low", "low-medium", "medium", "medium-high", "high"))



#looks at each unique site name
unique(datW$NAME)

#get average daily maximum temp for aberdeen, wa
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

#create average temp for each observation
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)

#create average temp variable for each location
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averageTemp

#change column names from Group.1 and x to Name and MAAT
colnames(averageTemp) <- c("NAME","MAAT")
averageTemp

#codes each site as a value from 1-5
datW$siteN <- as.numeric(as.factor(datW$NAME))

#displays the following four histogram outputs in 1 window
par(mfrow=c(2,2))

#create histogram of average daily temps for site coded as 1 (aberdeen, wa)
h1<-hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = "Aberdeen, WA US", 
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

#add line for mean
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)

#add line for mean - 1 standard deviation
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#add line for mean + 1 standard deviation
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#From Instructions
#Add normal distribution line for histogram 1
x.plot <- seq(-10,30, length.out = 100)

y.plot <-  dnorm(seq(-10,30, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
y.scaled <- (max(h1$density)/max(y.plot)) * y.plot

points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)

#Histogram of Average Daily temp of site coded as 2
hist(datW$TAVE[datW$siteN == 2],
     freq=FALSE, 
     main = "LIVERMORE, CA US",
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="burlywood3",
     border="white")

abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)

#add line for mean - 1 standard deviation
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#add line for mean + 1 standard deviation
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)


#Histogram for Average Daily temp of site coded as 3
hist(datW$TAVE[datW$siteN == 3],
     freq=FALSE, 
     main = "MANDAN EXPERIMENT STATION, ND US",
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="cornflowerblue",
     border="white")

#add line for mean of site 3
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)

#add line for mean - 1 standard deviation
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#add line for mean + 1 standard deviation
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#Histogram for Average Daily temp of site coded as 4
hist(datW$TAVE[datW$siteN == 4],
     freq=FALSE, 
     main = "MORMON FLAT, AZ US",
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="cornflowerblue",
     border="white")
#add line for mean of site 4
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)

#add line for mean - 1 standard deviation
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#add line for mean + 1 standard deviation
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#Gives Probability of observing temp below 5 degrees C (probability of
#observations below 5 on the distribution curve)(Site 1)
pnorm(5,
        mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
        sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))- pnorm(0,
        mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
        sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#Gives probability of observing temperatures above 20 degrees C
1 - pnorm(20,
          mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#Get probability of observing extremely higher temperatures at site 1 
qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))


#this returns the value associated with with observations in the top 5% and assigns
# it to threshold, i.e. temperatures values in the top 5%
threshold<-qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
threshold

#this calculates the probability of of observing temperatures above the current threshold
#for high temperatures if the average temperature increases by 4 degrees C
1 - pnorm(threshold,
          mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE)+4,
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))


#Creates histogram of average daily precipitation for Aberdeen, WA US
hist(datW$PRCP[datW$siteN == 1],
         freq=FALSE, 
         main = "Aberdeen, WA US", 
         xlab = "Average Daily Precipitation (mm)", 
         ylab="Relative frequency",
         col="grey50",
         border="white")

#Question 8:
#Annual precipitation for each site by year
PRCP_SITE_YEAR <- aggregate(datW$PRCP, by=list(datW$year,datW$siteN), FUN="sum",na.rm=TRUE)
colnames(PRCP_SITE_YEAR) <- c("DATE","LOCATION", "PRCP")
PRCP_SITE_YEAR

#creates subset of data from PRCP_SITE_YEAR pertaining to site 1 (Aberdeen, WA US)
prcp1 <- subset(PRCP_SITE_YEAR, PRCP_SITE_YEAR$LOCATION==1)
prcp1

#Creates histogram of average annual precipitation by site 1 (Aberdeen, WA US)
hist(prcp1[,3],
     freq=FALSE, 
     main = "Aberdeen, WA US", 
     xlab = "Average Yearly Precipitation (mm)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

#Question 9
#Average Precipitation for all sites 
averageTemp

#creates summary statistics for average annual precipitation of each site from
#1930-2019
averagePRCP <- aggregate(PRCP_SITE_YEAR$PRCP, by=list(PRCP_SITE_YEAR$LOCATION), FUN="mean",na.rm=TRUE)
colnames(averagePRCP) <- c("NAME","AVE PRCP")
averagePRCP


