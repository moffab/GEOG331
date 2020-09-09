#Matrix Practice
m<-matrix(c(7,8,9,10,11,12), ncol=2, byrow=TRUE)
m
m[1,2]
m[,1]
m[2,]
#??? does the number of elements in a matrix have to be evenly divided by number of rows?
m2<-matrix(c(7,8,9,10,11,12), ncol=2, byrow=FALSE)
m2
m2[1,2]
m2[1,]
m[,2]
#End matrix practice

#Activity 2
datW <- read.csv("/Users/benjaminmoffa/Documents/Github/noaa_weather/2011124.csv")
str(datW)
#Question 1 answer note: there are 157849 rows and 9 columns in the data set

#Set up dates...creating new date variables dateF and year
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")
datW$year <- as.numeric(format(datW$dateF,"%Y"))

#Question 2 answer note: In PDF

levels(datW$NAME)

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

#Question #3 answer note: consult help documentation for histogram function

par(mfrow=c(2,2))

#create histogram of average daily temps for site coded as 1 (aberdeen, wa)
h1<-hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = "Aberdeen, WA US", #fix
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

#add line for mean
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)

#add line for standard deviation
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#add line for standard deviation
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#Average Daily temp of site coded as 2
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

#add line for standard deviation
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#add line for standard deviation
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)


#Average Daily temp of site coded as 3
hist(datW$TAVE[datW$siteN == 3],
     freq=FALSE, 
     main = "MANDAN EXPERIMENT STATION, ND US",
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="cornflowerblue",
     border="white")
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)

#add line for standard deviation
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#add line for standard deviation
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#Average Daily temp of site coded as 4
hist(datW$TAVE[datW$siteN == 4],
     freq=FALSE, 
     main = "MORMON FLAT, AZ US",
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="cornflowerblue",
     border="white")
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)

#add line for standard deviation
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#add line for standard deviation
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
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

#Gives Probability of Area Below Curve (Site 1)
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


#Questions 6-9

#Question 6
threshold<-qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

1 - pnorm(threshold,
          mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE)+4,
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#Question 7: This is an example of an exponential distribution, or a gamma 
#distribution in which an exponential distribution is a subset ofhe
hist(datW$PRCP[datW$siteN == 1],
         freq=FALSE, 
         main = "Aberdeen, WA US", 
         xlab = "Average daily precipitation", 
         ylab="Relative frequency",
         col="grey50",
         border="white")

#Question 8:
#Annual precipitation for each site by year
PRCP_SITE_YEAR <- aggregate(datW$PRCP, by=list(datW$year,datW$siteN), FUN="sum",na.rm=TRUE)
colnames(PRCP_SITE_YEAR) <- c("DATE","LOCATION", "PRCP Annual")
PRCP_SITE_YEAR




hist(PRCP_SITE_YEAR[datW$PRCP],
     freq=FALSE, 
     main = "Aberdeen, WA US", 
     xlab = "Average daily precipitation (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

#Question 9
#Average Precipitation for all sites comparing to average temp
averageTemp
averagePRCP <- aggregate(datW$PRCP, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
colnames(averagePRCP) <- c("NAME","AVE PRCP")
averagePRCP


