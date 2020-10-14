#FROM INSTRUCTIONS
#load in lubridate
library(lubridate)
library(dplyr)
library(ggplot2)

#read in streamflow data
datH <- read.csv("/Users/benjaminmoffa/Documents/Github/stream_flow_data.csv",
                 na.strings = c("Eqp"))
head(datH)        

#read in precipitation data
#hourly precipitation is in mm
datP <- read.csv("/Users/benjaminmoffa/Documents/Github/2049867.csv")                            
head(datP)

#only use most reliable measurements
datD <- datH[datH$discharge.flag == "A",]

#### define time for streamflow #####
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)

#### define time for precipitation #####    
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)

#### get decimal formats #####
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + (datD$decDay/366),
                       datD$year + (datD$decDay/365))
#calculate times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/366),
                       datP$year + (datP$decDay/365))              

#plot discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

#basic formatting
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")

#start new plot
dev.new(width=8,height=8)

#indicate margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2)

#END OF CODE EXPLICITLY FROM INSTRUCTIONS

#QUESTION 5 & 6:
#create 2017 subset
sub1<-subset(datD, datD$year==2017)
ave2017 <- aggregate(sub1$discharge, by=list(sub1$doy), FUN="mean")
colnames(ave2017) <- c("doy","dailyAve")

#plot daily ave of 2017
plot(ave2017$doy, ave2017$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2)

#bigger margins
par(mai=c(1,1,1,1))
#make plot of daily ave for each day 
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Month", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,100),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
#add line for 2017
lines(ave2017$doy, ave2017$dailyAve,
       col= "tomato3", pch=15)
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)      
axis(1, at=seq(0,360, by=30), #tick intervals
     lab=seq(0,12, by=1)) #tick labels
axis(2, seq(0,100, by=20),
     seq(0,100, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright",inset=.02, c("mean","1 standard deviation", "2017"), #legend items
       lwd=c(3,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2), col="tomato3"),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border

#QUESTION 7
#create dataframe with only doy and year and count number of unique occurences
newdataframe<-datP %>%
group_by(datP$doy, datP$year) %>%
summarise(Count = n())
sub2<-subset(newdataframe, newdataframe$Count==24)
colnames(sub2) <- c("doy","year", "count")

#do this as bootleg way to put points on graph where complete precipitation is
sub2$yval<- -4

#create new time variable to match datD$decYear
sub2$newdecYear <- ifelse(leap_year(sub2$year),sub2$year + (sub2$doy/366),
                       sub2$year + (sub2$doy/365))      
#plot discharge and add a vertical line at every point with complete precipitation measurements
plot(datD$decYear,
     datD$discharge, 
     type="l", xlab="Year",
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
points(x=sub2$newdecYear, y =sub2$yval, type = "p",pch=15, col="red")
legend("topright",inset=.02, c("Discharge","Days w/ 24hr precip"), #legend items
       lwd=c(3,NA),#lines
       col=c("black", col="tomato3"),#colors
       pch=c(NA,15),#symbols
       bty="n",
       cex=.6)#no legend border



#QUESTION 8 ANSWER
#create mini dataframe for jan 2 to jan 4 in 2010 
hydroD2 <- datD[datD$doy >= 2 & datD$doy < 4 & datD$year == 2010,]
hydroP2 <- datP[datP$doy >= 2 & datP$doy < 4 & datP$year == 2010,]
yl <- floor(min(hydroD2$discharge))-1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD2$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP2$HPCP))+.5
#scale precipitation to fit on the 
hydroP2$pscale <- (((yh-yl)/(pm-pl)) * hydroP2$HPCP) + yl

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD2$decDay,
     hydroD2$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP2)){
        polygon(c(hydroP2$decDay[i]-0.017,hydroP2$decDay[i]-0.017,
                  hydroP2$decDay[i]+0.017,hydroP2$decDay[i]+0.017),
                c(yl,hydroP2$pscale[i],hydroP2$pscale[i],yl),
                col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

#QUESTION 9
#Create Dataset of 2016 and 2017
data2016<-datD[datD$year==2016,]
data2017<-datD[datD$year==2017,]

#create seasonal variable based on if doy falls in range of season
data2017$season<-ifelse(data2017$doy>=265 & data2017$doy<=354, "fall",
                        ifelse(data2017$doy>=355 & data2017$doy<=365, "winter",
                        ifelse(data2017$doy>=1 & data2017$doy<=109, "winter",
                        ifelse(data2017$doy>=110 & data2017$doy<=171, "spring", 
                        ifelse(data2017$doy>=172 & data2017$doy<=264, "summer",NA)))))


#2016 was a leap year so different
data2016$season<-ifelse(data2016$doy>=266 & data2016$doy<=355, "fall",
                        ifelse(data2016$doy>=356 & data2016$doy<=366, "winter",
                        ifelse(data2016$doy>=1 & data2016$doy<=110, "winter",
                        ifelse(data2016$doy>=111 & data2016$doy<=171, "spring", 
                        ifelse(data2016$doy>=172 & data2016$doy<=265, "summer",NA)))))


#declare season as factor variable
data2017seasonplot<-as.factor(data2017$season)
#create violin plot for 2017
ggplot(data=data2017, aes(data2017seasonplot, discharge)) + geom_violin()

#declare season as factor variable
data2016seasonplot<-as.factor(data2016$season)
#create violin plot for 2016
ggplot(data=data2016, aes(data2016seasonplot, discharge)) + geom_violin()








