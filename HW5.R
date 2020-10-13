#FROM INSTRUCTIONS
#load in lubridate
library(lubridate)
library(dplyr)

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

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2)

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i")#remove gaps from axes  
#show standard deviation around the mean
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)


#QUESTION 5 & 6:
#create 2017 subset
sub1<-subset(datD, datD$year==2017)
ave2017 <- aggregate(sub1$discharge, by=list(sub1$doy), FUN="mean")
colnames(ave2017) <- c("doy","dailyAve")

plot(ave2017$doy, ave2017$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2)

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
lines(ave2017$doy, ave2017$dailyAve,
       col= "tomato3", pch=15)
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)      
#####COME BACK TO THIS#####
axis(1, at=seq(0,360, by=30), #tick intervals
     lab=seq(0,12, by=30)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation", "2017"), #legend items
       lwd=c(3,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2), col="tomato3"),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border

#QUESTION 7

newdataframe<-datP %>%
group_by(datP$doy, datP$year) %>%
summarise(Count = n())
sub2<-subset(newdataframe, newdataframe$Count==24)
colnames(sub2) <- c("doy","year", "count")

sub2$newdecYear <- ifelse(leap_year(sub2$year),sub2$year + (sub2$doy/366),
                       sub2$year + (sub2$doy/365))      

plot(datD$decYear,
     datD$discharge, 
     type="l", xlab="Year",
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
abline(v=sub2$newdecYear, col="red")



#QUESTIOON 8
hydroD <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2011,]
#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
        polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
                  hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
                c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
                col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

#QUESTION 8 ANSWER
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
data2016<-datD[datD$year==2016,]
data2017<-datD[datD$year==2017,]







seasons<-factor(c("Winter", "Spring", "Summer", "Fall"))

data2017$season<-seasons

season<-factor(c(data2017$doy[(data2017$doy>=355 & data2017$doy<=365) | (data2017$doy>=1 & data2017$doy<=80)]),
                          data2017$doy[data2017$doy>=81 & data2017$doy<=172], 
                          data2017$doy[data2017$doy>=173 & data2017$doy<=266],
                          data2017$doy[data2017$doy>=267 & data2017$doy<=354])

data2017$season[data2017$doy[data2017$doy>=81 & data2017$doy<=172]]<-2

winter<-c(data2017$doy[(data2017$doy>=355 & data2017$doy<=365) | (data2017$doy>=1 & data2017$doy<=80)])





data2016[data2016$doy>=355 | data2016$doy<=80, season="Winter"]


              #  data2016[data2016$doy>=81 | data2016$doy<=172,],
              # data2016[data2016$doy>=173 | data2016$doy<=266,],
             #  data2016[data2016$doy>=266 | data2016$doy<=354,])




data2016$seasons<-season

