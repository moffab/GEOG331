assert <- function(statement, err.message){
  if(statement == FALSE){
    print(err.message)
  }
}
assert(1==2, "error: unequal values")
a<-c(1,2,3,4)
b<-c(1,2,3)
assert(length(a) == length(b),"error: unequal length")

#FROM ONLINE ACTIVITY3 TUTORIAL
#install lubridate package; function for working with dates and times
install.packages(c("lubridate"))
library(lubridate)
#read in datafile bewkes_weather.csv and assign to datW, indicate formatting options below
# for header and string #N/A, and skipping first three rows when reading in data
datW<-read.csv("/Users/benjaminmoffa/Documents/Github/bewkes/bewkes_weather.csv",
na.strings=c("#N/A"), skip=3, header=FALSE)
#preview data
print(datW[1,])

#read in datafile bewkes_weather.csv and assign to sensorInfor, indicate formatting options
#for #N/A and read in only first two rows where nonnumerical data is
sensorInfo<-read.csv("/Users/benjaminmoffa/Documents/Github/bewkes/bewkes_weather.csv",
                         na.strings=c("#N/A"), nrows=2)
print(sensorInfo)

colnames(datW) <-   colnames(sensorInfo)
#preview data
print(datW[1,])

#date form in m/d/y, mdy_hm standardizes timestamp
dates<-mdy_hm(datW$timestamp, tz="America/New_York")
#calculate day of year
datW$doy<-yday(dates)
#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)
#quick preview of new date calcualtions
datW[1,]

#count number of missing observations, denoted by NA, for the following variables:
#air temp
length(which(is.na(datW$air.temperature)))
#wind speed
length(which(is.na(datW$wind.speed)))
#precipitation
length(which(is.na(datW$precipitation)))
#soil temperature
length(which(is.na(datW$soil.moisture)))
#soil moisture
length(which(is.na(datW$soil.temp)))

#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")
#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")

#looks at air values at the extreme range of the data
#and throughout the percentiles
quantile(datW$air.temperature)

#look at days with really low air temperature
datW[datW$air.temperature< 8,]  
#look at days with really high air temperature
datW[datW$air.temperature > 33,]  


#plot precipitation and lightning strikes on the same plot
#normalize lighting strikes to match precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy
#make the plot with precipitation and lightning activity marked
#make it empty to start and add in features
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
     type="n")
#plot precipitation points only when there is precipitation 
#make the points semi-transparent
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        

#plot lightning points only when there is lightning     
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)


#QUESTION 5

#filters out suspect air temperature observations
# filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm.    
#create a new air temp column
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.temperature))

#checks to see if the total time that lightning occurs is equal between each data subset
subl<-datW$DD[lightscale>0]
sort(subl)
suba<-datW$DD[datW$lightning.acvitivy>0]
sort(suba)
assert (length(subl) == length(suba), "error:unequal lengths")
for (n in length(subl)){
  assert(subl[n] == suba[n], "not equal")
}


#QUESTION 6
#filters out suspect wind speed observations
# filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm.    
#create a new air temp column
datW$wind.speedQ2<- ifelse(datW$precipitation>=2 & datW$lightning.acvitivy>0, NA, 
                          ifelse(datW$precipitation>5, NA, datW$wind.speed))

#check and see the number of NA values in datW$wind.speedQ2 match the number of values 
#with the specified conditions for stormy weather
sub1<-datW$wind.speed[(datW$precipitation>=2 & datW$lightning.acvitivy>0) | (datW$precipitation>5)]
sub1
assert(length(which(is.na(datW$wind.speedQ2)))==length(sub1))

#check contents of each 
sub2<-datW$DD[(datW$precipitation>=2 & datW$lightning.acvitivy>0) | (datW$precipitation>5)]
sub2
sub3<-datW$DD[is.na(datW$wind.speedQ2)]
sub3
#go through for loop after sorting and see if each DD is the same NA values and values where
#extreme weather conditions apply
sort(sub2)
sort(sub3)
assert(length(sub2) == length(sub3))
for (n in length(sub2)){
assert(sub2[n] == sub3[n], "not equal")
}

#Question 7

#compare soil temperature and air temperature
plot(datW$DD, datW$air.tempQ2, pch=19, type="b", xlab = "Day of Year",
     ylab="Temperature")
points(datW$DD, datW$soil.temp,
       col= "tomato3", pch=19)

#compare standardized precipitation and soil moisture
sm<- scale(datW$soil.moisture)
plot(datW$DD, datW$precipitation, pch=19, type="b", xlab = "Day of Year",
     ylab="Precipitation")
points(datW$DD[sm>=0], sm[sm>=0],
       col= "tomato3", pch=19)


#QUESTION 8

#get average air temp, wind speed, soil moisture, and soil temp, as well as 
#number of values that are not NA
averageairTemp<-mean(datW$air.tempQ2, na.rm = TRUE)
averageairTemp
length(which(!is.na(datW$air.tempQ2)))
averagewindSpeed<-mean(datW$wind.speedQ2, na.rm=TRUE)
averagewindSpeed
length(which(!is.na(datW$wind.speedQ2)))
averagesoilMoisture<-mean(datW$soil.moisture, na.rm=TRUE)
averagesoilMoisture
length(which(!is.na(datW$soil.moisture)))
averagesoilTemp<-mean(datW$soil.temp, na.rm=TRUE)
averagesoilTemp
length(which(!is.na(datW$soil.temp)))


#get the total amount of precipitation over the time period
totalprecip<-sum(datW$precipitation, na.rm=TRUE)

#get the range of time period for the data as well as the number of observations
nrow(datW)
datW[1,]
datW[2118,]

#QUESTION 9
#plot soil moisture
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil Moisture")
#plot air temp
plot(datW$DD, datW$air.tempQ2, pch=19, type="b", xlab = "Day of Year",
     ylab="Air Temperature")
#plot soil temp
plot(datW$DD, datW$soil.temp, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil Temperature")
#plot precipitation
plot(datW$DD, datW$precipitation, pch=19, type="b", xlab = "Day of Year",
     ylab="Wind Speed")



