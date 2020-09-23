assert <- function(statement, err.message){
  if(statement == FALSE){
    print(err.message)
  }
}
assert(1==2, "error: unequal values")
a<-c(1,2,3,4)
b<-c(1,2,3)
assert(length(a) == length(b),"error: unequal length")

#install lubridate package
install.packages(c("lubridate"))

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