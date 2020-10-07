#load in lubridate
library(lubridate)

#read in streamflow data
datH <- read.csv("/Users/benjaminmoffa/Documents/Github/stream_flow_data.csv",
                 na.strings = c("Eqp"))
head(datH)        

#read in precipitation data
#hourly precipitation is in mm
datP <- read.csv("/Users/benjaminmoffa/Documents/Github/2049867.csv")                            
head(datP)

