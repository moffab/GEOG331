
#import air quality data and change names of population and cbsacode columns
airDatO<-read.csv("/Users/benjaminmoffa/Documents/Github/GEOG331project/airdata1.csv", skip=2)
airDat<-subset(airDatO, airDatO$CBSA.Code>0)
names(airDat)[2]<-"population"
names(airDat)[3]<-"cbsacode"

#import covid data
covidDat<-read.csv("/Users/benjaminmoffa/Documents/Github/GEOG331project/us-counties.csv")

#import crosswalk data and subset it for only central counties 
crossDatO<-read.csv("/Users/benjaminmoffa/Documents/Github/GEOG331project/cbsa2fipsxw.csv")
crossDat<-subset(crossDatO, crossDatO$centraloutlyingcounty == "Central")

#merge crosswalk and air data
mergeddata<-merge(airDat, crossDat, by="cbsacode")
#subset so only includess metropolitan areas
mDat<-subset(mergeddata, mergeddata$metropolitanmicropolitanstatis == "Metropolitan Statistical Area")
#rename column 19 as county
names(mDat)[19]<-"county"

cDat<-merge(mDat, covidDat, by="county")