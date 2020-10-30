
install.packages("usmap")
library(usmap)

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
mDat1<-subset(mDat, mDat$statename != "Puerto Rico")

#create new fips code for mDat
for (n in 1:691){
  tryCatch({
mDat1$countynew[n]<-fips(mDat1$statename[n], county = mDat1$county[n])
  }, error=function(e){})
}

#create new fips code for covidDat
for (n in 1:3244){
  tryCatch({
    covidDat$fips1[n]<-fips(covidDat$state[n], county = covidDat$county[n])
  }, error=function(e){})
}

#rename columns for merge
names(covidDat)[4]<-"fips1"
names(mDat1)[24]<-"fips1"

#merge covid data and metro/air data
cDat<-merge(mDat1, covidDat, by="fips1")

