install.packages("usmap")
library(usmap)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)



#import air quality data and change names of population and cbsacode columns
airDat0<-read.csv("/Users/benjaminmoffa/Documents/Github/GEOG331project/countyair.csv", skip=2, na.strings = c("ND", "IN"))

#import covid data
covidDat<-read.csv("/Users/benjaminmoffa/Documents/Github/GEOG331project/us-counties.csv")

#import crosswalk data and subset it for only central counties 
crossDat0<-read.csv("/Users/benjaminmoffa/Documents/Github/GEOG331project/cbsa2fipsxw.csv")
crossDat0$inout<-0
crossDat0$inout[crossDat0$centraloutlyingcounty == "Central"]<-1



#create new and uniform fips code for all datasets 
for (n in 1:1883){
  tryCatch({
    crossDat0$fips1[n]<-fips(crossDat0$statename[n], county = crossDat0$countycountyequivalent[n])
  }, error=function(e){})
}
crossSub<-subset(crossDat0, crossDat0$fips1 != "NA" & crossDat0$metropolitanmicropolitanstatis == "Metropolitan Statistical Area" & crossDat0$statename != "Puerto Rico" & crossDat0$inout==1)

for (n in 1:3244){
  tryCatch({
    covidDat$fips1[n]<-fips(covidDat$state[n], county = covidDat$county[n])
  }, error=function(e){})
}

for (n in 1:1160){
  tryCatch({
    airDat0$fips1[n]<-fips(airDat0$State[n], county = airDat0$County[n])
  }, error=function(e){})
}


#merge crosswalk data, airpollutant data and covid data by fips
cDat1<-merge(crossSub, covidDat, by="fips1")
cDat<-merge(cDat1, airDat0, by="fips1")

#rename air pollutant data
names(cDat)[29]<-"carbon"
names(cDat)[30]<-"lead"
names(cDat)[31]<-"nitrogen_mean"
names(cDat)[32]<-"nitrogen_dmax"
names(cDat)[33]<-"Ozone"
names(cDat)[34]<-"PM10"
names(cDat)[35]<-"PM2.5_annualmean"
names(cDat)[36]<-"PM2.5_98th"
names(cDat)[37]<-"Sulfur"

#calculate deaths per case
cDat$dperc<-0
cDat$dperc<-(cDat$deaths/cDat$cases)

#calculate log of population
cDat$logpop<-log(cDat$X2010.Population)

#create summary statistics 
#sum stats code partially sourced from https://learnr4ds.com/html/tables-in-r.html
sumstat <- cDat %>%
  select(
    `Death Per Case` = dperc,
    `Population` = X2010.Population,
    `Nitrogen` = nitrogen_mean,
    `Ozone` = Ozone,
    `PM2.5` = PM2.5_annualmean,
    `Sulfur` = Sulfur,
  ) %>%
  # Find the mean, st. dev., min, and max for each variable 
  summarise_all(funs(mean, sd, min, max), na.rm=TRUE) %>%
  # Move summary stats to columns
  gather(key, value, everything()) %>% 
  separate(key, into = c("variable", "stat"), sep = "_") %>%
  spread(stat, value) %>%
  # Set order of summary statistics 
  select(variable, mean, sd, min, max) %>%
  # Round all numeric variables to one decimal point
  mutate_each(funs(round(., 2)), -variable)
sumstat


#Graphs

#deaths per capita by polutant
ggplot(aes(x=nitrogen_mean,y=dperc),data=cDat)+
  geom_point()+
  geom_smooth(method=lm)
ggplot(aes(x=Ozone,y=dperc),data=cDat)+
  geom_point()+
  geom_smooth(method=lm)
ggplot(aes(x=PM2.5_annualmean,y=dperc),data=cDat)+
  geom_point()+
  geom_smooth(method=lm)
ggplot(aes(x=Sulfur,y=dperc),data=cDat)+
  geom_point()+
  geom_smooth(method=lm)

#pollutant by population
ggplot(aes(x=logpop,y=Ozone),data=cDat)+
  geom_point()+
  geom_smooth(method=lm)
ggplot(aes(x=logpop,y=nitrogen_mean),data=cDat)+
  geom_point()+
  geom_smooth(method=lm)
ggplot(aes(x=logpop,y=PM2.5_annualmean),data=cDat)+
  geom_point()+
  geom_smooth(method=lm)
ggplot(aes(x=logpop,y=Sulfur),data=cDat)+
  geom_point()+
  geom_smooth(method=lm)

#dperc by population
ggplot(aes(x=logpop,y=dperc),data=cDat)+
  geom_point()+
  geom_smooth(method=lm)

#Regressions results

#dperc on pollutant
lmozone = lm(dperc~Ozone, data = cDat)
summary(lmozone)
lmnitrogen = lm(dperc~nitrogen_mean, data = cDat)
summary(lmnitrogen)
lmPM_2.5 = lm(dperc~PM2.5_annualmean, data = cDat)
summary(lmPM_2.5)
lmsulfer = lm(dperc~Sulfur, data = cDat)
summary(lmsulfer)





