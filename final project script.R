install.packages("usmap")
library(usmap)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)

#https://www.nber.org/research/data/us-intercensal-county-population-data-age-sex-race-and-hispanic-origin
#demDat<-read.csv("/Users/benjaminmoffa/Documents/Github/GEOG331project/Book2CSV.csv")
#names(demDat)[4]<-"fips"
#demDat1<-subset(demDat, agegrp==0)

#import air quality data and change names of population and cbsacode columns
airDatO<-read.csv("/Users/benjaminmoffa/Documents/Github/GEOG331project/airdata1.csv", skip=2, na.strings = c("ND", "IN"))
airDat<-subset(airDatO, airDatO$CBSA.Code>0)
names(airDat)[2]<-"population"
names(airDat)[3]<-"cbsacode"

#import covid data
covidDat<-read.csv("/Users/benjaminmoffa/Documents/Github/GEOG331project/us-counties.csv")

#import crosswalk data and subset it for only central counties 
crossDat0<-read.csv("/Users/benjaminmoffa/Documents/Github/GEOG331project/cbsa2fipsxw.csv")
#crossDat<-subset(crossDatO, crossDatO$centraloutlyingcounty == "Central")
crossDat0$inout<-0
crossDat0$inout[crossDat0$centraloutlyingcounty == "Central"]<-1


#merge crosswalk and air data
mergeddata<-merge(airDat, crossDat0, by="cbsacode")
#subset so only includess metropolitan areas
mDat<-subset(mergeddata, mergeddata$metropolitanmicropolitanstatis == "Metropolitan Statistical Area")
#rename column 19 as county
names(mDat)[19]<-"county"
mDat1<-subset(mDat, mDat$statename != "Puerto Rico")


mDat1$countynew<-0
#create new fips code for mDat
for (n in 1:691){
  tryCatch({
mDat1$countynew[n]<-fips(mDat1$statename[n], county = mDat1$county[n])
  }, error=function(e){})
}



covidDat$fips1<-0
#create new fips code for covidDat
for (n in 1:3244){
  tryCatch({
    covidDat$fips1[n]<-fips(covidDat$state[n], county = covidDat$county[n])
  }, error=function(e){})
}

#names(mDat1)[24]<-"inout"
#rename columns for merge
names(mDat1)[25]<-"fips1"

#merge covid data and metro/air data
cDat<-merge(mDat1, covidDat, by="fips1")
cDat = cDat[!cDat$fips1==0,]

#rename air pollutant data
names(cDat)[5]<-"carbon"
names(cDat)[6]<-"lead"
names(cDat)[7]<-"nitrogen_mean"
names(cDat)[8]<-"nitrogen_dmax"
names(cDat)[9]<-"Ozone"
names(cDat)[10]<-"PM10"
names(cDat)[11]<-"PM2.5_annualmean"
names(cDat)[12]<-"PM2.5_98th"
names(cDat)[13]<-"Sulfer"

#calculate deaths per case
cDat$dperc<-0
cDat$dperc<-(cDat$deaths/cDat$cases)


#summary stats
sumstat <- cDat %>%
  select(
    `Death Per Case` = dperc,
    `Central or Outlying` = inout,
    `Nitrogen` = nitrogen_mean,
    `Ozone` = Ozone,
    `PM2.5` = PM2.5_annualmean,
    `Sulfer` = Sulfer,
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
write.table(sumstat, file = "sumstats.doc", sep = ",", quote = FALSE, row.names = F)


#Graphs
ggplot(aes(x=carbon,y=dperc),data=cDat)+
  geom_point()+
  geom_smooth(method=lm)

ggplot(aes(x=lead,y=dperc),data=cDat)+
  geom_point()+
  geom_smooth(method=lm)

ggplot(aes(x=nitrogen_mean,y=dperc),data=cDat)+
  geom_point()+
  geom_smooth(method=lm)

ggplot(aes(x=nitrogen_dmax,y=dperc),data=cDat)+
  geom_point()+
  geom_smooth(method=lm)

ggplot(aes(x=Ozone,y=dperc),data=cDat)+
  geom_point()+
  geom_smooth(method=lm)

ggplot(aes(x=PM10,y=dperc),data=cDat)+
  geom_point()+
geom_smooth(method=lm)

ggplot(aes(x=PM2.5_annualmean,y=dperc),data=cDat)+
  geom_point()+
  geom_smooth(method=lm)

ggplot(aes(x=PM2.5_98th,y=dperc),data=cDat)+
  geom_point()+
  geom_smooth(method=lm)

ggplot(aes(x=Sulfer,y=dperc),data=cDat)+
  geom_point()+
  geom_smooth(method=lm)

#Regressions

lmozone = lm(dperc~Ozone+inout, data = cDat)
summary(lmozone)

lmnitrogen = lm(dperc~nitrogen_mean+inout, data = cDat)
summary(lmnitrogen)

lmPM_2.5 = lm(dperc~PM2.5_annualmean+inout, data = cDat)
summary(lmPM_2.5)

lmsulfer = lm(dperc~Sulfer+inout, data = cDat)
summary(lmsulfer)





#lmozoneb = lm(dperc~black+hispanic+inout, data = totalDat)
#summary(lmozoneb)

#lmnitrogenb = lmnitrogen = lm(dperc~black+hispanic+inout, data = totalDat)
#summary(lmnitrogenb)



#lmozone = lm(dperc~Ozone+inout+black+hispanic, data = totalDat)
#summary(lmozone)

#lmnitrogen = lm(dperc~nitrogen_mean+inout+black+hispanic, data = totalDat)
#summary(lmnitrogen)

#lmPM_2.5 = lm(dperc~PM2.5_annualmean+inout+black+hispanic, data = totalDat)
#summary(lmPM_2.5)

#lmsulfer = lm(dperc~Sulfer+inout+black+hispanic, data = totalDat)
#summary(lmsulfer)


#merge county demographic data and calculate racial statistics
#totalDat<-merge(cDat,demDat1, by="fips")
#totalDat$white<-((totalDat$wa_male + totalDat$wa_female) / totalDat$tot_pop)
#totalDat$black<-((totalDat$ba_male + totalDat$ba_female) / totalDat$tot_pop)
#totalDat$asian<-((totalDat$aa_male + totalDat$aa_female) / totalDat$tot_pop)
#totalDat$hispanic<-((totalDat$h_male + totalDat$h_female) / totalDat$tot_pop)

#m1<-tidy(lm(dperc~Ozone+inout, data = cDat))
#m2<-tidy(lm(dperc~nitrogen_mean+inout, data = cDat))
#m3<-tidy(lm(dperc~PM2.5_annualmean+inout, data = cDat))
#m4<-tidy(lm(dperc~Sulfer+inout, data = cDat))

#all_models <- rbind(
#  m1 %>% mutate(model = 1),
#  m2 %>% mutate(model = 2),
#  m3 %>% mutate(model = 3),
#  m4 %>% mutate(model = 4))

#all_models
