
library(R.utils)
library(rmarkdown)
library(knitr)
Sys.setlocale("LC_TIME", "English")
temp <- tempfile()

##Performing the download
if(!file.exists("/stormData.csv.bz2")){
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile="./stormData.csv.bz2")
}
##Uncompressing the file
if(!file.exists("stormdata.csv"))
{
  bunzip2("stormData.csv.bz2","stormdata.csv",remove=F)
}

# DP1.3 loading the data & reading the file

storm <- read.csv("stormdata.csv",header=TRUE,sep=",")
summary(storm)
variables<-c("EVTYPE","FATALITIES","INJURIES","PROPDMG", "PROPDMGEXP","CROPDMG","CROPDMGEXP")
strmdata<-storm[variables]

dim(strmdata)

names(strmdata)
Fatalities <- aggregate(FATALITIES ~ EVTYPE, data = strmdata, FUN = sum)
Top10_Fatalities <- Fatalities[order(-Fatalities$FATALITIES), ][1:10, ] 
Top10_Fatalities 

Injuries <- aggregate(INJURIES ~ EVTYPE, data = strmdata, FUN = sum)
Top10_Injuries <- Injuries[order(-Injuries$INJURIES), ][1:10, ] 
Top10_Injuries 


par(mfrow=c(1,2),mar=c(10,3,3,2))
barplot(Top10_Fatalities$FATALITIES,names.arg=Top10_Fatalities$EVTYPE,las=2,col="purple",ylab="fatalities",main="Top 10 fatalities")
barplot(Top10_Injuries$INJURIES,names.arg=Top10_Injuries$EVTYPE,las=2,col="purple",ylab="injuries",main="Top 10 Injuries")



# Assigning values for the property exponent strmdata 
strmdata$PROPEXP[strmdata$PROPDMGEXP == "K"] <- 1000
strmdata$PROPEXP[strmdata$PROPDMGEXP == "M"] <- 1e+06
strmdata$PROPEXP[strmdata$PROPDMGEXP == ""] <- 1
strmdata$PROPEXP[strmdata$PROPDMGEXP == "B"] <- 1e+09
strmdata$PROPEXP[strmdata$PROPDMGEXP == "m"] <- 1e+06
strmdata$PROPEXP[strmdata$PROPDMGEXP == "0"] <- 1
strmdata$PROPEXP[strmdata$PROPDMGEXP == "5"] <- 1e+05
strmdata$PROPEXP[strmdata$PROPDMGEXP == "6"] <- 1e+06
strmdata$PROPEXP[strmdata$PROPDMGEXP == "4"] <- 10000
strmdata$PROPEXP[strmdata$PROPDMGEXP == "2"] <- 100
strmdata$PROPEXP[strmdata$PROPDMGEXP == "3"] <- 1000
strmdata$PROPEXP[strmdata$PROPDMGEXP == "h"] <- 100
strmdata$PROPEXP[strmdata$PROPDMGEXP == "7"] <- 1e+07
strmdata$PROPEXP[strmdata$PROPDMGEXP == "H"] <- 100
strmdata$PROPEXP[strmdata$PROPDMGEXP == "1"] <- 10
strmdata$PROPEXP[strmdata$PROPDMGEXP == "8"] <- 1e+08

# Assigning '0' to invalid exponent strmdata
strmdata$PROPEXP[strmdata$PROPDMGEXP == "+"] <- 0
strmdata$PROPEXP[strmdata$PROPDMGEXP == "-"] <- 0
strmdata$PROPEXP[strmdata$PROPDMGEXP == "?"] <- 0

# Calculating the property damage value
strmdata$PROPDMGVAL <- strmdata$PROPDMG * strmdata$PROPEXP


# Q2.3 Defining & Calcuating [ Crop Damage ]

## Crop damage exponents for each level listed out & assigned those values for the crop exponent data. 
## Invalid data was excluded by assigning the value as '0'. 
## Then, the crop damage value was calculated by multiplying the crop damage and crop exponent value.

unique(strmdata$CROPDMGEXP)


strmdata$CROPEXP[strmdata$CROPDMGEXP == "M"] <- 1e+06
strmdata$CROPEXP[strmdata$CROPDMGEXP == "K"] <- 1000
strmdata$CROPEXP[strmdata$CROPDMGEXP == "m"] <- 1e+06
strmdata$CROPEXP[strmdata$CROPDMGEXP == "B"] <- 1e+09
strmdata$CROPEXP[strmdata$CROPDMGEXP == "0"] <- 1
strmdata$CROPEXP[strmdata$CROPDMGEXP == "k"] <- 1000
strmdata$CROPEXP[strmdata$CROPDMGEXP == "2"] <- 100
strmdata$CROPEXP[strmdata$CROPDMGEXP == ""] <- 1

# Assigning '0' to invalid exponent strmdata
strmdata$CROPEXP[strmdata$CROPDMGEXP == "?"] <- 0

# calculating the crop damage 
strmdata$CROPDMGVAL <- strmdata$CROPDMG * strmdata$CROPEXP


# Q2.4 Property Damage Summary

## Procedure = aggregate the property damage by the event type and sort the output it in descending order

prop <- aggregate(PROPDMGVAL~EVTYPE,data=strmdata,FUN=sum,na.rm=TRUE)
prop <- prop[with(prop,order(-PROPDMGVAL)),]
prop <- head(prop,10)
print(prop)




crop <- aggregate(CROPDMGVAL~EVTYPE,data=strmdata,FUN=sum,na.rm=TRUE)
crop <- crop[with(crop,order(-CROPDMGVAL)),]
crop <- head(crop,10)
print(crop)



par(mfrow=c(1,2),mar=c(11,3,3,2))
barplot(prop$PROPDMGVAL/(10^9),names.arg=prop$EVTYPE,las=2,col="gold",ylab="Prop.damage(billions)",main="Top10 Prop.Damages")
barplot(crop$CROPDMGVAL/(10^9),names.arg=crop$EVTYPE,las=2,col="gold",ylab="Crop damage(billions)",main="Top10 Crop.Damages")



