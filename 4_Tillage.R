library(birk)
library(installr)
library(stringr)
library(ggplot2)
library(dygraphs)
library(xts)
library(stringr)
library(lubridate)
library(webshot)
library(htmlwidgets)
library(scales)
library(dplyr)
path_data <- ".../"
setwd(path_data)
fileds<-read.csv("fields_example.csv")
#no grassland
fileds<-fileds[which(fileds$crop2015 !=6 & 
                       fileds$crop2016 !=6 & fileds$crop2017 !=6 & 
                       fileds$crop2018 !=6 & fileds$crop2019  !=6 & 
                       fileds$crop2020!=6),]
legend<-read.csv("legend_example.csv")   #didnt modified try it first # for a late winter wheat start to identify CC in a season
colnames(legend)[1]<-c("CROP")
IDfind<-fileds$Only_ID
IDfind<-as.data.frame(IDfind)
##in order
IDfind <- dplyr::arrange(IDfind,IDfind)
colnames(IDfind)[1]<-c("OnlyID")

NDVI<-read.csv("NDVI_example.csv")
NBR2<-read.csv("NBR2_example.csv")
Prec<-read.csv("Prec_example.csv")
VV<-read.csv("VV_example.csv")
VH<-read.csv("VH_example.csv")
colnames(NDVI)[6:ncol(NDVI)]<-as.integer(str_extract_all(colnames(NDVI[6:ncol(NDVI)]),"[0-9]+"))
colnames(NBR2)[6:ncol(NBR2)]<-as.integer(str_extract_all(colnames(NBR2[6:ncol(NBR2)]),"[0-9]+"))
colnames(VV)[6:ncol(VV)]<-as.integer(str_extract_all(colnames(VV[6:ncol(VV)]),"[0-9]+"))
colnames(VH)[6:ncol(VH)]<-as.integer(str_extract_all(colnames(VH[6:ncol(VH)]),"[0-9]+"))  
colnames(Prec)[6:ncol(Prec)]<-as.integer(str_extract_all(colnames(Prec[6:ncol(Prec)]),"[0-9]+"))  
NDVI_all<-NDVI
NBR2_all<-NBR2
Prec_all<-Prec
VV_all<-VV
VH_all<-VH


bb<-NULL
cov<-c("NDVIMEAN","NBR2MEAN","NBR2min","NDVImin","NBR2_PEAK","difference_NBR2","VVmean","VHmean"
       ,"ratiomean","VVminpoint","VHminpoint","ratiominpoint")
year<-c("2018","2019","2020")
for(j in 1:3){
  aa<-c(paste(cov, year[j], sep=".")) 
  bb<-c(bb,aa)
}
bb 
Tillage <- data.frame(matrix(NA,nrow(IDfind),37))  #change to the number of points later
colnames(Tillage)<-c(bb,"IDnow")

for (i in 1:nrow(IDfind)){   
  IDnow<-IDfind[i,]
  NDVI<-NDVI_all[,c(2,which(colnames(NDVI_all)==IDnow))]
  NBR2<-NBR2_all[,c(2,which(colnames(NBR2_all)==IDnow))]
  VV<-VV_all[,c(2,which(colnames(VV_all)==IDnow))]
  VH<-VH_all[,c(2,which(colnames(VH_all)==IDnow))]
  prec<-Prec_all[,c(2,which(colnames(Prec_all)==IDnow))]
  
  S1<-cbind(VV,VH[,2])
  S1<-S1[complete.cases(S1),] 
  S1$ratio<-round(S1[,2]/S1[,3],2)
  colnames(S1)<-c("Date","VV","VH","ratio")
  S1 <- dplyr::arrange(S1,Date)
  S1$Date<-as.Date(S1$Date, '%Y-%m-%d')
  
  #S1<-cbind(S1,S1[,2]/S1[,3])
  S2<-cbind(NDVI,NBR2[,2])
  S2<-S2[complete.cases(S2),] 
  colnames(S2)<-c("Date","NDVI","NBR2")
  S2$Date<-as.Date(S2$Date, '%Y-%m-%d')
  
  #remove outlier points
  smooth_NDVI<-read.csv(paste0("C:/Yue/Data/Precip/New data/Process1_NDVI_season/",IDnow,"smoothNDVI.csv"))
  colnames(smooth_NDVI)[2]<-"Date"
  smooth_NDVI$Date<-as.Date(smooth_NDVI$Date, '%Y-%m-%d')
  combine<-merge(S2, smooth_NDVI[,c(2,6)], by = "Date", left = TRUE)
  combine<-combine[which(combine$witer3>0.01),]       #w <0.01 outlier
  
  
  S2<-combine[,1:3]
  #colnames(S2)<-c("Date","NDVI","NBR2")
  S2 <- dplyr::arrange(S2,Date)
  #NBR2 PEAK
  x<-S2$NBR2
  x.1<-c(x[-1],0)
  n<-nrow(S2)
  x.2<-c(0,x[-n])
  dx.1 <- sign(diff(x, na.pad = FALSE))
  pks <- which(diff(dx.1, na.pad = FALSE) < 0 & dx.1[-(n-1)] > 0) + 1   #detect peaks
  #transfer peaks to 0/1
  S2$NBR2_peaks<-1
  S2$NBR2_peaks[-c(pks)]<-0
  
  ####NDVI trend
  XNDVI<-S2$NDVI
  x.t2 <- c(0,sign(diff(XNDVI, na.pad = FALSE)))   #relation with former point
  S2$NDVI_trend<-x.t2
  
  
  prec<-prec[complete.cases(prec),] 
  colnames(prec)<-c("Date","prec")
  prec <- dplyr::arrange(prec,Date)
  prec$Date<-as.Date(prec$Date, '%Y-%m-%d')
  
  
  Cropuse<-fileds[which(fileds$Only_ID==IDnow),]
  calendar2018<-legend[legend$CROP==Cropuse[,"crop2018"],]
  calendar2019<-legend[legend$CROP==Cropuse[,"crop2019"],]
  calendar2020<-legend[legend$CROP==Cropuse[,"crop2020"],]
  
  #define a potential tillage period and a bigger one(for fowlloing calculation) 
  tillage_detect_interval<-40 #how many days after harvest
  bigger_range<-50
  #2018
  date1 <- as.POSIXct(paste((2018+calendar2018$start_year),"-",calendar2018$start_month,"-",calendar2018$start_day,sep=""))
    #harvest
  date2 <- as.POSIXct(paste((2018+calendar2018$stop_year),"-",calendar2018$stop_month,"-",calendar2018$stop_day,sep=""))
  event1 <- calendar2018$name2015
    #after harvest 40 day
  date2.1 <- date2+60*60*24*tillage_detect_interval
    #before harvaest 50 day
  date2.0<-date2-60*60*24*bigger_range
  #event1.1 <- calendar2018$name2015
  int1 <- interval(date2, date2.1) 
  int1bigger<-interval(date2.0, date2.1) 
  
  #2019
  date3 <- as.POSIXct(paste((2019+calendar2019$start_year),"-",calendar2019$start_month,"-",calendar2019$start_day,sep=""))
  date4 <- as.POSIXct(paste((2019+calendar2019$stop_year),"-",calendar2019$stop_month,"-",calendar2019$stop_day,sep=""))
  date4.1 <-  date4+60*60*24*tillage_detect_interval
  event2 <- calendar2019$name2015
  int2 <- interval(date4, date4.1) 
  int2bigger <- interval(date4-60*60*24*bigger_range, date4.1) 
  
  
  #2020
  date5 <- as.POSIXct(paste((2020+calendar2020$start_year),"-",calendar2020$start_month,"-",calendar2020$start_day,sep=""))
  date6 <- as.POSIXct(paste((2020+calendar2020$stop_year),"-",calendar2020$stop_month,"-",calendar2020$stop_day,sep=""))
  date6.1 <-  date6+60*60*24*tillage_detect_interval
  event3 <- calendar2020$name2015
  #int5 <- interval(date9, date10) 
  int3 <- interval(date6, date6.1) 
  int3bigger <- interval(date6-60*60*24*bigger_range, date6.1) 
  
  time<-c(int1,int2,int3)
  
  timebigger<-c(int1bigger,int2bigger,int3bigger)
  
  Precday<-4
  Tillage[i,37]<-IDnow
  
  
  for(z in 1:length(time)){
    chooseinPeriod_bigger<- S2[ S2$Date %within% timebigger[z],]
    try( chooseinPeriod_bigger$Prec<-NA, silent = T)
    Npoint<-nrow(chooseinPeriod_bigger)
    try(  for (n in 1: Npoint){
      chooseinPeriod_bigger$Prec[n]<-sum(prec[prec$Date %within% interval(chooseinPeriod_bigger[n,]$Date-Precday,chooseinPeriod_bigger[n,]$Date),2])
    },silent = T)
    chooseinPeriod<- chooseinPeriod_bigger[ chooseinPeriod_bigger$Date %within% time[z],]
    try( NBR2min<-min(chooseinPeriod$NBR2),silent = T)
    try( NDVImin<-min(chooseinPeriod$NDVI),silent = T)
   
     #tillage cases
    #Case 1 NBR2 PEAK AND no rain
    if(any(chooseinPeriod[which(chooseinPeriod$NBR2_peaks==1  & chooseinPeriod$NDVI_trend==-1),c("Prec")]<2)){
      NBR2_PEAK<-1
    }else{
      NBR2_PEAK<-0}

    #Case 2  
    #NBR2min<0.1 & NDVImin<0.25
    #NDVI data
   # chooseinPeriod_bigger<-chooseinPeriod_bigger[which(chooseinPeriod_bigger$Prec<2.5),] #remove high precipitation event
    chooseinPeriod_norain<- chooseinPeriod[which(chooseinPeriod$Prec<2.5),]

    NDVIMEAN<-mean( chooseinPeriod$NDVI)
    NBR2MEAN<-mean( chooseinPeriod_norain$NBR2)
    No_min<-which(chooseinPeriod_bigger$NBR2==NBR2min) #row number of min NBR2 point
    Date_min<-chooseinPeriod_bigger$Date[No_min]   #date of  min NBR2 point
    
    chooseinPeriod_bigger$NBR2_RESCALE<- chooseinPeriod_bigger$NBR2/rescale(chooseinPeriod_bigger$Prec, to=c(1,2),from=c(0,50))
  
    difference_NBR2<-chooseinPeriod_bigger$NBR2_RESCALE[No_min-1]- chooseinPeriod_bigger$NBR2_RESCALE[No_min]     
  
  
    chooseinPeriod_S1<-S1[ S1$Date %within% time[z],]
    VVmean<-mean(chooseinPeriod_S1$VV)
    VHmean<-mean(chooseinPeriod_S1$VH)
    ratiomean<-mean(chooseinPeriod_S1$ratio)
    
    #replace nearest day
    VVminpoint<-last(chooseinPeriod_S1[which.closest(chooseinPeriod_S1$Date, Date_min),2])
    VHminpoint<-last(chooseinPeriod_S1[which.closest(chooseinPeriod_S1$Date, Date_min),3])
    ratiominpoint<-last(chooseinPeriod_S1[which.closest(chooseinPeriod_S1$Date, Date_min),4])
    
    #add all covariates here
    temp<-data.frame(matrix(NA,1,12))
    try( temp[5:6]<-c(NBR2_PEAK,difference_NBR2 ),silent = T)
    try( temp[1:4]<-c(NDVIMEAN,NBR2MEAN,NBR2min,NDVImin),silent = T)
    try( temp[7:12]<-c(VVmean,VHmean,ratiomean,VVminpoint, VHminpoint,ratiominpoint),silent = T)
    
    
    Tillage[i,c((12*z-11):(12*z))]<-temp
    
  }
}


colnames(fileds)[6:8]<-c("CROP.2018","CROP.2019","CROP.2020")

colnames(Tillage)[37]<-"Only_ID"

Tillagedata<-merge(fileds[,c(2,6:8)],Tillage,by="Only_ID",right=TRUE)
#  Tillagedata<-Tillagedata[which(Tillagedata$DebutAC!=2021),] choose later

library(reshape)
reshape<-reshape(data = Tillagedata,
                 idvar= "Only_ID",
                 timevar= "year",
                 times = c(2018,2019,2020),
                 varying = 2:40,
                 sep= ".",
                 #v.names = c("NDVIMEAN","NBR2MEAN","NBR2min","NDVImin","difference_NBR2","difference_NDVI","VVmean","VHmean"
                 #           ,"ratiomean","VVminpoint","VHminpoint","ratiominpoint"),
                 new.row.names= 1:60000,
                 direction = "long")

#If you want to build to model for tillage, 
#you need a file to define the tillage practice for each field.
#fileds_info<-read.csv("CAfileds.csv") 
#Now you could link the covariates with your own tillage data and build a model for tillage practice.




