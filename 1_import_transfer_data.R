###This code is to convert the data downloaded from the GEE into a more readable format 
           #and to add date information.

library(zoo)
library(xts)
library(ggplot2)
library(lubridate)  #extract date year
library(stringr) #ectract number from "ALPHBET+NUMEBER"
library(beepr)

#####Read in data#####
# path_data <- "...."
setwd(path_data)
#Read in all csv data, if you have several csv for one index, combine them first (rbind)
NDVI<-read.csv(files_NDVI)
NBR2<-read.csv(files_NBR2)
Prec<-read.csv(files_Prec)
VV<-read.csv(files_VV)
VH<-read.csv(files_VH)
  
#List data together
datalist<-list(NDVI,NBR2,Prec,VV,VH)
datanames<-c("NDVI","NBR2","Prec","VV","VH")


for(j in 1:5){
  DATAuse<-datalist[[j]]
  name<-datanames[j]
  DATAuse <- dplyr::arrange(DATAuse, id)
  DATAuse<-t(DATAuse)
  DATAuse<-as.data.frame(DATAuse)
  colnames(DATAuse)<-c(DATAuse["id",])
  nrow<-nrow(DATAuse)
  DATAuse<-DATAuse[-c(1,nrow-1,nrow),] #delete unuseful field and empty point
  
  #add Date column
  data<-cbind(DATAuse,rownames(DATAuse))
  colnames(data)[length(colnames(data))]<-"Date"
  #colnames(data)
  data$Date<-gsub("X","",data$Date)
  #data$Date
  
  #add Date year/month/day
  data[,length(colnames(data))]<-as.Date(data$Date,format='%Y.%m.%d')
  year<-year(data$Date)
  month<-month(data$Date)
  yday<-yday(data$Date)
  data<-data.frame(data,year,month,yday)
  #datacopy<-data #store a copy
  #data<-NDVI710
  ncol(data)
  data<-data[,c((ncol(data)-3):ncol(data),1:(ncol(data)-4))]
  data[,c(2:ncol(data))]<-as.data.frame(lapply(data[,c(2:ncol(data))],as.numeric))
  data[data < -9998] <- NA
  #output NDVI
  write.csv(data,paste0(name,".csv"))
}

beep()
