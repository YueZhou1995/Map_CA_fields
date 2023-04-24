#####This code is for calculating the	length of the cover crop growing season and periods of bare soil
library(ggplot2)
library(dygraphs)
library(xts)
library(stringr)
library(lubridate)
library(htmlwidgets)
library(webshot)


path_data <- "..."
setwd(path_data)

#add a csv for the calendar of all of the crops. # you can find a csv example in the repository
legend<-read.csv("legend_example.csv")   
colnames(legend)[1]<-c("CROP")


NDVI<-read.csv("NDVI_example.csv")
NBR2<-read.csv("NBR2_example.csv")
Prec<-read.csv("Prec_example.csv")

colnames(NDVI)[6:ncol(NDVI)]<-as.integer(str_extract_all(colnames(NDVI[6:ncol(NDVI)]),"[0-9]+"))
colnames(NBR2)[6:ncol(NBR2)]<-as.integer(str_extract_all(colnames(NBR2[6:ncol(NBR2)]),"[0-9]+"))
colnames(Prec)[6:ncol(Prec)]<-as.integer(str_extract_all(colnames(Prec[6:ncol(Prec)]),"[0-9]+"))

NDVI_all<-NDVI
NBR2_all<-NBR2
Prec_all<-Prec

#add fields data, should include crop type (code) during 2015-2020 #you can find a csv example in the repository
fields<-read.csv("fields_example.csv")
#no grassland
fields<-fields[which(fields$crop2015 !=6 & 
                       fields$crop2016 !=6 & fields$crop2017 !=6 & 
                       fields$crop2018 !=6 & fields$crop2019  !=6 & 
                       fields$crop2020!=6),]
ID_fields<-fields$Only_ID
IDfind<-colnames(NDVI_all)[6:ncol(NDVI_all)]
IDfind<-as.integer(str_extract_all(IDfind,"[0-9]+"))
IDfind<-as.data.frame(IDfind)
##in order
IDfind <- dplyr::arrange(IDfind,IDfind)
colnames(IDfind)[1]<-c("OnlyID")


bb<-NULL
cov<-c("length","NDVImean","NBR2mean","CC","CClength","CCstart","CCend","BSlength","RClength")  #residue cover
year<-c("Inter1_","Inter2_","Inter3_","Inter4_","Inter5_")
for(j in 1:5){
  aa<-c(paste(year[j],cov,  sep="")) 
  bb<-c(bb,aa)
}
bb 

Inter <- data.frame(matrix(NA,nrow(IDfind),46))  
colnames(Inter)<-c(bb,"IDnow")

d<-Sys.time()


for (i in 1:nrow(IDfind)){  
  print(i)
  IDnow<-IDfind[i,]
  NDVI<-NDVI_all[,c(2,which(colnames(NDVI_all)==IDnow))]
  NBR2<-NBR2_all[,c(2,which(colnames(NBR2_all)==IDnow))]
  prec<-Prec_all[,c(2,which(colnames(Prec_all)==IDnow))]
  
  prec<-prec[complete.cases(prec),] 
  
  colnames(prec)<-c("Date","prec")
  prec <- dplyr::arrange(prec,Date)
  prec$Date<-as.Date(prec$Date, '%Y-%m-%d')
  
  S2<-merge(NDVI, NBR2, by = "Date", left = TRUE)
  S2<-S2[complete.cases(S2),] 
  
  #remove outlier points  based on results of Smooth NDVI
  smooth_NDVI<-read.csv(paste0(".../Process1_NDVI_season/",IDnow,"smoothNDVI.csv"))
  colnames(smooth_NDVI)[2]<-"Date"
  combine<-merge(S2, smooth_NDVI[,c(2,6)], by = "Date", left = TRUE)
  combine<-combine[which(combine$witer3>0.01),]       #w <0.01 outlier
  
  S2<-combine[,1:3]
  colnames(S2)<-c("Date","NDVI","NBR2")
  S2$Date<-as.Date(S2$Date, '%Y-%m-%d')
  S2 <- dplyr::arrange(S2,Date)

  Cropuse<-fields[which(fields$Only_ID==IDnow),]
  calendar2015<-legend[legend$CROP==Cropuse[,"crop2015"],]
  calendar2016<-legend[legend$CROP==Cropuse[,"crop2016"],]
  calendar2017<-legend[legend$CROP==Cropuse[,"crop2017"],]
  calendar2018<-legend[legend$CROP==Cropuse[,"crop2018"],]
  calendar2019<-legend[legend$CROP==Cropuse[,"crop2019"],]
  calendar2020<-legend[legend$CROP==Cropuse[,"crop2020"],]
  
  #2015
  date_start_2015 <- as.POSIXct(paste((2015+calendar2015$start_year),"-",calendar2015$start_month,"-",calendar2015$start_day,sep=""))
  date_end_2015 <- as.POSIXct(paste((2015+calendar2015$stop_year),"-",calendar2015$stop_month,"-",calendar2015$stop_day,sep=""))
  event_2015 <- calendar2015$cropname
  int_2015 <- interval(date_start_2015, date_end_2015) 
  event_2015 <- calendar2015$cropname
  #2016
  date_start_2016 <- as.POSIXct(paste((2016+calendar2016$start_year),"-",calendar2016$start_month,"-",calendar2016$start_day,sep=""))
  date_end_2016 <- as.POSIXct(paste((2016+calendar2016$stop_year),"-",calendar2016$stop_month,"-",calendar2016$stop_day,sep=""))
  event_2016 <- calendar2016$cropname
  int_2016 <- interval(date_start_2016, date_end_2016) 
  #2017
  date_start_2017 <- as.POSIXct(paste((2017+calendar2017$start_year),"-",calendar2017$start_month,"-",calendar2017$start_day,sep=""))
  date_end_2017 <- as.POSIXct(paste((2017+calendar2017$stop_year),"-",calendar2017$stop_month,"-",calendar2017$stop_day,sep=""))
  event_2017 <- calendar2017$cropname
  int_2017 <- interval(date_start_2017, date_end_2017) 
  #2018
  date_start_2018 <- as.POSIXct(paste((2018+calendar2018$start_year),"-",calendar2018$start_month,"-",calendar2018$start_day,sep=""))
  date_end_2018 <- as.POSIXct(paste((2018+calendar2018$stop_year),"-",calendar2018$stop_month,"-",calendar2018$stop_day,sep=""))
  event_2018 <- calendar2018$cropname
  int_2018 <- interval(date_start_2018, date_end_2018) 
  #2019
  date_start_2019 <- as.POSIXct(paste((2019+calendar2019$start_year),"-",calendar2019$start_month,"-",calendar2019$start_day,sep=""))
  date_end_2019 <- as.POSIXct(paste((2019+calendar2019$stop_year),"-",calendar2019$stop_month,"-",calendar2019$stop_day,sep=""))
  event_2019 <- calendar2019$cropname
  int_2019 <- interval(date_start_2019, date_end_2019) 
  #2020
  date_start_2020 <- as.POSIXct(paste((2020+calendar2020$start_year),"-",calendar2020$start_month,"-",calendar2020$start_day,sep=""))
  date_end_2020 <- as.POSIXct(paste((2020+calendar2020$stop_year),"-",calendar2020$stop_month,"-",calendar2020$stop_day,sep=""))
  event_2020 <- calendar2020$cropname
  int_2020 <- interval(date_start_2020, date_end_2020) 

  
  time<-c(int_2015,int_2016,int_2017,int_2018,int_2019,int_2020)
  
  date_list<-list(date_end_2015,date_start_2016,date_end_2016,date_start_2017,date_end_2017,date_start_2018,
                  date_end_2018,date_start_2019,date_end_2019,date_start_2020)
  
  #intervals
  out1<-interval(date_end_2015,date_start_2016) 
  out2<-interval(date_end_2016,date_start_2017) 
  out3<-interval(date_end_2017,date_start_2018) 
  out4<-interval(date_end_2018,date_start_2019) 
  out5<-interval(date_end_2019,date_start_2020) 


  Inter[i,46]<-IDnow
  
 #read in seasons files here
  season<-read.csv(paste0(".../Process1_NDVI_season/",IDnow,"seasons.csv"))
  nseason<-nrow(season)
  nseason
      
      
  inter_list<-list(out1,out2,out3,out4,out5)
  cc_season<-NA
  bs_season<-NA
  
  
  for (j in 1:5){
    a<-NULL
    out<-inter_list[[j]]
    out
    try(a<-seq(date_list[[j*2-1]],date_list[[j*2]], by=60*60*24),silent = TRUE)
    days<- length(format(a, "%d"))
    days 
    Inter[i,(9*j-8)]<-days
    
    #calculate mean NDVI 
    S2_out<-S2[S2$Date %within% out,]
    NDVImean<-mean(S2_out[,c("NDVI")]) 
    Inter[i,(9*j-7)]<- NDVImean
    
    #add precipitation data and remove points with high precipitation
    try(  S2_out$Prec<-NA, silent = T)
    Precday<-4
    try(  for (n in 1: nrow(S2_out)){
      S2_out$Prec[n]<-sum(prec[prec$Date %within% 
                            interval( S2_out[n,]$Date-Precday, S2_out[n,]$Date),2])
    },silent = T)
    
    
    S2_out_norain<-S2_out[which(S2_out$Prec<2.5),] #remove high precipitation event
    
    NBR2mean_norain<-mean(S2_out_norain[,c("NBR2")])
    
    Inter[i,(9*j-6)]<-NBR2mean_norain

    all<-0
    line<-0
    #compare out time with seasons one by one
    for (z in 1:nrow(season)){
      seasonuse<-interval(season[z,"beg"], season[z,"end"]) 
      #seasonuse@tzone<-"CET"
      #out@tzone<-"CET"
      
      seasonlength<-season[z,"len"]
      overlay<-day(as.period(lubridate::intersect(seasonuse, out), "days"))
      overlay/seasonlength
    
      if (is.na(overlay/seasonlength)) {
        num<-0
        count<-0
      }else{
        if(overlay/seasonlength <0.8){
          num<-0
          count<-0
        }else{
          num<-z 
          count<-1
          cc_season<-c(cc_season,num)
        }
      }
      line<-num+line
      all<-count+all
    } 
    
    #decide whether there is a cover crop
    if(days<30){
      Inter[i,(9*j-5):(9*j)]<-0 
    }else{ 
      if(all>1){    #A COVER CROP cover all inter time
      Inter[i,(9*j-5)]<-1 
      Inter[i,(9*j-4)]<- days
      Inter[i,(9*j-3)]<- as.character(date_list[[j*2-1]])
      Inter[i,(9*j-2)]<- as.character(date_list[[j*2]])
      Inter[i,(9*j-1)]<- 0
      Inter[i,(9*j)]<- 0
    }else{
      if(is.na(NDVImean)|is.na(NBR2mean_norain)){
        Inter[i,(9*j-5):(9*j)]<-0 
      }else{ 
        if(all==0 & NDVImean<0.25 & NBR2mean_norain<0.09){  #bare soil or residue
          Inter[i,(9*j-5)]<-0 
          Inter[i,(9*j-4)]<- 0
          Inter[i,(9*j-3)]<- 0
          Inter[i,(9*j-2)]<- 0
          Inter[i,(9*j-1)]<-days
          Inter[i,(9*j)]<-0
          bs_season<-c(bs_season,j)
        }else{
          if(all==0){
            Inter[i,(9*j-5)]<-0 
            Inter[i,(9*j-4)]<- 0
            Inter[i,(9*j-3)]<- 0
            Inter[i,(9*j-2)]<- 0
            Inter[i,(9*j-1)]<-0
            Inter[i,(9*j)]<-days
          
          }else{     #A COVER CROP   
            #calculte remain part no rain point (from S2 data perspective)
            remain<-S2_out[which(!S2_out$Date %within% interval(season[line,"beg"],season[line,"end"])),]
           
            remain_norain<-remain[which(remain$Prec<2.5),] #remove high precipitation event
            
            NDVImean_remain<-mean(remain_norain$NDVI)
            NBR2mean_remain<-mean(remain_norain$NBR2)
            
            #calculte remain part (from date perspective)
            overlay<-day(as.period(lubridate::intersect(out, interval(season[line,"beg"],season[line,"end"])), "days"))
            days_remain<-days-overlay
      
            if ( all==1 & days_remain<30 |   all==1 &is.na(NDVImean_remain)  ) {  #only CC
       
              Inter[i,(9*j-5)]<-1 
              Inter[i,(9*j-4)]<- season[line,"len"]
              Inter[i,(9*j-3)]<- season[line,"beg"]
              Inter[i,(9*j-2)]<- season[line,"end"]
              Inter[i,(9*j-1)]<-0
              Inter[i,(9*j)]<-0
            
        }else{
          if(all==1 & days_remain>30 & NDVImean_remain<0.25 & NBR2mean_remain<0.09){    #cover crop and bare soil
            Inter[i,(9*j-5)]<-1 
            Inter[i,(9*j-4)]<- season[line,"len"]
            Inter[i,(9*j-3)]<- season[line,"beg"]
            Inter[i,(9*j-2)]<- season[line,"end"]
            Inter[i,(9*j-1)]<-days_remain 
            Inter[i,(9*j)]<-0
          
          bs_season<-c(bs_season,j)
      }else{
        Inter[i,(9*j-5)]<-1 
        Inter[i,(9*j-4)]<- season[line,"len"]
        Inter[i,(9*j-3)]<- season[line,"beg"]
        Inter[i,(9*j-2)]<- season[line,"end"]
        Inter[i,(9*j-1)]<-0
        Inter[i,(9*j)]<-days_remain 

        #no CC calculate outer
   }
     }    
      }    
      }
    }
  }
  }
  }  
  beg1<-season$beg[1]
  beg2<-season$beg[2]
  beg3<-season$beg[3]
  beg4<-season$beg[4]
  beg5<-season$beg[5]
  beg6<-season$beg[6]
  beg7<-season$beg[7]
  beg8<-season$beg[8]
  beg9<-season$beg[9]
  beg10<-season$beg[10]
  beg11<-season$beg[11]
  beg12<-season$beg[12]
  beg13<-season$beg[13]
  beglist<-list(beg1,beg2,beg3,beg4,beg5,beg6,beg7,beg8,beg9,beg10,beg11,beg12,beg13)
  
  end1<-season$end[1]
  end2<-season$end[2]
  end3<-season$end[3]
  end4<-season$end[4]
  end5<-season$end[5]
  end6<-season$end[6]
  end7<-season$end[7]
  end8<-season$end[8]
  end9<-season$end[9]
  end10<-season$end[10]
  end11<-season$end[11]
  end12<-season$end[12]
  end13<-season$end[13]
  endlist<-list(end1,end2,end3,end4,end5,end6,end7,end8,end9,end10,end11,end12,end13)
  
  
  timedata<-xts( S2[,2:3],as.Date( S2$Date,format='%Y-%m-%d'))  #transfer to time format
  p <- dygraph(timedata) %>%
    dyOptions(drawPoints = TRUE,pointSize = 3,labelsUTC = TRUE,drawGrid = TRUE) %>%
    dyRangeSelector() %>%
    dyCrosshair(direction = "vertical") %>%
    dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
    dyRoller(rollPeriod = 1)%>%
    
    ##seasons
    dyShading(from = beg1, to = end1, color = "#CED1CF")%>%
    dyShading(from = beg2, to = end2, color = "#EEEFEE")%>%
    dyShading(from = beg3, to = end3, color = "#CED1CF")%>%
    dyShading(from = beg4, to = end4, color = "#EEEFEE")%>%
    dyShading(from = beg5, to = end5, color = "#CED1CF")%>%
    dyShading(from = beg6, to = end6, color = "#EEEFEE")%>%
    dyShading(from = beg7, to = end7, color =" #CED1CF")%>%
    dyShading(from = beg8, to = end8, color = "#EEEFEE")%>%
    dyShading(from = beg9, to = end9, color = "#EEEFEE")%>%
    dyShading(from = beg10, to = end10, color = "#EEEFEE")%>%
    dyShading(from = beg11, to = end11, color = "#EEEFEE")%>%
    dyShading(from = beg12, to = end12, color = "#EEEFEE")%>%
    dyShading(from = beg13, to = end13, color = "#EEEFEE")%>%
    ##Bare soil
    dyShading(from = date_list[[(2*bs_season[2]-1)]], to = date_list[[(2*bs_season[2])]], color = "#E1C16E")%>%
    dyShading(from = date_list[[(2*bs_season[3]-1)]], to = date_list[[(2*bs_season[3])]], color = "#E1C16E")%>%
    dyShading(from = date_list[[(2*bs_season[4]-1)]], to = date_list[[(2*bs_season[4])]], color = "#E1C16E")%>%
    dyShading(from = date_list[[(2*bs_season[5]-1)]], to = date_list[[(2*bs_season[5])]], color = "#E1C16E")%>%
    ##Cover crop
    dyShading(from = beglist[[cc_season[1]]], to = endlist[[cc_season[1]]], color = "#CCEBD6")%>%
    dyShading(from = beglist[[cc_season[2]]], to = endlist[[cc_season[2]]], color ="#CCEBD6")%>%
    dyShading(from = beglist[[cc_season[3]]], to = endlist[[cc_season[3]]], color ="#CCEBD6")%>%
    dyShading(from = beglist[[cc_season[4]]], to = endlist[[cc_season[4]]], color ="#CCEBD6")%>%
    dyShading(from = beglist[[cc_season[5]]], to = endlist[[cc_season[5]]], color ="#CCEBD6")%>%
    ##event
    dyEvent(date_start_2015, paste0(event_2015, " start"), labelLoc = "bottom") %>%
    dyEvent(date_end_2015, paste0(event_2015, " end"), labelLoc = "bottom") %>%
    # dyEvent(date2.1,"end tillage", labelLoc = "bottom") %>%
    dyEvent(date_start_2016, paste0(event_2016, " start"), labelLoc = "bottom") %>%
    dyEvent(date_end_2016, paste0(event_2016, " end"), labelLoc = "bottom")%>%
    #dyEvent(date4.1, "end tillage", labelLoc = "bottom")%>%
    dyEvent(date_start_2017, paste0(event_2017, " start"), labelLoc = "bottom") %>%
    dyEvent(date_end_2017, paste0(event_2017, " end"), labelLoc = "bottom")%>%
    dyEvent(date_start_2018, paste0(event_2018, " start"), labelLoc = "bottom") %>%
    dyEvent(date_end_2018, paste0(event_2018, " end"), labelLoc = "bottom")%>%
    dyEvent(date_start_2019, paste0(event_2019, " start"), labelLoc = "bottom") %>%
    dyEvent(date_end_2019, paste0(event_2019, " end"), labelLoc = "bottom")%>%
    dyEvent(date_start_2020, paste0(event_2020, " start"), labelLoc = "bottom") %>%
    dyEvent(date_end_2020, paste0(event_2020, " end"), labelLoc = "bottom")
  #dyEvent(date6.1, "end tillage", labelLoc = "bottom")
  # dyEvent("2018-8-30", "maybe end tillage", labelLoc = "bottom")
  
  
  p
  
  saveWidget(p,file ="ab.html", selfcontained = TRUE, libdir = NULL)
  webshot(url = "ab.html",file=paste0(".../Picture/",IDnow,"NDVIseasonCrop.jpg"))
  
  
  
} 

write.csv(Inter,"CC_BS_length.csv")
beep()


