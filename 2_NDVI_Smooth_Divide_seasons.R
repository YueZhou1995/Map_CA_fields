#####this code is for extracting seasons and phenology based on time-series values using phenofit package

#####detailed index/parameters can be found from https://cran.r-project.org/web/packages/phenofit/phenofit.pdf
#https://cran.r-project.org/web/packages/phenofit/vignettes/phenofit_CA-NS6.html

library(stringr)
library(phenofit)
library(beepr)
path_data <- "..."
setwd(path_data)
NDVI<-read.csv("NDVI_example.csv")   # you can find a csv example in the repository

#add a new path for this step
path_new <- ".../Process1_NDVI_season"
setwd(path_new)

c<-str_extract_all(colnames(NDVI[6:ncol(NDVI)]),"[0-9]+")
c<-as.data.frame(c)
b<-as.integer(c[1,])
colnames(NDVI)[6:ncol(NDVI)]<-b


#divide into seasons
for(i in 6:ncol(NDVI)){
  NDVIuse<-NDVI[,c(2,i)]
  IDfind<-str_extract_all(colnames(NDVIuse)[2],"[0-9]+")
  #NDVIuse<-NDVI[,c("Date","year","month","yday","246")]
  IDfind<-as.integer(IDfind)
  IDfind

  data<-NDVIuse
  colnames(data)<-c("Date","NDVI")
  data[,1]<-as.Date(data[,1],format='%Y-%m-%d')
  data <- data[complete.cases(data),]  #remove NA
  
  if(nrow(data)<60){  
    print(IDfind)
  }else{
  data <- dplyr::arrange(data, Date)
  nptperyear<-25    # How many points for a single year
  #check input data
   l <- check_input(data$Date, data$NDVI,nptperyear=nptperyear,ymin=0.05,
                    alpha = 0.01,wmin=0.01)  #alpha: percentage min/max data  wmin:min weight of data
   #plot input NDVI data
   plot_input(l)
   # detect season  
   wFUN <- "wTSM"  #could be one of `wTSM`, 'wBisquare', `wChen` and `wSELF`. # Weights updating function
   brks2 <- season_mov(l, list(rFUN = "smooth_wWHIT", wFUN = wFUN, ypeak_min = 0.6 ,lambda = 0.1,
                               r_max=0.35, r_min = 0.05,
                               iters =3,verbose = FALSE)) 
   #iters: number of rough fitting iterations
   #rFUN : character (default smooth_wWHIT), the name of rough curve fitting function
   #lambda : double (default NULL), the smoothing parameter of smooth_wWHIT()
   #ypeak_min:min of y peak
   #r_max and r_min are used to eliminate fake peaks and troughs.  
   #height difference from the peak to the left- and right-hand troughs.bigger than r min
   plot_season (l, brks2)
   a<-brks2$dt
   c<-brks2$fit
   #output smoothed NDVI and divided seasons 
   write.csv(c,paste0(IDfind,"smoothNDVI.csv"))
   write.csv(a,paste0(IDfind,"seasons.csv"))
   #output plots
   dev.off()
   jpeg(file = paste0(IDfind,"NDVIseason.jpg"),width=1000,height=500)
   plot_season(l, brks2)
   dev.off() 
  }  
}
beep()   
