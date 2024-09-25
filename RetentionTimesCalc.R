#### calcuation of reservoir volume from bathymetry data 
####

# install.packages('devtools')
# install.packages("remotes")
# remotes::install_github("nbuccola/w2r")

# library(w2r)
library(gdata)
library(dplyr)
library(tidyr)
library(stringr)
library(foreach)
library(rLakeAnalyzer)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(magick)
library(processx)

# Load Custom Functions
source("CEQUALW2_function.R")

# locations of W2 model directories for all models in analysis
outfiles<-c("C:/...../Detroit Reservoir_Modified",
            "C:/...../Long Lake",
            "C:/...../DeGray Reservoir Modified",
            "C:/...../Model_files_for_modified",
            "C:/...../2018",
            "C:/...../2017_Milton",
            "C:/...../2017_Berlin",
            "C:/...../DWR",
            "C:/...../2006-08_Foster_focus",
            "C:/...../2006-08_GreenPeter_focus")


outfiles
for (z in c(1:length(outfiles))){
  
  
  dirfile<-outfiles[z]

  detach(W2inputs)
  W2inputs<-read.csv(paste0(dirfile,"/R_W2_config.csv"))
  if("Jan1date_number" %in% colnames(W2inputs)){
    Jan1date_number<-W2inputs$Jan1date_number[1]
  }else{
    Jan1date_number<-NULL
  }
  upstream_order<- W2inputs$upstream_order
  W2inputs<-W2inputs[1,-12]
  attach(W2inputs)
  
  
  
  # loop output options
  # seasonal window selecting
  # examples c(31.5)  or c(1.5:60.5)
  winter_window <- c(1.5:60.5) # Jan - Feb
  # examples c(213.5)  or c(1.5:60.5)
  summer_window <- c(182.5:243.5) # July - Aug
  
  if(is.numeric(Jan1date_number)){
    winter_window<-Jan1date_number -1+winter_window # adjusting day window for models that are indexed off of previous years
    summer_window<-Jan1date_number -1+summer_window # adjusting day window for models that are indexed off of previous years
  }
  
  #### Bathy File Input ####
  #Check if CE-QUAL-W2 is newer than Version 3.7
  
  BathyFile_loc<-list.files(dirfile, pattern = paste0(stringr::str_squish(basename(BathyFile)),"$"), recursive=TRUE, full.names = TRUE)
  if(length(BathyFile_loc)!=1){print("More than one file included in location")}
  
  BathyHead<-ReadBathyH(BathyFile_loc)
  BathyWidth<-ReadBathyW(BathyFile_loc)
  # BathyWidth<-na.omit(BathyWidth) #### long lake, wa
  # BathyHead<-BathyHead[c(1:37),] #### long lake, wa
  
  if(length(BathyHead$Segments)!=length(upstream_order)){
    print("Upstream order and number of segments do not match!")
  }
  BathyHead<-na.omit(BathyHead)
  
  
  #### iteration loop planning ####
  
  
  # initial or lowest water elevation
  base_wl<-read.csv(paste0(dirfile,"/",WaterLevel))
  # upstream_order
  # as.numeric(gsub("\\D", "", colnames(base_wl)))
  # filter to segments within matching water body (important for multiple WB models)
  wl_filter_list<-as.numeric(gsub("\\D", "", colnames(base_wl))) %in% upstream_order
  mean_elev<-mean(rowMeans(base_wl[,wl_filter_list],na.rm = TRUE))
  
  
  
  
  #area minimum at lowest water level
  
  layer_activity<-BathyWidth %>%
    dplyr::select(-c(LAYERH,K)) %>%
    rowSums(na.rm = TRUE)
  bottom_layer<-(as.numeric(names(tail(layer_activity[layer_activity>0],n=1)))+1)
  
  # adding elevation column to bathymetry widths
  BathyWidth$Elev_diff <- ifelse(BathyWidth$K==bottom_layer,0,
                                 ifelse(BathyWidth$K<bottom_layer,BathyWidth$LAYERH,-BathyWidth$LAYERH))
  BathyWidth$Elevation <- NULL
  
  for (i in BathyWidth$K){
    BathyWidth$Elevation[i]<-sum(BathyWidth$Elev_diff[c(i:bottom_layer)])+EBOT
  }
  ### area of step/% change FPV
  
  #identify layer closest to mean surface
  mean_surface_layer<-BathyWidth$K[which.min(abs(mean_elev-BathyWidth$Elevation))]
  
  full_volume_set<-c(mean_surface_layer:dim(BathyWidth)[1])
  # extract widthds for lowest water layer for FPV
  layers_in_volume<-BathyWidth[full_volume_set,-which(names(BathyWidth) %in% c("LAYERH","K","Elev_diff","Elevation"))]
  layer_heights<-BathyWidth[full_volume_set,which(names(BathyWidth) == "Elev_diff")]
  layer_volume<-sweep(layers_in_volume, MARGIN=1, layer_heights, `*`)*rep(BathyHead$DLX,each=nrow(layers_in_volume))
  res_volume_m3<-sum(layer_volume, na.rm = TRUE) # "max FPV area"
  res_volume_m3
  
  
  #importing baseline reference data
  ref_outflow_q_raw_file<-list.files(paste0(dirfile,"/"), pattern = paste0("qwo_",outflow_seg), recursive=TRUE, full.names = TRUE)[1]
  ref_outflow_q_raw<-readLines(ref_outflow_q_raw_file)
  ref_outflow_q_data_row<-grep("^JDAY",ref_outflow_q_raw)
  ref_outflow_q<-read.csv(ref_outflow_q_raw_file, skip=(ref_outflow_q_data_row), row.names = NULL ,header=FALSE)
  
  ref_outflow_q_df<-data.frame(JDAY=ref_outflow_q[,1], meanQ=ref_outflow_q[,2])
  
  ref_outflow_q_dailyMean<-ref_outflow_q_df %>%
    mutate(Date=as.integer(JDAY)) %>%
    group_by(Date) %>%
    summarise(meanDailyQ = mean(meanQ)) %>%
    pull(meanDailyQ) %>%
    mean()
  
  
  (res_volume_m3/ref_outflow_q_dailyMean) # refresh time in seconds
  retention_days<-(res_volume_m3/ref_outflow_q_dailyMean)/86400 # refresh time in days
  
  
  if(z==1){
    retention_out<-NULL
    retention_out<-data.frame(res_index=z, wtr_body=waterbody_name, mean_flow=ref_outflow_q_dailyMean, vol =res_volume_m3 ,renew=retention_days)
  }else{
    retention_out<-rbind(retention_out,data.frame(res_index=z, wtr_body=waterbody_name, mean_flow=ref_outflow_q_dailyMean, vol =res_volume_m3 ,renew=retention_days))
  }
}

retention_out

write.csv(retention_out, "C:/...../flushing_rates.csv")


#### verticel profile plots ####

library(extrafont)
# font_import()
loadfonts(device = "all")
fonts()

My_Theme = theme( axis.text = element_text( size = 16 , family = "Calibri"),
                  axis.text.x = element_text( size = 16 , family = "Calibri"),
                  axis.title = element_text(size=20, family = "Calibri"),
                  legend.title  = element_text( size = 16, face = "bold" , family = "Calibri"),
                  legend.text = element_text(size=14, family = "Calibri"),
                  strip.text = element_text(size = 20, family = "Calibri"))

outfiles

dir.create("C:/...../ProfilePlot")
graph_out_file<-"C:/...../ProfilePlot"
for (z in c(1:length(outfiles))){
  
  
  dirfile<-outfiles[z]
  
  detach(W2inputs)
  W2inputs<-read.csv(paste0(dirfile,"/R_W2_config.csv"))
  if("Jan1date_number" %in% colnames(W2inputs)){
    Jan1date_number<-W2inputs$Jan1date_number[1]
  }else{
    Jan1date_number<-NULL
  }
  upstream_order<- W2inputs$upstream_order
  W2inputs<-W2inputs[1,-12]
  attach(W2inputs)
  
  
  
  # loop output options
  # seasonal window selecting
  # examples c(31.5)  or c(1.5:60.5)
  winter_window <- c(1.5:60.5) # Jan - Feb
  # examples c(213.5)  or c(1.5:60.5)
  summer_window <- c(182.5:243.5) # July - Aug
  
  if(is.numeric(Jan1date_number)){
    winter_window<-Jan1date_number -1+winter_window # adjusting day window for models that are indexed off of previous years
    summer_window<-Jan1date_number -1+summer_window # adjusting day window for models that are indexed off of previous years
  }

  profile_data<-read.csv(paste0(dirname(dirfile),"/iterate_temp/",basename(dirfile),"_profile.csv"))
  
  unique(profile_data$iteration)
  select_levels<-as.vector(round(quantile(unique(profile_data$iteration), c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)),0))
  select_levels
  
  str(profile_data)
  profile_data %>%
    mutate(`Perc. FPV`= 100*(iteration/max(iteration)),
           season = ifelse(Date %in% as.integer(summer_window),"Summer",
                           ifelse(Date %in% as.integer(winter_window),"Winter",NA))) %>%
    filter(!is.na(season)) %>%
    filter(iteration %in% select_levels) %>%
    group_by(`Perc. FPV`,season,LayerUnderSurface) %>%
    summarise(Temp = mean(MeanTemp, na.rm = TRUE),
              AveDepth = -mean(Depth, na.rm = TRUE)) %>% #View()
    ggplot(., aes(x=Temp, y = AveDepth, group = `Perc. FPV`))+
    facet_wrap(~season, scales = "free")+
    geom_line(aes(color=`Perc. FPV`), linewidth=1.15, orientation = "y")+
    scale_x_continuous(limits = c(0,NA),name = "Temperature (C)")+
    scale_y_continuous(labels = abs, name = "Depth (m)")+
    ggtitle(paste0(waterbody_name))+theme_bw()+
    My_Theme#+labs(color='Iteration')
    
  dir.create(paste0(dirname(dirfile),"/iterate_temp/",basename(dirfile),"/ProfilePlot"))
  ggsave(paste0(dirname(dirfile),"/iterate_temp/",basename(dirfile),"/ProfilePlot","/fig_profile.png"), dpi = 200, width = 16, height = 8)
  ggsave(paste0(graph_out_file,"/",z,"_fig_profile.png"), dpi = 200, width = 16, height = 8)
  
  
  }
