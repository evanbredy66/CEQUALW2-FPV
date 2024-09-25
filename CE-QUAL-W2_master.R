# R version 4.2.2

# loading librarys
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

sI <- sessionInfo()
sI$otherPkgs$gdata$Version
sI$otherPkgs$dplyr$Version
sI$otherPkgs$tidyr$Version
sI$otherPkgs$stringr$Version
sI$otherPkgs$foreach$Version
sI$otherPkgs$rLakeAnalyzer$Version
sI$otherPkgs$lubridate$Version
sI$otherPkgs$ggplot2$Version
sI$otherPkgs$gridExtra$Version
sI$otherPkgs$magick$Version
sI$otherPkgs$processx$Version

# Version of dependant libraries
# > sI$otherPkgs$gdata$Version
# [1] "2.18.0.1"
# > sI$otherPkgs$dplyr$Version
# [1] "1.0.10"
# > sI$otherPkgs$tidyr$Version
# [1] "1.3.0"
# > sI$otherPkgs$stringr$Version
# [1] "1.5.0"
# > sI$otherPkgs$foreach$Version
# [1] "1.5.2"
# > sI$otherPkgs$rLakeAnalyzer$Version
# [1] "1.11.4.1"
# > sI$otherPkgs$lubridate$Version
# [1] "1.9.2"
# > sI$otherPkgs$ggplot2$Version
# [1] "3.4.2"
# > sI$otherPkgs$gridExtra$Version
# [1] "2.3"
# > sI$otherPkgs$magick$Version
# [1] "2.8.1"
# > sI$otherPkgs$processx$Version
# [1] "3.8.3"


# Load Custom Functions
source("CEQUALW2_functions.R")
source("schmidt.stability.custom.R")



#### USER: Data entry from W2_con ####

#NOTE: Check "W2_Model_LoopNotes.txt" file to adjust 'W2_con' file
#Local W2 Files
dirfile<-"C:/...../Directory_of_W2_Model"


# Read in filenames for W2 model
# template of R_W2_config.csv located in GitHub repo

detach(W2inputs)
W2inputs<-read.csv(paste0(dirfile,"/R_W2_config.csv"))
if("Jan1date_number" %in% colnames(W2inputs)){
  Jan1date_number<-W2inputs$Jan1date_number[1]
}else{
  Jan1date_number<-NULL
}
# upstream order lists the segments of W2 model and in what order they will be filled with FPV
upstream_order<- W2inputs$upstream_order
W2inputs<-W2inputs[1,-12]
attach(W2inputs)



# loop output options
# seasonal window selecting via Julian Dates
# examples c(31.5)  or c(1.5:60.5)
winter_window <- c(1.5:60.5) # Jan - Feb
# examples c(213.5)  or c(1.5:60.5)
summer_window <- c(182.5:243.5) # July - Aug

if(is.numeric(Jan1date_number)){
winter_window<-Jan1date_number -1+winter_window # adjusting day window for models that are indexed off of previous years
summer_window<-Jan1date_number -1+summer_window # adjusting day window for models that are indexed off of previous years
}



# Assigning location of W2 executable files
# Script written with 4.5, 4.22, and 4.2.USGS as possible versions
# NOTE: 4.2.USGS models must have executable files within model directory since it is not command line aware

if(W2vers == 4.22) {
  w2_exe_pre<-"C:\\.....\\preW2-v4_64.exe"
  w2_exe<-"C:\\.....\\w2_v4_64.exe"
}
if(W2vers == 4.5) {
  w2_exe_pre<-"C:\\.....\\preW2-v45_64.exe"
  w2_exe<-"C:\\.....\\w2_v45_64.exe"
}



 #### run baseline W2 model ####
# Run model via CommandLine
# CLOSEC to ON in w2_con.npt -> close dialog box

if(is.numeric(W2vers)){
system(sprintf(paste0(w2_exe_pre,' "',gsub("/", "\\", dirfile, fixed = TRUE),'"')), 
        invisible=FALSE, wait=TRUE, timeout = 15)
# MUST CHECK ON OUTPUTS OF PROCESS before next

system(sprintf(paste0(w2_exe,' "',gsub("/", "\\", dirfile, fixed = TRUE),'"')),
       invisible=FALSE, wait=TRUE, timeout = (8*60))
}



if(W2vers == "4.2.USGS") {
  W2_exe<-list.files(dirfile, pattern = ".exe$", full.names = TRUE)
  processx::run(W2_exe, 
                wd=dirfile)
  }


#### copy all files to new folder ####
# iterate folder located in same subdirectory as model directory

iterate_file<-paste0(dirname(dirfile),"/iterate_temp")
if(dir.exists(iterate_file)){print("Directory Already Created")}else{
dir.create(paste0(dirname(dirfile),"/iterate_temp"))
}

file.copy(paste0(dirfile,"/"), paste0(iterate_file,"/"), recursive=TRUE, overwrite = TRUE)

iterate_file1<-paste0(iterate_file,"/",basename(dirfile))





#### reading in model inputs ####

## Shade File Input

stringr::str_squish(basename(shadefile))
shadefile_loc<-list.files(iterate_file1, pattern = paste0(stringr::str_squish(basename(shadefile)),"$"), recursive=TRUE, full.names = TRUE)
if(length(shadefile_loc)!=1){print("More than one file included in location")}

if(str_detect(shadefile_loc, ".csv$")){
  shadefile_type<-"CSV"
  shade<-ReadShade.CSV(shadefile_loc)
  
}
if(str_detect(shadefile_loc, ".npt$")){
  shadefile_type<-"NPT"
  shade<-ReadShade.NPT(shadefile_loc)
}

colnames(shade)[2]<-"DynSh"
shade$DynSh<-shade$DynSh
str(shade)


## WSC File Input
WSCfile_loc<-list.files(iterate_file1, pattern = paste0(stringr::str_squish(basename(WSCfile)),"$"), recursive=TRUE, full.names = TRUE)
if(length(WSCfile_loc)!=1){print("More than one file included in location")}
# WSCfile_loc<-WSCfile_loc[2]

if(str_detect(shadefile_loc, ".csv$")){
  WSCfile_type<-"CSV"
  WSC<-ReadWSC.CSV(WSCfile_loc)
  
}
if(str_detect(shadefile_loc, ".npt$")){
  WSCfile_type<-"NPT"
  WSC<-ReadWSC.NPT(WSCfile_loc)
}

# ## Bathy File Input
# Check if CE-QUAL-W2 is newer than Version 3.7
# There was a change in bathymetry format from that version

BathyFile_loc<-list.files(iterate_file1, pattern = paste0(stringr::str_squish(basename(BathyFile)),"$"), recursive=TRUE, full.names = TRUE)
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
base_wl<-read.csv(paste0(iterate_file1,"/",WaterLevel))
# upstream_order
# as.numeric(gsub("\\D", "", colnames(base_wl)))
# filter to segments within matching water body (important for multiple WB models)
wl_filter_list<-as.numeric(gsub("\\D", "", colnames(base_wl))) %in% upstream_order
lowest_elev<-rowMeans(base_wl[,wl_filter_list],na.rm = TRUE)[which.min(rowMeans(base_wl[,wl_filter_list],na.rm = TRUE))]



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

#identify layer with FPV area shading
install_FPV_layer<-BathyWidth$K[which.min(abs(lowest_elev-BathyWidth$Elevation))]
# extract widthds for lowest water layer for FPV
install_FPV_width<-BathyWidth[install_FPV_layer,-which(names(BathyWidth) %in% c("LAYERH","K","Elev_diff","Elevation"))]
install_FPV_area<-BathyHead$DLX*install_FPV_width
sum(install_FPV_area) # "max FPV area"


# determining unit of iteration area

# TO SET #
FPV_perc = 0.05 # 5% percent
FPV_area = 500000 # 0.5 square km
summary(t(install_FPV_area))

# either 5% or 0.5 sq km, whichever is smaller as increment
# smaller percentages or areas will create more iterations of the model
area_increment<-min(sum(install_FPV_area)*FPV_perc,FPV_area)
# area_increment<-sum(install_FPV_area)*FPV_perc

other_wb_segment_skip<-(min(upstream_order)-1)
colnames(install_FPV_area)<-as.integer(as.numeric(colnames(install_FPV_area))+other_wb_segment_skip)

#### creating sets of coverage for loop ####
scalars<-FPV_segment_calculate(install_FPV_area,area_increment = area_increment ,upstream_order = (upstream_order-other_wb_segment_skip) ,sorted_order = TRUE)

## non-segment specific shading
# scalars<-FPV_universal_calculate(install_FPV_area,area_increment = area_increment) 


# used to adjust full coverage shading (optional)
scalars_shade<-max_adjustment_scalar(scalars, max_perc = 100, perc.decimal = FALSE)
scalars_wsc<-max_adjustment_scalar(scalars, max_perc = 100, perc.decimal = FALSE)

# scalars_shade<-max_adjustment_scalar(scalars, max_perc = 73, perc.decimal = FALSE)
# scalars_wsc<-max_adjustment_scalar(scalars, max_perc = 23, perc.decimal = FALSE)

# https://doi.org/10.1038/s41598-023-34751-2
# measured irradiance reduction of 73% and wind reduction of 23%.


# wl_filter_list<-as.numeric(gsub("\\D", "", colnames(base_wl))) %in% upstream_order

#### adjust scalar for waterbodies ####

#### checking WSC and shade lengths to scalars
if(dim(scalars_wsc)[1]!=length(unique(WSC$Seg))){
  add_on_wsc_list<-unique(WSC$Seg[!(unique(WSC$Seg) %in% as.numeric(rownames(scalars_wsc)))])
  add_on_wsc<-data.frame(matrix(1,ncol = dim(scalars_wsc)[2], nrow = length(add_on_wsc_list)), row.names = c(add_on_wsc_list))
  colnames(add_on_wsc)<-colnames(scalars_wsc)
  scalars_wsc<-rbind(scalars_wsc,add_on_wsc)
  scalars_wsc<-scalars_wsc[order(as.numeric(row.names(scalars_wsc))), ]
  # WSC<-filter(WSC,Seg<=dim(scalars)[1])
  print("More WSC segments than included in Bathymetry; scalar adjusted to match")
}

if(dim(scalars_shade)[1]!=length(unique(shade[,1]))){
  add_on_shade_list<-unique(shade[!(unique(shade[,1]) %in% as.numeric(rownames(scalars_shade))),1])
  add_on_shade<-data.frame(matrix(1,ncol = dim(scalars_shade)[2], nrow = length(add_on_shade_list)), row.names = c(add_on_shade_list))
  colnames(add_on_shade)<-colnames(scalars_shade)
  scalars_shade<-rbind(scalars_shade,add_on_shade)
  scalars_shade<-scalars_shade[order(as.numeric(row.names(scalars_shade))), ]
  # shade<-filter_at(shade,1,all_vars(.<=dim(scalars)[1]))
  print("More shade segments than included in Bathymetry; scalar adjusted to matchh")
}


#### model iteration loop ####

##start loop
# i<- 1
# iteration<-10
# scalars[1,]

# creating directory for loop graphs to save
if(dir.exists(paste0(iterate_file1,"/Plot_save_Loop"))) {
  paste("Directory already exists")
}else{
  dir.create(paste0(iterate_file1,"/Plot_save_Loop"))
}

# creating directory fo W2L file to save
if(dir.exists(paste0(iterate_file1,"/W2L_Loop"))) {
  paste("Directory already exists")
}else{
  dir.create(paste0(iterate_file1,"/W2L_Loop"))
}

#### START LOOP ####
for(i in c(1:length(scalars[1,]))){
# for(i in c(1:3)){ #used for debugging
  iteration<-i
  
  ## resetting input objects
  shade_out<-shade
  WSC_out<-WSC

  ## modifying model input files
  shade_out$DynSh<-shade$DynSh*scalars_shade[,iteration]

  WSC_out$WSC<-WSC$WSC*rep(scalars_wsc[,iteration],length(unique(WSC$JDAY)))
  WSC_out$WSC<-pmax(WSC_out$WSC,0.01)
  ### TO DO: Edit functions to use "lookup" style multipliers

if(shadefile_type=="NPT"){
  WriteShade.NPT(shade_out, shadefile_loc, shadefile_loc)
}
if(shadefile_type=="CSV"){
  WriteShade.CSV(shade_out, shadefile_loc, shadefile_loc)
}

if(WSCfile_type=="NPT"){
  WriteWSC.NPT(WSC_out, WSCfile_loc, WSCfile_loc)
}
if(WSCfile_type=="CSV"){
  WriteWSC.CSV(WSC_out, WSCfile_loc, WSCfile_loc)
}

## test model function with modified inputs



#Check various "functions" turned on?
#Modify model outputs to match#


  if(W2vers == "4.2.USGS") {
    W2_exe<-list.files(iterate_file1, pattern = ".exe$", full.names = TRUE)
    processx::run(W2_exe, 
                  wd=iterate_file1)
  }else{
  
if(i==1){
system(sprintf(paste0(w2_exe_pre,' "',gsub("/", "\\", iterate_file1, fixed = TRUE),'"')),
       invisible=FALSE, wait=TRUE, timeout = 15)}
# iterate_file1
# MUST CHECK ON OUTPUTS OF PROCESS before next

system(sprintf(paste0(w2_exe,' "',gsub("/", "\\", iterate_file1, fixed = TRUE),'"')),
       invisible=FALSE, wait=TRUE, timeout = (8*60))
  }
#### create W2L loop backup ####
# copy .w2l file with new name
iteration

W2L_filename<-list.files(iterate_file1)[grep(".w2l$",list.files(iterate_file1))]

file.copy(paste0(iterate_file1,"/",W2L_filename),paste0(iterate_file1,"/W2L_Loop/",iteration,"_",W2L_filename) ,overwrite=TRUE)


#### Extract outputs to database for FPV% ####

iteration
iterate_file1
dirfile
iteration_area_FPV=min(iteration*area_increment,sum(install_FPV_area))
model_reservoir<-waterbody_name

# output structure with response variables
if(i==1){
collected_responses<-data.frame(model_reservoir=character(0),
                                Iteration=double(),
                                FPV_covered_area=double(),
                                Season=factor(levels = c("summer","winter")),
                                Location=factor(levels = c("outflow","center","full")),
                                ResType=factor(levels = c("percent_change","value")),
                                ResponseVar=factor(levels = c("schmidt.stability",
                                                              "temp.mean",
                                                              "temp.min",
                                                              "temp.max",
                                                              "do.mean",
                                                              "do.min",
                                                              "do.max",
                                                              "stratified.days",
                                                              "thermocline.depth",
                                                              "mean.surface.temp",
                                                              "warmwater.fish",
                                                              "coldwater.fish",
                                                              "mean.outflow.temp",
                                                              "range.outflow.temp")),
                                RespValue=double(),
                                stringsAsFactors=FALSE)
}

# 1

# NOTES
# If the SPRC was set to ‘ONV’ rather than ‘ON’, an additional output file is written that contains the volume 
# weighted temperature and water quality state and derived variables at the locations and times specified. This file 
# has the suffix ‘_volw.csv’ and is a comma delimited file. A typical output file for a simulation with the vertical, 
# volume-weighted temperature and water age is shown below.

# SPR
# ON
# SPR filename

#TSR: specific seg/layer through time (can be surface layer = 0)
#CSV for each location through time
# W2 setting


# change in depth profile attributes
# Change in max, min, and range of values
# Depth of greatest change
# Aug 1, Feb 1
# nearest dam profile & center res profile
# need to be IDed and entered in W2_con before start



SPR<-read.csv(paste0(iterate_file1,"/",OutputSPR))

seg_columns<-colnames(SPR)[grep("^Seg_",colnames(SPR))]
SPR_wide<- SPR %>%
  pivot_longer(all_of(seg_columns), names_to ="Seg", values_to = "Value") %>%
  mutate(
    Constituent= str_squish(Constituent),
    SegNum=as.numeric(gsub("\\D", "", Seg))
    ) %>%
  pivot_wider(names_from = "Constituent", values_from = "Value")  %>%
  group_by(Julian_day,SegNum) %>%
  mutate(LayerUnderSurface= rank(Depth)) 

# read in reference comparison
ref_SPR<-read.csv(paste0(dirfile,"/",OutputSPR))

ref_seg_columns<-colnames(ref_SPR)[grep("^Seg_",colnames(ref_SPR))]
ref_SPR_wide<- ref_SPR %>%
  pivot_longer(all_of(ref_seg_columns), names_to ="Seg", values_to = "Value") %>%
  mutate(
    Constituent= str_squish(Constituent),
    SegNum=as.numeric(gsub("\\D", "", Seg))
  ) %>%
  pivot_wider(names_from = "Constituent", values_from = "Value")  %>%
  group_by(Julian_day,SegNum) %>%
  mutate(LayerUnderSurface= rank(Depth)) 


## collecting annual temp profiles ##
AnnSurfaceTemp<-SPR_wide %>%   ### only surface layer for segments output in SPR
  mutate(Date= as.integer(Julian_day)) %>%
  filter(LayerUnderSurface==1) %>%
  mutate(Temperature = replace(Temperature, Temperature==-99, NA)) %>%
  group_by(Date) %>%
  summarise(MeanTemp = mean(Temperature,na.rm = TRUE),
            MinTemp = min(Temperature,na.rm = TRUE),
            MaxTemp = max(Temperature,na.rm = TRUE),
            Depth=mean(Depth))


# Extra Plots
{
# surface temp plot and profile + thermocline plot
AnnTempProfile<-SPR_wide %>%
  mutate(Date= as.integer(Julian_day)) %>%
  filter(SegNum==outflow_seg) %>%
  mutate(Temperature = replace(Temperature, Temperature==-99, NA)) %>%
  group_by(Date,LayerUnderSurface) %>%
  summarise(MeanTemp = mean(Temperature,na.rm = TRUE),
            Depth=mean(Depth)) %>%
  group_by(LayerUnderSurface) %>%
  mutate(MeanDepth = (mean(Depth)))
AnnThermocline<-AnnTempProfile%>%
  ungroup() %>%
  group_by(Date) %>%
  mutate(ThermoDepth = rLakeAnalyzer::thermo.depth(MeanTemp,Depth, Smin = 0.1))


# ref_AnnSurfaceTemp<-ref_SPR_wide %>%  ### only surface layer for segments output in SPR
#   mutate(Date= as.integer(Julian_day)) %>%
#   filter(LayerUnderSurface==1) %>%
#   mutate(Temperature = replace(Temperature, Temperature==-99, NA)) %>%
#   group_by(Date) %>%
#   summarise(MeanTemp = mean(Temperature,na.rm = TRUE),
#             MinTemp = min(Temperature,na.rm = TRUE),
#             MaxTemp = max(Temperature,na.rm = TRUE),
#             Depth=mean(Depth))
ref_AnnTempProfile<-ref_SPR_wide %>%
  mutate(Date= as.integer(Julian_day)) %>%
  filter(SegNum==outflow_seg) %>%
  mutate(Temperature = replace(Temperature, Temperature==-99, NA)) %>%
  group_by(Date,LayerUnderSurface) %>%
  summarise(MeanTemp = mean(Temperature,na.rm = TRUE),
            Depth=mean(Depth)) %>%
  group_by(LayerUnderSurface) %>%
  mutate(MeanDepth = (mean(Depth)))
ref_AnnThermocline<-ref_AnnTempProfile%>%
  group_by(Date) %>%
  mutate(ThermoDepth = thermo.depth(MeanTemp,Depth, Smin = 0.1))
# thermo.depth(ref_AnnThermocline$MeanTemp,ref_AnnThermocline$Depth)
# ts.thermo.depth(wtr, Smin = 0.1, na.rm = FALSE, ...)


if(i==1){
  vert_profile_output<-cbind(ref_AnnThermocline,
                             data.frame(iteration=rep(0,dim(ref_AnnThermocline)[1])))
  vert_profile_output<-rbind(vert_profile_output,
                             cbind(AnnThermocline,
                                   data.frame(iteration=rep(i,dim(AnnThermocline)[1]))
                                   )
                             )
}else{
  vert_profile_output<-rbind(vert_profile_output,
                             cbind(AnnThermocline,
                                   data.frame(iteration=rep(i,dim(AnnThermocline)[1]))
                             )
  )
}



## Surface Profile Responses ##
horz_profile<-NULL
horz_profile<- read_horz_profile_files(paste0(iterate_file1))
horz_profile<- horz_profile[horz_profile[,1] %in% rownames(scalars),]
SurfaceTempVar<-grep(colnames(horz_profile),pattern="Temp")[1]
colnames(horz_profile)[SurfaceTempVar]<-"SurfaceTemp.oC."
horz_profile %>%
  group_by(JDate) %>%
  summarise(Mean_Surface_Temp= mean(SurfaceTemp.oC.),
            Min_Surface_Temp= min(SurfaceTemp.oC.),
            Max_Surface_Temp= max(SurfaceTemp.oC.)) %>%
  ggplot(aes(x=JDate, y = Mean_Surface_Temp))+geom_line()+
  geom_ribbon(aes(x = JDate, ymin = Min_Surface_Temp, ymax=Max_Surface_Temp), color=NA ,alpha=0.3)+
  ylab(paste("Surface Tempurature")) +
  theme_bw()

ref_horz_profile<-NULL
ref_horz_profile<- read_horz_profile_files(paste0(dirfile))
ref_horz_profile<- ref_horz_profile[ref_horz_profile[,1] %in% rownames(scalars),]
colnames(ref_horz_profile)[SurfaceTempVar]<-"SurfaceTemp.oC."
ref_horz_profile %>%
  group_by(JDate) %>%
  summarise(Mean_Surface_Temp= mean(SurfaceTemp.oC.),
            Min_Surface_Temp= min(SurfaceTemp.oC.),
            Max_Surface_Temp= max(SurfaceTemp.oC.)) %>%
  ggplot(aes(x=JDate, y = Mean_Surface_Temp))+geom_line()+
  geom_ribbon(aes(x = JDate, ymin = Min_Surface_Temp, ymax=Max_Surface_Temp), color=NA ,alpha=0.3)+
  ylab(paste("Surface Tempurature")) +
  theme_bw()

horz_profile_diff <- ref_horz_profile %>%
  group_by(JDate) %>%
  summarise(ref_Mean_Surface_Temp= mean(SurfaceTemp.oC.)) %>%
  dplyr::select(JDate, ref_Mean_Surface_Temp)
horz_profile_diff <- horz_profile %>%
  group_by(JDate) %>%
  summarise(Mean_Surface_Temp= mean(SurfaceTemp.oC.)) %>%
  dplyr::select(JDate, Mean_Surface_Temp) %>%
  merge(.,horz_profile_diff, by="JDate") %>%
  mutate(Temp_Change = ((Mean_Surface_Temp-ref_Mean_Surface_Temp)/ref_Mean_Surface_Temp))

horz_profile_diff %>%
  ggplot(aes(x=JDate, y=Temp_Change)) + geom_line()+
  geom_smooth(se = FALSE)+
  ylab("Percent Change in Mean Surface Temperature")+
  theme_bw()


#### thermo profile plots ####
{
  maxT<-max(max(AnnTempProfile$MeanTemp),max(ref_AnnTempProfile$MeanTemp),max(ref_horz_profile$SurfaceTemp.oC.),max(horz_profile$SurfaceTemp.oC.))
  minT<-min(min(AnnTempProfile$MeanTemp),min(ref_AnnTempProfile$MeanTemp),min(ref_horz_profile$SurfaceTemp.oC.),min(horz_profile$SurfaceTemp.oC.))

  surf_ts<- horz_profile %>%
    group_by(JDate) %>%
    summarise(Mean_Surface_Temp= mean(SurfaceTemp.oC.),
              Min_Surface_Temp= min(SurfaceTemp.oC.),
              Max_Surface_Temp= max(SurfaceTemp.oC.)) %>%
    ggplot(aes(x = JDate, y = Mean_Surface_Temp, color=Mean_Surface_Temp))+
    geom_ribbon(aes(x = JDate, ymin = Min_Surface_Temp, ymax=Max_Surface_Temp), color=NA ,alpha=0.3)+
    scale_color_gradientn(colors = hcl.colors(20, "RdYlBu", rev = TRUE), limits=c(minT,maxT)) +
    geom_line(linewidth=2)+
    ylim(c(minT,maxT))+
    ylab(paste("Loop ",iteration,", Surface Temp (C)")) +
    theme_bw()

  temp_ts<-ggplot(AnnTempProfile, aes(x = Date, y = MeanDepth, fill = MeanTemp)) +
    scale_y_reverse() +
    geom_tile(show.legend = FALSE) +
    scale_fill_gradientn(colors = hcl.colors(20, "RdYlBu", rev = TRUE), limits=c(minT,maxT)) +
    geom_line(data=AnnThermocline, aes(x = Date, y = ThermoDepth )) +
    ylab(paste("Loop ",iteration," Seg ",outflow_seg,", Depth (m)")) +
    theme_bw() #+ coord_fixed()

  ref_surf_ts<- ref_horz_profile %>%
    group_by(JDate) %>%
    summarise(Mean_Surface_Temp= mean(SurfaceTemp.oC.),
              Min_Surface_Temp= min(SurfaceTemp.oC.),
              Max_Surface_Temp= max(SurfaceTemp.oC.)) %>%
    ggplot(aes(x = JDate, y = Mean_Surface_Temp, color=Mean_Surface_Temp))+
    geom_ribbon(aes(x = JDate, ymin = Min_Surface_Temp, ymax=Max_Surface_Temp), color=NA ,alpha=0.3)+
    scale_color_gradientn(colors = hcl.colors(20, "RdYlBu", rev = TRUE), limits=c(minT,maxT)) +
    geom_line(linewidth=2)+
    ylim(c(minT,maxT))+
    ylab(paste("Reference Surface Temp (C)")) +
    theme_bw()

  reft_ts<-ggplot(ref_AnnTempProfile, aes(x = Date, y = MeanDepth, fill = MeanTemp)) +
    scale_y_reverse() +
    geom_tile(show.legend = FALSE) +
    scale_fill_gradientn(colors = hcl.colors(20, "RdYlBu", rev = TRUE), limits=c(minT,maxT)) +
    geom_line(data=ref_AnnThermocline, aes(x = Date, y = ThermoDepth )) +
    ylab(paste("Reference Seg ",outflow_seg,", Depth (m)")) +
    theme_bw() #+ coord_fixed()
}
# grid.arrange
# arrangeGrob

annual_profile<-gridExtra::arrangeGrob(top = waterbody_name, bottom =paste0(100*iteration_area_FPV/sum(install_FPV_area)," %"),
                        arrangeGrob(surf_ts, ref_surf_ts, top="Mean Surface Tempurature"),
                        arrangeGrob(temp_ts, reft_ts, top="Vertical Temperature Profile"), ncol=2)
# plot(annual_profile)
}

# iteration_area_FPV/sum(install_FPV_area)


ggsave(annual_profile, file= paste0(iterate_file1,"/Plot_save_Loop/year_profile_plot_",i,".png"), dpi = 200, width = 16, height = 8)
# paste0(iterate_file1,"/Plot_save_Loop/year_profile_plot_",i,".png")

# j<-31.5
# j<-213.5
# k=outflow_seg
# as_datetime_custom(j, mod_year)
# as_datetime_custom(j, mod_year)

# winter_window <- c(31.5)
# winter_window <- c(1.5:60.5) # Jan - Feb
# summer_window <- c(213.5)
# summer_window <- c(182.5:243.5) # July - Aug
# c(winter_window,summer_window)

#### seasonal temperature profile ####

for(k in c(outflow_seg,center_seg)){
  if(k==outflow_seg){
    loop_location<-"outflow"
  }
  if(k==center_seg){
    loop_location<-"center"
  }
  
  for(j in c("winter","summer")){
  # day<-j
  season <- j
  
    if(j == "winter"){
    day <- winter_window
  }
    if(j == "summer"){
    day <- summer_window
  }

  
  # Extract specific days thermocline
  test<-SPR_wide %>%
    mutate(Date= as.integer(Julian_day)) %>%
    filter(SegNum==k & Date %in% as.integer(day)) %>%
    filter(Temperature!=-99) %>%
    group_by(Date,LayerUnderSurface) %>%
    summarise(
      MeanTemp = mean(Temperature,na.rm = TRUE),
      MaxTemp = max(Temperature,na.rm = TRUE),
      MinTemp = min(Temperature,na.rm = TRUE),
      Depth= mean(Depth,na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(Date) %>%
      mutate(SegDepth= max(Depth),
             Thermocline = rLakeAnalyzer::thermo.depth(MeanTemp, Depth)) 
  
 
    
  ref_test<-ref_SPR_wide %>%
    mutate(Date= as.integer(Julian_day)) %>%
    filter(SegNum==k & Date %in% as.integer(day)) %>%
    filter(Temperature!=-99) %>%
    group_by(Date,LayerUnderSurface) %>%
    summarise(
      MeanTemp = mean(Temperature,na.rm = TRUE),
      MaxTemp = max(Temperature,na.rm = TRUE),
      MinTemp = min(Temperature,na.rm = TRUE),
      Depth= mean(Depth,na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(Date) %>%
    mutate(SegDepth= max(Depth),
           Thermocline = rLakeAnalyzer::thermo.depth(MeanTemp, Depth))
  
  # #plot of thermocline depth

  # Schmidt Stability Index Bathymetry
  BathA<-rowSums(BathyHead$DLX*BathyWidth[,-which(names(BathyWidth) %in% c("LAYERH","K","Elev_diff","Elevation"))])
  BathD<-na.omit(BathyWidth$Elevation-na.omit(BathyWidth$Elevation[BathyWidth$Elev_diff==0]))
  
  max_depth_profile<-max(test$Depth)
  top<-head(BathD[(BathD-max_depth_profile)<=0], n=1)
  bot<-tail(BathD[BathD>=0], n=1)
  
  BathA_out<-BathA[BathD<=top & BathD>=bot]
  BathD_out<-abs(BathD[BathD<=top & BathD>=bot]-max_depth_profile)
  BathD_out
  
  # lake stratification strength
  loop_stability_daily<- test %>%
    group_by(Date) %>%
    summarise(S_Stability=c(schmidt.stability.custom(MeanTemp , Depth, bthA=BathA_out, bthD=BathD_out)))
  loop_stability<- mean(loop_stability_daily$S_Stability)
  # c(schmidt.stability.custom(test[c(1:26),]$MeanTemp , test[c(1:26),]$Depth, bthA=BathA_out, bthD=BathD_out))
  ref_loop_stability_daily<-ref_test %>%
    group_by(Date) %>%
    summarise(S_Stability=c(schmidt.stability.custom(MeanTemp , Depth, bthA=BathA_out, bthD=BathD_out)))
  ref_loop_stability<-mean(ref_loop_stability_daily$S_Stability)
  
  
  loop_stability_change<-(loop_stability-ref_loop_stability)/ref_loop_stability
  if(i==1 & j=="winter" & loop_location=="outflow"){
    collected_responses2[1,]<-c(model_reservoir,iteration, iteration_area_FPV, season, loop_location, 
                               "percent_change", "schmidt.stability", loop_stability_change)
    collected_responses<-rbind(collected_responses,c(model_reservoir,iteration, iteration_area_FPV, season, loop_location,
                                                     "value","schmidt.stability", loop_stability))
    collected_responses<-rbind(collected_responses,c(model_reservoir, 0, 0, season, loop_location,
                                                     "value","schmidt.stability", ref_loop_stability))
  }else{
  collected_responses<-rbind(collected_responses,c(model_reservoir,iteration, iteration_area_FPV, season, loop_location,
                                                   "percent_change","schmidt.stability", loop_stability_change))
  collected_responses<-rbind(collected_responses,c(model_reservoir,iteration, iteration_area_FPV, season, loop_location,
                                                   "value","schmidt.stability", loop_stability))
  }
  
  
  # mean thermocline depth
  thcD_mean_in<-mean(aggregate(test$Thermocline, by=list(test$Date), FUN=mean)$x, na.rm = TRUE)
  thcD_mean_ref<-mean(aggregate(ref_test$Thermocline, by=list(ref_test$Date), FUN=mean)$x, na.rm = TRUE)
  thcD_mean<-(thcD_mean_in-thcD_mean_ref)/thcD_mean_ref
  
  if(i==1){
    collected_responses<-rbind(collected_responses,c(model_reservoir, 0,0, season,loop_location, "value", "thermocline.depth", 
                                                     thcD_mean_ref))
  }
  collected_responses<-rbind(collected_responses,c(model_reservoir,iteration, iteration_area_FPV, season,loop_location, 
                                                   "percent_change", "thermocline.depth", thcD_mean))
  collected_responses<-rbind(collected_responses,c(model_reservoir,iteration, iteration_area_FPV, season,loop_location, 
                                                   "value", "thermocline.depth", thcD_mean_in))
  
  # change in mean, max, and min temperatures
  loop_mean_in<-mean(aggregate(test$MeanTemp, by=list(test$Date), FUN=mean)$x)
  loop_mean_ref<-mean(aggregate(ref_test$MeanTemp, by=list(ref_test$Date), FUN=mean)$x)
  loop_mean<-(loop_mean_in-loop_mean_ref)/loop_mean_ref
  
  if(i==1){
    collected_responses<-rbind(collected_responses,c(model_reservoir, 0,0, season,loop_location, "value", "temp.mean", 
                                                     loop_mean_ref))
  }
  collected_responses<-rbind(collected_responses,c(model_reservoir,iteration, iteration_area_FPV, season,loop_location,
                                                   "percent_change","temp.mean", loop_mean))
  collected_responses<-rbind(collected_responses,c(model_reservoir,iteration, iteration_area_FPV, season,loop_location,
                                                   "value","temp.mean", loop_mean_in))
  
  loop_min_in<-mean(aggregate(test$MeanTemp, by=list(test$Date), FUN=min)$x)
  loop_min_ref<-mean(aggregate(ref_test$MeanTemp, by=list(ref_test$Date), FUN=min)$x)
  loop_min<-(loop_min_in-loop_min_ref)/loop_min_ref
  
  if(i==1){
    collected_responses<-rbind(collected_responses,c(model_reservoir, 0,0, season,loop_location, "value", "temp.min", 
                                                     loop_min_ref))
  }
  collected_responses<-rbind(collected_responses,c(model_reservoir,iteration, iteration_area_FPV, season,loop_location,
                                                   "percent_change","temp.min", loop_min))
  collected_responses<-rbind(collected_responses,c(model_reservoir,iteration, iteration_area_FPV, season,loop_location,
                                                   "value","temp.min", loop_min_in))
  
  loop_max_in<-mean(aggregate(test$MaxTemp, by=list(test$Date), FUN=max)$x)
  loop_max_ref<-mean(aggregate(ref_test$MaxTemp, by=list(ref_test$Date), FUN=max)$x)
  loop_max<-(loop_max_in-loop_max_ref)/loop_max_ref

  if(i==1){
    collected_responses<-rbind(collected_responses,c(model_reservoir, 0,0, season,loop_location, "value", "temp.max", 
                                                     loop_max_ref))
  }
  collected_responses<-rbind(collected_responses,c(model_reservoir,iteration, iteration_area_FPV, season,loop_location,
                                                   "percent_change","temp.max", loop_max))
  collected_responses<-rbind(collected_responses,c(model_reservoir,iteration, iteration_area_FPV, season,loop_location,
                                                   "value","temp.max", loop_max_in))

  

  
  
  #### number of days stratified ####
  
  # Extract specific days thermocline
  test<-SPR_wide %>%
    mutate(Date= as.integer(Julian_day)) %>%
    filter(SegNum==k & Date %in% as.integer(day)) %>%
    filter(Temperature!=-99) %>%
    group_by(Date,LayerUnderSurface) %>%
    summarise(
      MeanTemp = mean(Temperature,na.rm = TRUE),
      MaxTemp = max(Temperature,na.rm = TRUE),
      MinTemp = min(Temperature,na.rm = TRUE),
      Depth= mean(Depth,na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(Date) %>%
    summarise(SegDepth= max(Depth),
           Thermocline = rLakeAnalyzer::thermo.depth(MeanTemp, Depth))
  
  
  
  ref_test<-ref_SPR_wide %>%
    mutate(Date= as.integer(Julian_day)) %>%
    filter(SegNum==k & Date %in% as.integer(day)) %>%
    filter(Temperature!=-99) %>%
    group_by(Date,LayerUnderSurface) %>%
    summarise(
      MeanTemp = mean(Temperature,na.rm = TRUE),
      MaxTemp = max(Temperature,na.rm = TRUE),
      MinTemp = min(Temperature,na.rm = TRUE),
      Depth= mean(Depth,na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(Date) %>%
    summarise(SegDepth= max(Depth),
           Thermocline = rLakeAnalyzer::thermo.depth(MeanTemp, Depth)) 
  
  n_day_strat<-length(test$Thermocline[!is.na(test$Thermocline)])
  n_day_strat_ref<-length(ref_test$Thermocline[!is.na(ref_test$Thermocline)])
  thermocline.yn <- (n_day_strat - n_day_strat_ref)/ length(day)
  
  # thermocline.yn<- ifelse(is.na(thermo.depth(test$Temperature,test$Depth)),0,1)+ifelse(is.na(thermo.depth(ref_test$Temperature,ref_test$Depth)),0,-1)

  if(i==1){
    collected_responses<-rbind(collected_responses,c(model_reservoir, 0,0, season,loop_location, "value", "stratified.days", 
                                                     n_day_strat_ref))
  }
  collected_responses<-rbind(collected_responses,c(model_reservoir,iteration, iteration_area_FPV, season, loop_location, 
                                                   "percent_change","stratified.days", thermocline.yn))
  collected_responses<-rbind(collected_responses,c(model_reservoir,iteration, iteration_area_FPV, season, loop_location, 
                                                   "value","stratified.days", n_day_strat))

  
  #### Dissolved O2 if present ####
  
  if("Dissolved_oxygen" %in% colnames(SPR_wide)){

    test<-SPR_wide %>%
      mutate(Date= as.integer(Julian_day)) %>%
      filter(SegNum==k & Date %in% as.integer(day)) %>%
      filter(Temperature!=-99) %>%
      group_by(Date,LayerUnderSurface) %>%
      summarise(
        MeanDO = mean(Dissolved_oxygen,na.rm = TRUE),
        MaxDO = max(Dissolved_oxygen,na.rm = TRUE),
        MinDO = min(Dissolved_oxygen,na.rm = TRUE),
        Depth= mean(Depth,na.rm = TRUE)) %>%
      ungroup() %>%
      group_by(Date) %>%
      mutate(SegDepth= max(Depth)) 
    
    ref_test<-ref_SPR_wide %>%
      mutate(Date= as.integer(Julian_day)) %>%
      filter(SegNum==k & Date %in% as.integer(day)) %>%
      filter(Temperature!=-99) %>%
      group_by(Date,LayerUnderSurface) %>%
      summarise(
        MeanDO = mean(Dissolved_oxygen,na.rm = TRUE),
        MaxDO = max(Dissolved_oxygen,na.rm = TRUE),
        MinDO = min(Dissolved_oxygen,na.rm = TRUE),
        Depth= mean(Depth,na.rm = TRUE)) %>%
      ungroup() %>%
      group_by(Date) %>%
      mutate(SegDepth= max(Depth))
    
    # change in mean, max, and min temperatures
    loop_mean_in<-mean(aggregate(test$MeanDO, by=list(test$Date), FUN=mean)$x)
    loop_mean_ref<-mean(aggregate(ref_test$MeanDO, by=list(ref_test$Date), FUN=mean)$x)
    loop_mean<-(loop_mean_in-loop_mean_ref)/loop_mean_ref

    loop_mean<-ifelse(is.na(loop_mean)|is.infinite(loop_mean),NA,loop_mean)
    
    if(i==1){
      collected_responses<-rbind(collected_responses,c(model_reservoir, 0,0, season,loop_location, "value", "do.mean", 
                                                       loop_mean_ref))
    }
    collected_responses<-rbind(collected_responses,c(model_reservoir,iteration, iteration_area_FPV, season,loop_location, 
                                                     "percent_change","do.mean", loop_mean))
    collected_responses<-rbind(collected_responses,c(model_reservoir,iteration, iteration_area_FPV, season,loop_location, 
                                                     "value","do.mean", loop_mean_in))

    loop_min_in<-mean(aggregate(test$MeanDO, by=list(test$Date), FUN=min)$x)
    loop_min_ref<-mean(aggregate(ref_test$MeanDO, by=list(ref_test$Date), FUN=min)$x)
    loop_min<-(loop_min_in-loop_min_ref)/loop_min_ref
    
    loop_min<-ifelse(is.na(loop_min)|is.infinite(loop_min),NA,loop_min)
    
    if(i==1){
      collected_responses<-rbind(collected_responses,c(model_reservoir,0, 0, season,loop_location,
                                                       "value","do.min", loop_min_ref))
    }
    collected_responses<-rbind(collected_responses,c(model_reservoir,iteration, iteration_area_FPV, season,loop_location,
                                                     "percent_change","do.min", loop_min))
    collected_responses<-rbind(collected_responses,c(model_reservoir,iteration, iteration_area_FPV, season,loop_location,
                                                     "value","do.min", loop_min_in))

    loop_max_in<-mean(aggregate(test$MeanDO, by=list(test$Date), FUN=max)$x)
    loop_max_ref<-mean(aggregate(ref_test$MeanDO, by=list(ref_test$Date), FUN=max)$x)
    loop_max<-(loop_max_in-loop_max_ref)/loop_max_ref
    
    loop_max<-ifelse(is.na(loop_max)|is.infinite(loop_max),NA,loop_max)
    
    if(i==1){
      collected_responses<-rbind(collected_responses,c(model_reservoir,0, 0, season,loop_location, 
                                                       "value","do.max", loop_max_ref))
    }
    collected_responses<-rbind(collected_responses,c(model_reservoir,iteration, iteration_area_FPV, season,loop_location, 
                                                     "percent_change","do.max", loop_max))
    collected_responses<-rbind(collected_responses,c(model_reservoir,iteration, iteration_area_FPV, season,loop_location, 
                                                     "value","do.max", loop_max_in))
    # end DO if
  }

}

}

# 1.5
#### mean surface water temp ####
for(j in c("winter","summer")){
  # day<-j
  season <- j
  
  if(j == "winter"){
    day <- winter_window
  }
  if(j == "summer"){
    day <- summer_window
  }
  
  horz_profile<-NULL
  horz_profile<- read_horz_profile_files(paste0(iterate_file1))
  horz_profile<- horz_profile[horz_profile[,1] %in% rownames(scalars),]
  SurfaceTempVar<-grep(colnames(horz_profile),pattern="Temp")[1]
  colnames(horz_profile)[SurfaceTempVar]<-"SurfaceTemp.oC."
  surface_test<-horz_profile %>%
    group_by(JDate) %>%
    summarise(Mean_Surface_Temp= mean(SurfaceTemp.oC.),
              Min_Surface_Temp= min(SurfaceTemp.oC.),
              Max_Surface_Temp= max(SurfaceTemp.oC.)) %>%
    mutate(Date= as.integer(JDate)) %>%
    filter(Date %in% as.integer(day))
  
  surface_mean_in<-mean(surface_test$Mean_Surface_Temp, na.rm = TRUE)
  
  ref_horz_profile<-NULL
  ref_horz_profile<- read_horz_profile_files(paste0(dirfile))
  ref_horz_profile<- ref_horz_profile[ref_horz_profile[,1] %in% rownames(scalars),]
  colnames(ref_horz_profile)[SurfaceTempVar]<-"SurfaceTemp.oC."
  ref_surface_test<- ref_horz_profile %>%
    group_by(JDate) %>%
    summarise(Mean_Surface_Temp= mean(SurfaceTemp.oC.),
              Min_Surface_Temp= min(SurfaceTemp.oC.),
              Max_Surface_Temp= max(SurfaceTemp.oC.)) %>%
    mutate(Date= as.integer(JDate)) %>%
    filter(Date %in% as.integer(day))
  
  surface_mean_ref<-mean(ref_surface_test$Mean_Surface_Temp, na.rm = TRUE)
  
  surface_mean <- (surface_mean_in - surface_mean_ref)/surface_mean_ref
  
  
  if(i==1){
    collected_responses<-rbind(collected_responses,c(model_reservoir,0, 0, season, "full", 
                                                     "value","mean.surface.temp",surface_mean_ref))
  }
  collected_responses<-rbind(collected_responses,c(model_reservoir,iteration, iteration_area_FPV, season, "full", 
                                                   "percent_change","mean.surface.temp",surface_mean))
  collected_responses<-rbind(collected_responses,c(model_reservoir,iteration, iteration_area_FPV, season, "full", 
                                                   "value","mean.surface.temp",surface_mean_in))
}


# 2
#### dam outflow temp ####
# NOTES:
# If there is one output located at the withdrawal output segment [IWDO], like a structure, the output files will be called
# qwo_segXX.YYY, two_segXX.YYY, cwo_segXX.YYY, and dwo_segXX.YYY, where XX is the seg-ment number, q is for flow, t is for
# temperature, c is for concentration, d is for derived concentrations, and YYY is the filetype given for the wdo output file
# (if one chooses, wdo.csv, then YYY=csv). If there is a struc-ture, withdrawal and a pump at this segment, the combined flows
# and flow-averaged temperature and concentrations will be written to these files.
# qwo_11.csv
# two_11.csv


# WSCfile_loc<-list.files(iterate_file1, pattern = paste0(stringr::str_squish(basename(WSCfile)),"$"), recursive=TRUE, full.names = TRUE)
# if(length(WSCfile_loc)!=1){print("More than one file included in location")}


outflow_q_raw_file<-list.files(paste0(iterate_file1,"/"), pattern = paste0("qwo_",outflow_seg), recursive=TRUE, full.names = TRUE)[1]
outflow_q_raw<-readLines(outflow_q_raw_file)
outflow_q_data_row<-grep("^JDAY",outflow_q_raw)
outflow_q<-read.csv(outflow_q_raw_file, skip=(outflow_q_data_row), row.names = NULL ,header=FALSE)

outflow_t_raw_file<-list.files(paste0(iterate_file1,"/"), pattern = paste0("two_",outflow_seg), recursive=TRUE, full.names = TRUE)[1]
outflow_t_raw<-readLines(outflow_t_raw_file)
outflow_t_data_row<-grep("^JDAY",outflow_t_raw)
outflow_t<-read.csv(outflow_t_raw_file, skip=(outflow_t_data_row), row.names = NULL ,header=FALSE)

# outflow_q$row.names<-as.numeric(outflow_q$row.names)
# outflow_t$row.names<-as.numeric(outflow_t$row.names)
all(outflow_q[,1]==outflow_t[,1])
if(all(outflow_q[,1]==outflow_t[,1])){
  outflow<-data.frame(JDAY=outflow_q[,1], meanQ=outflow_q[,2], meanT=outflow_t[,2])
}else{
  print("Not all JDATE values match")
}

outflow_summary<-outflow %>%
  mutate(Date=as.integer(JDAY)) %>%
  group_by(Date) %>%
  summarise(
    meanDailyQ = mean(meanQ),
    meanDailyT = mean(meanT),
    minDailyQ = min(meanQ),
    minDailyT = min(meanT),
    maxDailyQ = max(meanQ),
    maxDailyT = max(meanT)
  )

#importing baseline reference data
ref_outflow_q_raw_file<-list.files(paste0(dirfile,"/"), pattern = paste0("qwo_",outflow_seg), recursive=TRUE, full.names = TRUE)[1]
ref_outflow_q_raw<-readLines(ref_outflow_q_raw_file)
ref_outflow_q_data_row<-grep("^JDAY",ref_outflow_q_raw)
ref_outflow_q<-read.csv(ref_outflow_q_raw_file, skip=(ref_outflow_q_data_row), row.names = NULL ,header=FALSE)

ref_outflow_t_raw_file<-list.files(paste0(dirfile,"/"), pattern = paste0("two_",outflow_seg), recursive=TRUE, full.names = TRUE)[1]
ref_outflow_t_raw<-readLines(ref_outflow_t_raw_file)
ref_outflow_t_data_row<-grep("^JDAY",ref_outflow_t_raw)
ref_outflow_t<-read.csv(ref_outflow_t_raw_file, skip=(ref_outflow_t_data_row), row.names = NULL ,header=FALSE)


# ref_outflow_q$row.names<-as.numeric(ref_outflow_q$row.names)
# ref_outflow_t$row.names<-as.numeric(ref_outflow_t$row.names)
all(ref_outflow_q[,1]==ref_outflow_t[,1])
if(all(ref_outflow_q[,1]==ref_outflow_t[,1])){
  ref_outflow<-data.frame(JDAY=ref_outflow_q[,1], meanQ=ref_outflow_q[,2], meanT=ref_outflow_t[,2])
}else{
  print("Not all JDATE values match")
}

ref_outflow_summary<-ref_outflow %>%
  mutate(Date=as.integer(JDAY)) %>%
  group_by(Date) %>%
  summarise(
    meanDailyQ = mean(meanQ),
    meanDailyT = mean(meanT),
    minDailyQ = min(meanQ),
    minDailyT = min(meanT),
    maxDailyQ = max(meanQ),
    maxDailyT = max(meanT)
  )

outflow_summary$treatment<-"test"
ref_outflow_summary$treatment<-"ref"

# outflow_merged<-rbind(outflow_summary,ref_outflow_summary)
# outflow_merged[outflow_merged$Date==213,]

# extra plot: outflow
# plot(ref_outflow_summary$Date ,ref_outflow_summary$meanDailyT, type='l', xlab='Date', ylab='Temperature (C)', main="Outflow Temperature (C)")
# lines(outflow_summary$Date ,outflow_summary$meanDailyT, type='l', col="red")
# legend("topleft", legend=c("Reference", paste0("Loop #",iteration)),
#        col=c("black", "red"), lty=1, cex=0.8)



for(j in c("winter","summer")){
  # day<-j
  season <- j
  
  if(j == "winter"){
    day <- winter_window
  }
  if(j == "summer"){
    day <- summer_window
  }
  
  
loop_flow<-outflow_summary[outflow_summary$Date %in% as.integer(day),]
ref_loop_flow<-ref_outflow_summary[ref_outflow_summary$Date %in% as.integer(day),]

# levels(collected_responses$ResponseVar)
loop_mean_in<-mean(aggregate(loop_flow$meanDailyT, by=list(loop_flow$Date), FUN=mean)$x)
loop_mean_ref<-mean(aggregate(ref_loop_flow$meanDailyT, by=list(ref_loop_flow$Date), FUN=mean)$x)
loop_mean<-(loop_mean_in-loop_mean_ref)/loop_mean_ref

if(i==1){
  collected_responses<-rbind(collected_responses,c(model_reservoir,0, 0, season, "full", 
                                                   "value","mean.outflow.temp",loop_mean_ref))
}
collected_responses<-rbind(collected_responses,c(model_reservoir,iteration, iteration_area_FPV, season, "full", 
                                                 "percent_change","mean.outflow.temp",loop_mean))
collected_responses<-rbind(collected_responses,c(model_reservoir,iteration, iteration_area_FPV, season, "full", 
                                                 "value","mean.outflow.temp",loop_mean_in))

loop_min_in<-aggregate(loop_flow$minDailyT, by=list(loop_flow$Date), FUN=mean)
loop_max_in<-aggregate(loop_flow$maxDailyT, by=list(loop_flow$Date), FUN=mean)
# mean((loop_max_in-loop_min_in)$x)
loop_min_ref<-aggregate(ref_loop_flow$minDailyT, by=list(ref_loop_flow$Date), FUN=mean)
loop_max_ref<-aggregate(ref_loop_flow$maxDailyT, by=list(ref_loop_flow$Date), FUN=mean)
# mean((loop_max_ref-loop_min_ref)$x)

loop_range<- (mean((loop_max_in-loop_min_in)$x) - mean((loop_max_ref-loop_min_ref)$x))/mean((loop_max_ref-loop_min_ref)$x)
loop_range_ref<-mean((loop_max_ref-loop_min_ref)$x)
loop_range_val<-mean((loop_max_in-loop_min_in)$x)

if(i==1){
  collected_responses<-rbind(collected_responses,c(model_reservoir,0, 0, season, "full",
                                                   "value","range.outflow.temp",loop_range_ref))
}
collected_responses<-rbind(collected_responses,c(model_reservoir,iteration, iteration_area_FPV, season, "full",
                                                 "percent_change","range.outflow.temp",loop_range))
collected_responses<-rbind(collected_responses,c(model_reservoir,iteration, iteration_area_FPV, season, "full",
                                                 "value","range.outflow.temp",loop_range_val))
}


# 3
#### warm/coldwater fish ####
# check spp reference and habitat criteria limits #### W2_con
fish_raw<-readLines(paste0(iterate_file1,"/habitat.csv"))
fish_data_row<-grep("^JDAY",fish_raw)

#reference data
fish_data_ref<-read.csv(paste0(dirfile,"/habitat.csv"), skip=(fish_data_row-1))

fish_data<-read.csv(paste0(iterate_file1,"/habitat.csv"), skip=(fish_data_row-1))

# fish_data_ref
# fish_data %>%
  # colMeans()
# need to count the number of entries (more resolution across the year)

fish_data_summary<-fish_data %>%
  mutate(Date=as.integer(JDAY)) %>%
  group_by(Date) %>%
  summarise(
    meanDailyCold = mean(X.VOL.RainbowTrout),
    meanDailyWarm = mean(X.VOL.LargemouthBass)
  )

fish_data_ref_summary<-fish_data_ref %>%
  mutate(Date=as.integer(JDAY)) %>%
  group_by(Date) %>%
  summarise(
    meanDailyCold = mean(X.VOL.RainbowTrout),
    meanDailyWarm = mean(X.VOL.LargemouthBass)
  )

mean(fish_data_summary$meanDailyCold)
mean(fish_data_ref_summary$meanDailyCold)
mean(fish_data_summary$meanDailyWarm)
mean(fish_data_ref_summary$meanDailyWarm)

# Extra Plots: fish habitat
# plot(fish_data_ref_summary$Date ,fish_data_ref_summary$meanDailyCold, type='l', lty=1, col="black",xlab='Date', ylab='Percent of Volumn', main="Coldwater Fish Habitat")
# lines(fish_data_summary$Date ,fish_data_summary$meanDailyCold, type='l', lty=2, col="red")
# legend("bottomleft", legend=c("Reference", paste0("Loop #",iteration)),
#        col=c("black", "red"), lty=1:2, cex=0.8)
# 
# plot(fish_data_ref_summary$Date ,fish_data_ref_summary$meanDailyWarm, type='l', lty=1, col="black",xlab='Date', ylab='Percent of Volumn', main="Warmwater Fish Habitat")
# lines(fish_data_summary$Date ,fish_data_summary$meanDailyWarm, type='l', lty=2, col="red")
# legend("topleft", legend=c("Reference", paste0("Loop #",iteration)),
#        col=c("black", "red"), lty=1:2, cex=0.8)

for(j in c("winter","summer")){
  # day<-j
  season <- j
  
  if(j == "winter"){
    day <- winter_window
  }
  if(j == "summer"){
    day <- summer_window
  }

  
  loop_fish<-fish_data_summary[fish_data_summary$Date %in% as.integer(day),]
  ref_loop_fish<-fish_data_ref_summary[fish_data_ref_summary$Date %in% as.integer(day),]
  
  # levels(collected_responses$ResponseVar)
  loop_cold_in<-mean(aggregate(loop_fish$meanDailyCold, by=list(loop_fish$Date), FUN=mean)$x)
  loop_cold_ref<-mean(aggregate(ref_loop_fish$meanDailyCold, by=list(ref_loop_fish$Date), FUN=mean)$x)
  loop_cold<-((loop_cold_in)-(loop_cold_ref))
  
  loop_cold<-ifelse(is.na(loop_cold)|is.infinite(loop_cold),NA,loop_cold)
  
  if(i==1){
    collected_responses<-rbind(collected_responses,c(model_reservoir,0, 0, season, "full", 
                                                     "value","coldwater.fish", loop_cold_ref))
  }
  collected_responses<-rbind(collected_responses,c(model_reservoir,iteration, iteration_area_FPV, season, "full", 
                                                   "percent_change","coldwater.fish", loop_cold))
  collected_responses<-rbind(collected_responses,c(model_reservoir,iteration, iteration_area_FPV, season, "full", 
                                                   "value","coldwater.fish", loop_cold_in))
  
  loop_warm_in<-mean(aggregate(loop_fish$meanDailyWarm, by=list(loop_fish$Date), FUN=mean)$x)
  loop_warm_ref<-mean(aggregate(ref_loop_fish$meanDailyWarm, by=list(ref_loop_fish$Date), FUN=mean)$x)
  loop_warm<-((loop_warm_in)-(loop_warm_ref))
  
  loop_warm<-ifelse(is.na(loop_warm)|is.infinite(loop_warm),NA,loop_warm)
  
  if(i==1){
    collected_responses<-rbind(collected_responses,c(model_reservoir,0, 0, season, "full", 
                                                     "value","warmwater.fish",loop_warm_ref))
  }
  collected_responses<-rbind(collected_responses,c(model_reservoir,iteration, iteration_area_FPV, season, "full", 
                                                   "percent_change","warmwater.fish",loop_warm))
  collected_responses<-rbind(collected_responses,c(model_reservoir,iteration, iteration_area_FPV, season, "full", 
                                                   "value","warmwater.fish",loop_warm_in))
}


if(i==1){
  collected_responses<-arrange(collected_responses, Iteration)
}

print(paste( "Completed loop:",i, "of", length(scalars[1,]) ))
#### END LOOP Repeat  #### 
}
collected_responses
str(collected_responses)

collected_responses$Iteration<-as.numeric(collected_responses$Iteration)
collected_responses$FPV_covered_area<-as.numeric(collected_responses$FPV_covered_area)
collected_responses$RespValue       <-as.numeric(collected_responses$RespValue       )
collected_responses$Res_Lowest_Area<-sum(install_FPV_area)
# percent area of FPV based on lowest elevation watermark from the course of the model
collected_responses$percentFPV <- collected_responses$FPV_covered_area/sum(install_FPV_area)
collected_responses<-arrange(collected_responses, Iteration)

#saving collected response output
write.csv(collected_responses, file = paste0(iterate_file1,".csv"))

#saving vertical profile output
str(vert_profile_output)
vert_profile_output$iteration<-as.factor(vert_profile_output$iteration)
write.csv(vert_profile_output, file= paste0(iterate_file1,"_profile.csv"))

#### visualize results ####

# animated figure with annual profile plots
list.files(path=paste0(iterate_file1,"/Plot_save_Loop/"), pattern = '*.png', full.names = TRUE) %>%
  gtools::mixedsort() %>%
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=2,optimize=TRUE) %>% # animates, can opt for number of loops
  image_write(paste0(iterate_file1,"/Plot_save_Loop/annimated.gif")) # write to current dir


collected_responses$RespValue<-as.numeric(collected_responses$RespValue)
collected_responses$Iteration<-as.numeric(collected_responses$Iteration)
collected_responses$FPV_covered_area<-as.numeric(collected_responses$FPV_covered_area)

collected_responses %>%
  filter(Season=="summer", ResType =="value") %>%
  ggplot(., aes(x=Iteration, y=RespValue))+
    facet_grid(ResponseVar~Location , scales = "free")+
    geom_line()
  
  
collected_responses %>%
  filter(Location!="outflow", ResType =="value") %>%
  ggplot(., aes(x=Iteration, y=RespValue))+
  facet_grid(ResponseVar ~Season, scales = "free")+
  geom_line()

collected_responses %>%
  filter(Location!="outflow", ResType =="percent_change") %>%
  ggplot(., aes(x=Iteration, y=RespValue))+
  facet_grid(ResponseVar ~Season, scales = "free")+
  geom_line()



#### end of file ####


