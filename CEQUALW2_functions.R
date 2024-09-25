#### function integration with CE-QUAL-W2
####
#### Author: Evan Bredeweg, PhD



#### Date Adjustment Fucntion ####

as_datetime_custom <- function(x, year_input, tz="UTC") as.POSIXct(paste0(year_input,"-01-01"), tz=tz) + as.difftime(x, units = "days", )


#### Read in Multiple Horiz. profile files ####

read_horz_profile_files <- function(filedir,prefix="ProfLongJD") {
  # This function reads in csv files that have the same prefix and returns a list of data frames.
  
  # Parameters:
  # filedir (character): Directory of the set of files
  # prefix (character): The prefix of the csv files to be read in
  
  # Returns:
  # dataframe: A dataframe containing the joined csv files with an additional column indicating the Julian Date extracted from the filename
  
  require(stringr)
  require(dplyr)
  tryCatch({
    # Get all csv files in the current working directory with the specified prefix
    
    
    file_names <- list.files(filedir, pattern = paste0("^", prefix))
    
    # Read in each csv file and add a column with the filename
    data_list <- lapply(file_names, function(file_name) {
      data <- read.csv(paste0(filedir,"/",file_name))
      data$JDate <- as.numeric(str_extract(file_name,"\\d+\\.*\\d*"))
      data
    })
    
    # Join all dataframes into one
    joined_data <-  bind_rows(data_list)
    joined_data1 <- arrange_at(joined_data,c("JDate",colnames(joined_data)[1]))
    
    # Return the joined dataframe
    return(joined_data1)
  }, error = function(e) {
    # Log the error
    cat("An error occurred:", e$message, "\n")
    return(NULL)
  })
}
# hor_prof<-read_horz_profile_files("testdir")
# as_datetime_custom(hor_prof$JDate, 2018)


#### Shade/WSC Adjustments for Loop ####

FPV_segment_calculate<-function(data_install_layer, area_increment, upstream_order, sorted_order=FALSE){
  stopifnot(is.logical(sorted_order), length(sorted_order) == 1)
  stopifnot(is.numeric(area_increment), length(area_increment) == 1, area_increment>0)
  stopifnot(dim(data_install_layer)[1]==1)
  stopifnot(is.vector(upstream_order), length(upstream_order)==dim(data_install_layer)[2])
  
  install_FPV_area_ordered<-data_install_layer[upstream_order]
  area_additive<-cumsum(c(install_FPV_area_ordered))
  
  for (i in 1:ceiling(sum(data_install_layer)/area_increment)){
    loop_area=i*area_increment
    
    seg_to_shade<-(area_additive-loop_area)/area_additive
    seg_to_shade1<-pmax(seg_to_shade,0)
    total_shade_n=length(seg_to_shade1[seg_to_shade1==0])
    
    area_total_shade<-sum(install_FPV_area_ordered[seg_to_shade1==0])
    
    if(total_shade_n==length(area_additive)){
      temp_out<-as.data.frame(seg_to_shade1)
      colnames(temp_out)="FPV_adjust"
    }else{
      if(area_total_shade!=loop_area){
        seg_partial_shade<- seg_to_shade1[total_shade_n+1] 
        seg_unchanged<- seg_to_shade1[c((total_shade_n+2):length(seg_to_shade1))]
        temp1<-as.data.frame(seg_to_shade1[seg_to_shade1==0])
        colnames(temp1)="FPV_adjust"
        temp2<-as.data.frame(seg_partial_shade)
        colnames(temp2)="FPV_adjust"
        temp3<-as.data.frame(pmax(seg_unchanged,1))
        colnames(temp3)="FPV_adjust"
        temp_out<-rbind(temp1,temp2,temp3)
      }else{
        seg_unchanged<- seg_to_shade1[c((total_shade_n+1):length(seg_to_shade1))]
        temp1<-as.data.frame(seg_to_shade1[seg_to_shade1==0])
        colnames(temp1)="FPV_adjust"
        temp3<-as.data.frame(pmax(seg_unchanged,1))
        colnames(temp3)="FPV_adjust"
        temp_out<-rbind(temp1,temp3)
      }
    }
    colnames(temp_out)=paste0(colnames(temp_out),"_",i)
    
    
    if(i==1){
      full_output<-temp_out
    }else{
      full_output<-cbind(full_output,temp_out)
    }
    
  }
  
  full_output
  
  if(sorted_order){
    scalar_output<-full_output[order(as.numeric(row.names(full_output))), ]
    return(scalar_output)
  }else{
    return(full_output)
  }
}

FPV_universal_calculate<-function(install_FPV_area,area_increment){
  #create empty scalar structure
  df<-data.frame(matrix(nrow = length(install_FPV_area), ncol = ceiling(sum(install_FPV_area)/area_increment),data = 1))
  colnames(df)<- paste0("FPV_adjust_",str_extract(colnames(df), "\\d+"))
  
  #adjust scale values for each segment equally
  for(j in c(1:ceiling(sum(install_FPV_area)/area_increment))){
    percent_mod<-(area_increment*j/sum(install_FPV_area))
    percent_mod<-pmin(percent_mod,1)
    df[j]<-df[j]*(1-percent_mod)
  }
  return(df)
}

max_adjustment_scalar <- function(df, max_perc=100, perc.decimal=FALSE){
  # adjusts the maximum percentage of shade from FPV array
  # measured as percent irradiation entering the lake
  # 100% is complete lost of radiation while 0% is no shading
  stopifnot(is.logical(perc.decimal))
  if(perc.decimal){
    max_perc <- max_perc*100
  }
  stopifnot(max_perc<=100,max_perc>=0)
  
  #creating adjustment function to apply to each cell
  adjustment_fun <- function(x, max_perc_pass=100){
    return ((x-1)*max_perc_pass/100+1)}
  
  # applying function and returning dataframe
  df_out <- data.frame(lapply(df,adjustment_fun, max_perc_pass=max_perc))
  row.names(df_out)<-row.names(df)
  
  return(df_out)
}

# NOTES for other version of fucntion
# 
# install_FPV_area_ordered<-install_FPV_area[upstream_order]
# area_additive<-cumsum(c(install_FPV_area_ordered))
# 
# 
# for (i in 1:ceiling(sum(install_FPV_area)/area_increment)){
#   loop_area=i*area_increment
#   
#   seg_to_shade<-(area_additive-loop_area)/area_additive
#   seg_to_shade1<-pmax(seg_to_shade,0)
#   total_shade_n=length(seg_to_shade1[seg_to_shade1==0])
#   
#   area_total_shade<-sum(install_FPV_area_ordered[seg_to_shade1==0])
#   
#   if(total_shade_n==length(area_additive)){
#     temp_out<-as.data.frame(seg_to_shade1)
#     colnames(temp_out)="FPV_adjust"
#   }else{
#     if(area_total_shade!=loop_area){
#       seg_partial_shade<- seg_to_shade1[total_shade_n+1] 
#       seg_unchanged<- seg_to_shade1[c((total_shade_n+2):length(seg_to_shade1))]
#       temp1<-as.data.frame(seg_to_shade1[seg_to_shade1==0])
#       colnames(temp1)="FPV_adjust"
#       temp2<-as.data.frame(seg_partial_shade)
#       colnames(temp2)="FPV_adjust"
#       temp3<-as.data.frame(pmax(seg_unchanged,1))
#       colnames(temp3)="FPV_adjust"
#       temp_out<-rbind(temp1,temp2,temp3)
#     }else{
#       seg_unchanged<- seg_to_shade1[c((total_shade_n+1):length(seg_to_shade1))]
#       temp1<-as.data.frame(seg_to_shade1[seg_to_shade1==0])
#       colnames(temp1)="FPV_adjust"
#       temp3<-as.data.frame(pmax(seg_unchanged,1))
#       colnames(temp3)="FPV_adjust"
#       temp_out<-rbind(temp1,temp3)
#     }
#   }
#   colnames(temp_out)=paste0(colnames(temp_out),"_",i)
#   temp_out
#   
#   if(i==1){
#     test<-temp_out
#   }else{
#     test<-cbind(test,temp_out)
#   }
#   
# }
# 
# 
# test<-foreach(i=c(1:ceiling(sum(install_FPV_area)/area_increment)),
#               .combine='cbind'
# ) %do% {
#   # set area of FPV for iteration
#   loop_area=i*area_increment
#   loop_area=i*area_increment
#   
#   seg_to_shade<-(area_additive-loop_area)/area_additive
#   seg_to_shade1<-pmax(seg_to_shade,0)
#   total_shade_n=length(seg_to_shade1[seg_to_shade1==0])
#   
#   area_total_shade<-sum(install_FPV_area_ordered[seg_to_shade1==0])
#   
#   if(total_shade_n==length(area_additive)){
#     temp_out<-as.data.frame(seg_to_shade1)
#     colnames(temp_out)="FPV_adjust"
#   }else{
#     if(area_total_shade!=loop_area){
#       seg_partial_shade<- seg_to_shade1[total_shade_n+1] 
#       seg_unchanged<- seg_to_shade1[c((total_shade_n+2):length(seg_to_shade1))]
#       temp1<-as.data.frame(seg_to_shade1[seg_to_shade1==0])
#       colnames(temp1)="FPV_adjust"
#       temp2<-as.data.frame(seg_partial_shade)
#       colnames(temp2)="FPV_adjust"
#       temp3<-as.data.frame(pmax(seg_unchanged,1))
#       colnames(temp3)="FPV_adjust"
#       temp_out<-rbind(temp1,temp2,temp3)
#     }else{
#       seg_unchanged<- seg_to_shade1[c((total_shade_n+1):length(seg_to_shade1))]
#       temp1<-as.data.frame(seg_to_shade1[seg_to_shade1==0])
#       colnames(temp1)="FPV_adjust"
#       temp3<-as.data.frame(pmax(seg_unchanged,1))
#       colnames(temp3)="FPV_adjust"
#       temp_out<-rbind(temp1,temp3)
#     }
#   }
#   colnames(temp_out)=paste0(colnames(temp_out),"_",i)
#   temp_out
# }
# 
# 
# test_reordered<-test[order(as.numeric(row.names(test))), ]


#### Read Bathyfile Head and Widths ####

ReadBathyHead <- function(filename, headerlines=1){
  require(stringr)
  tempfile<-read.csv(filename, header = FALSE)
  # remove header from file
  tempfile1<-tempfile[-c(1:(headerlines)),]
  #Extract values for segment dimensions and transpose
  tempfile1[1,1]<-"Segments"
  tempfile2<-t(tempfile1[c(1:5),])
  # rename columns and rename rows
  colnames(tempfile2)<- c(as.vector(unlist(tempfile2[1,])))
  tempfile3<-as.data.frame(tempfile2[-c(1),])
  tempfile3<-tempfile3[tempfile3$Segments!="",]
  row.names(tempfile3)<-c(1:dim(tempfile3)[1])
  # convert at numeric values
  tempfile3[,1]<-as.numeric(tempfile3[,1])
  tempfile3[,2]<-as.numeric(tempfile3[,2])
  tempfile3[,3]<-as.numeric(tempfile3[,3])
  tempfile3[,4]<-as.numeric(tempfile3[,4])
  tempfile3[,5]<-as.numeric(tempfile3[,5])
  return(tempfile3)
}


ReadNPTBathyHead <- function(filename, columns=10){
  require(tidyr)
  
  #locate the existing WSC and read it into R
  tempfile<-readLines(filename)
  # templengths<-sapply(tempfile, str_count)
  
  
  # checking for detection of acronyms
  if(!any(grepl("DLX", tempfile))){
    stop("Check acronyms of file. Must include 'DLX' in headings")
  }
  if(!any(grepl("PHI0", tempfile))){
    stop("Check acronyms of file. Must include 'PHI0' in headings")
  }
  if(!(any(grepl("FRICT", tempfile)) | any(grepl("FRICTC", tempfile)))){
    stop("Check acronyms of file. Must include 'FRICTC' or 'FRICT' in headings")
  }
  if(!(any(grepl("WSEL", tempfile)) | any(grepl("ELWS", tempfile)))){
    stop("Check acronyms of file. Must include 'WSEL' or 'ELWS' in headings")
  }
  
  # checking for water level acronym 
  if(any(grepl("WSEL", tempfile))){
    waterlevelcode<-"WSEL"
  }else{
    waterlevelcode<-"ELWS"
  }
  # checking for friction acronym 
  if(any(grepl("FRICTC", tempfile))){
    frictioncode<-"FRICTC"
  }else{
    frictioncode<-"FRICT"
  }
  
  
  # Segment lengths (DLX)
  start=grep("DLX", tempfile)
  end=grep(waterlevelcode, tempfile)-start-2
  DLX<-read.fortran(filename, c(paste0(columns,"F8.0")), skip= start, n= end)
  DLX1<-pivot_longer(as.data.frame(DLX),cols = starts_with("V"))
  
  # Water surface elevation [WSEL] or [ELWS]
  start=grep(waterlevelcode, tempfile)
  WSEL<-read.fortran(filename, c(paste0(columns,"F8.0")), skip= start, n= end)
  WSEL1<-pivot_longer(as.data.frame(WSEL),cols = starts_with("V"))
  
  # Segment orientation [PHI0]
  start=grep("PHI0", tempfile)
  PHI0<-read.fortran(filename, c(paste0(columns,"F8.0")), skip= start, n= end)
  PHI01<-pivot_longer(as.data.frame(PHI0),cols = starts_with("V"))
  
  # Bottom friction [FRICTC] or [FRICT]
  start=grep(frictioncode, tempfile)
  FRICTC<-read.fortran(filename, c(paste0(columns,"F8.0")), skip= start, n= end)
  FRICTC1<-pivot_longer(as.data.frame(FRICTC),cols = starts_with("V"))
  
  # recombine all variables into one data frame
  header_1<-data.frame(Segments=as.numeric(c(1:dim(DLX1)[1])),
                       DLX=DLX1$value,
                       ELWS = WSEL1$value,
                       PHI0 = PHI01$value,
                       FRIC = FRICTC1$value)
  header_out<-na.omit(header_1)
  attributes(header_out)$na.action <- NULL
  
  return(header_out)
}


ReadBathyH <- function(filename, columns_pass=10, headerlines_pass=1){
  require(stringr)
  
  if(str_detect(filename, ".csv$")){
    fun_out <- ReadBathyHead(filename, headerlines=headerlines_pass)
    message("CSV file format dectected")
  }
  
  if(str_detect(filename, ".npt$")){
    fun_out <- ReadNPTBathyHead(filename, columns=columns_pass)
    message("NPT file format dectected")
  }
  
  colnames(fun_out)<-str_squish(colnames(fun_out))
  
  return(fun_out)
}
# test<-ReadBathyH(filename)


ReadBathyWidth <- function(filename, headerlines=1){
  require(stringr)
  tempfile<-read.csv(filename, header = FALSE)
  # remove header from file
  tempfile1<-tempfile[-c(1:(headerlines)),]
  #Extract values for segment dimentions
  Segments<-as.numeric(na.omit(as.numeric(tempfile1[1,-c(1)])))
  first_row<-which(!is.na(as.numeric(tempfile1[,1])))[1]
  tempfile2<-tempfile1[-c(1:(first_row-1)),c(1:(1+length(Segments)))]
  colnames(tempfile2)<- c("LAYERH",Segments)
  row.names(tempfile2)<-c(1:dim(tempfile2)[1])
  tempfile3<-as.data.frame(tempfile2)
  tempfile3$K<-row.names(tempfile3)
  # convert at numeric values
  tempfile3[] <- sapply(tempfile3, as.numeric)
  return(tempfile3)
}



ReadNPTBathyWidth <- function(filename, columns=10){
  require(tidyr)
  
  #locate the existing WSC and read it into R
  tempfile<-readLines(filename, warn=FALSE)

  # Layer heights
  start=grep("ayer", tempfile)
  endL=which.min(ifelse((grep("egment", tempfile)-start)<0,NA,(grep("egment", tempfile)-start)))
  end=(grep("egment", tempfile)-start)[endL]-1
  LayerH<-read.fortran(filename, c(paste0(columns,"F8.0")), skip= start, n= (end-1))
  LayerH1<-pivot_longer(as.data.frame(LayerH),cols = starts_with("V"))
  
  # number of segments
  NumSeg<-length(na.omit(ifelse((grep("egment", tempfile)-start)<0,NA,(grep("egment", tempfile)-start))))

  # number of layers
  # dim(na.omit(LayerH1))[1]
  
  # Loop to extract and collect segment/layer widths
  for (i in c(1:NumSeg)){
    # start and end for layer widths
    loop_start=start+(1+end)*i

    # extracting layer widths
    SegWidth<-read.fortran(filename, c(paste0(columns,"F8.0")), skip= loop_start, n= (end-1))
    SegWidth_loop<-pivot_longer(as.data.frame(SegWidth),cols = starts_with("V"))
    colnames(SegWidth_loop)
    
    assign(paste0("SegWidth_",i) , SegWidth_loop$value)
    
    #combining layers into dataframe output
    if(i==1){
      loop_out <- data.frame(LAYERH =LayerH1$value)
      loop_out <- cbind(loop_out, SegWidth_loop$value)
      colnames(loop_out)[(i+1)]=i
    }else{
      loop_out <- cbind(loop_out, SegWidth_loop$value)
      colnames(loop_out)[(i+1)]=i
    }

  }
  
  # Adding K column and converting all to numeric
  loop_out$K<-row.names(loop_out)
  loop_out[] <- sapply(loop_out, as.numeric)
  
  # Removing Na Values at end
  loop_out<-na.omit(loop_out)
  attributes(loop_out)$na.action <- NULL
  
  return(loop_out)
}



ReadBathyW <- function(filename, columns_pass=10, headerlines_pass=1){
  require(stringr)
  
  if(str_detect(filename, ".csv$")){
    fun_out <- ReadBathyWidth(filename, headerlines = headerlines_pass)
    message("CSV file format dectected")
  }
  
  if(str_detect(filename, ".npt$")){
    fun_out <- ReadNPTBathyWidth(filename, columns = columns_pass)
    message("NPT file format dectected")
  }
  return(fun_out)
}
# test<-ReadBathyW(filename_test)


#### Read WSC.npt File ####

ReadWSC.NPT <- function(filename, columns=10, headerlines=3){
  #locate the existing WSC and read it into R
  tempfile<-read.fortran(filename, c(paste0(columns,"F8.0")), skip=headerlines)
  
  #extracting WSC values into long format table for each JDAY value
  temp_out<-data.frame(JDAY=NA,WSC=NA,Seg=NA)
  for(i in c(1:dim(tempfile)[1])){
    if(!is.na(tempfile[i,1])){
      WSC_loop<-as.vector(unlist(tempfile[i,-1]))
      Date_loop<-tempfile[i,1]
      loop_out<-NULL
    }else{
      WSC_loop<-append(WSC_loop,as.vector(unlist(tempfile[i,-1])))
    }
    if(!is.na(tempfile[(i+1),1]) | (i+1)>(dim(tempfile)[1])){
      loop_out<-data.frame(JDAY=Date_loop, WSC=WSC_loop)
      # Adding segment ID to table for editing
      loop_out$Seg=as.numeric(row.names(loop_out))
      temp_out<-rbind(temp_out,loop_out)
    }
  }
  # Removing 
  temp_out<-na.omit(temp_out)
  temp_out1<-data.frame(JDAY=temp_out$JDAY,WSC=temp_out$WSC,Seg=temp_out$Seg)
  return(temp_out1)
}



#### Write WSC.npt File ####
WriteWSC.NPT <- function(data, filename, filename_old, columns=10, headerlines=3, chr_width=8, date_digits=1, digits=2){
  require(stringr)
  #read in original file for header
  temp_in<-readLines(filename_old)
  
  #creating output object
  lines_out_loop2<-NULL
  out_columns<-(columns-1)
  
  #loop for each unique JDAY in file
  for(i in unique(data$JDAY)){
    loop_filter<-data[data$JDAY==i,]
    loop_lines<-length(loop_filter$WSC)/out_columns
    loop_string<-str_pad(as.character(sprintf(loop_filter$WSC, fmt = paste0("%0.",digits,"f"))), width = chr_width, side = "left", pad=" ")
    for(j in c(1:ceiling(loop_lines))){
      # loop_string[((j-1)*out_columns+1):((j-1)*out_columns+out_columns)]
      if(j==1){
        lines_out<-as.character(paste(c(str_pad(sprintf(i, fmt = paste0("%0.",date_digits,"f")), width = chr_width, side = "left", pad=" "),loop_string[((j-1)*out_columns+1):((j-1)*out_columns+out_columns)]), collapse = ""))
        lines_out_loop<-NULL
      }else{
        lines_out<-as.character(paste(c(rep(" ",chr_width),na.omit(loop_string[((j-1)*out_columns+1):((j-1)*out_columns+out_columns)])), collapse = ""))
      }
      lines_out_loop<-rbind(lines_out_loop,lines_out)
    }
    lines_out_loop2<-rbind(lines_out_loop2,lines_out_loop)
  }
  
  lines_out_loop2<-as.vector(lines_out_loop2)
  
  writeLines(c(as.character(temp_in[1:headerlines]),as.character(lines_out_loop2)), filename)
  
  return(paste("File written:",filename))
}

# out_columns=4
# for(j in 1:4){
#   print(((j-1)*out_columns+1):((j-1)*out_columns+out_columns))
# }




#### Read WSC.csv File ####

ReadWSC.CSV <- function(filename, headerlines=3){
  require(tidyr)
  tempfile<-read.csv(filename, header = FALSE)
  tempfile1<-tempfile[-c(1:(3)),]
  colnames(tempfile1)<- c("JDAY",as.vector(unlist(tempfile[c(3),-1])))

  # tempfile1$JDAY<-as.numeric(tempfile1$JDAY)
  # tempfile1$JDAY<-fill(tempfile1[,c(1,2)],JDAY, .direction = "down")[,1]
  # temp_out1<-pivot_longer(tempfile1,cols_vary = "fastest", cols = "WSC",names_to="Seg")
  # 
  # 
  # tempfile1$JDAY<-as.character(tempfile1$JDAY)
  # 
  # tempfile1[1,2] = 0.1
  # tempfile1[22,10] = 0.9
  
  temp_out1<-stats::reshape(tempfile1,direction = "long", varying = list(2:(dim(tempfile1)[2])),
                     timevar="Seg", v.names="WSC")
  temp_out2<-temp_out1[,c(1,3,2)]
  temp_out2$JDAY<-as.numeric(temp_out2$JDAY)  
  temp_out2$WSC<-as.numeric(temp_out2$WSC)
  temp_out2$Seg<-as.numeric(temp_out2$Seg)
  temp_out2<-temp_out2[order(temp_out2$JDAY,temp_out2$Seg),]
  row.names(temp_out2)<-c(1:length(temp_out2[,1]))
  return(temp_out2)
}



#### Write WSC.csv File ####
WriteWSC.CSV <- function(data, filename, filename_old, headerlines=3){
  tempfile_old<-read.csv(filename_old, header = FALSE)
  
  entries_n <- length(unique(data$Seg))+1
  tempfile_header<-tempfile_old[c(1:(headerlines)),c(1:entries_n)]
  
  lines_out<-stats::reshape(data, direction = "wide", idvar = "JDAY", timevar = "Seg", v.names = "WSC", sep="")
  
  colnames(lines_out)<-colnames(tempfile_header)
  
  lines_out2<-rbind(tempfile_header, lines_out)
  
  write.table(lines_out2, file = filename, row.names = FALSE, na="", col.names = FALSE, sep=',', quote=FALSE)
  return(paste("File written:",filename))
}






#### Read Shade.npt ####

ReadShade.NPT <- function(filename, columns=10, headerlines=3){
  #locate the existing Shade file and read it into R
  tempfile<-read.fortran(filename, c(paste0(columns,"F8.0")), skip=(headerlines))
  #include appropriate column headings
  columnread<-read.fwf(filename,widths = rep(8,columns))[headerlines,]
  colnames(tempfile)<-gsub(" ", "", columnread)
  return(tempfile)
}



#### Write Shade.npt ####
WriteShade.NPT <- function(data, filename, filename_old, columns=10, chr_width=8, headerlines=3, digits=2){
  require(stringr)
  #read in original file for header
  temp_in<-readLines(filename_old)
  data[,2]<-round(data[,2],2)
  
  lines_out_loop<-NULL
  lines_out_loop2<-NULL
  for(i in c(1:dim(data)[1])){
    seg<-str_pad(data[i,1], width = 8, side = "left", pad=" ")
    entries<-str_pad(as.character(sprintf(data[i,-1], fmt = paste0("%0.",digits,"f"))), width = chr_width, side = "left", pad=" ")
    lines_out_loop<-c(seg,entries)
    lines_out_loop<-gsub("NA", "  ", lines_out_loop)
    lines_out_loop1<-paste(lines_out_loop, sep = "", collapse = "")
    lines_out_loop1<-str_trim(lines_out_loop1, side = "right")
    lines_out_loop2<-rbind(lines_out_loop2,lines_out_loop1)
  }
  # lines_out_loop2
  lines_out_loop2<-as.vector(lines_out_loop2)
  
  writeLines(c(as.character(temp_in[1:headerlines]),as.character(lines_out_loop2)), filename)
  
  return(paste("File written:",filename))
}



#### Read Shade.csv ####
ReadShade.CSV <- function(filename, headerlines=3){
  tempfile<-read.csv(filename, header = FALSE)
  # remove header from file
  tempfile1<-tempfile[-c(1:(headerlines)),]
  #include appropriate column headings
  colnames(tempfile1)<- c(as.vector(unlist(tempfile[c(headerlines),])))
  # convert at least first 2 columns to numeric
  tempfile1[,1]<-as.numeric(tempfile1[,1])
  tempfile1[,2]<-as.numeric(tempfile1[,2])
  return(tempfile1)
}


#### Write Shade.csv ####
WriteShade.CSV <- function(data, filename, filename_old, headerlines=3){
  # Open original file for header
  tempfile_old<-read.csv(filename_old, header = FALSE)
  tempfile_header<-tempfile_old[c(1:(headerlines)),]
  
  lines_out<-data
  #match columns with header for merge
  colnames(lines_out)<-colnames(tempfile_header)
  #merge lines
  lines_out2<-rbind(tempfile_header, lines_out)
  #write out data
  write.table(lines_out2, file = filename, row.names = FALSE, na="", col.names = FALSE, sep=',', quote=FALSE)
  return(paste("File written:",filename))
}


#### End ####