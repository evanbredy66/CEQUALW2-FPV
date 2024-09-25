library(readr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(extrafont)
font_import()
loadfonts(device = "all")
fonts()

# Locations of W2 model directories
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


paste0(dirname(outfiles),"/iterate_temp/",basename(outfiles),".csv")


df<-NULL
df <- readr::read_csv(paste0(dirname(outfiles),"/iterate_temp/",basename(outfiles),".csv"))

# renaming reservoirs for consistency 
str(df)
unique(df$model_reservoir)
df$model_reservoir[df$model_reservoir=="Degray Reservoir"]="Degray Reservoir, Arkansas"
df$model_reservoir[df$model_reservoir=="J. Percy Priest reservoir"]="J. Percy Priest Reservoir, Tennessee"
df$model_reservoir[df$model_reservoir=="Mosquito Creek, OH"]="Mosquito Creek, Ohio"
df$model_reservoir[df$model_reservoir=="MJ Kirwan Lake, OH"]="MJ Kirwan Lake, Ohio"
df$model_reservoir[df$model_reservoir=="Berlin, OH"]="Berlin Lake, Ohio"
df$model_reservoir[df$model_reservoir=="Milton Lake, OH"]="Milton Lake, Ohio"
df$model_reservoir[df$model_reservoir=="Dworshak Pool, ID"]="Dworshak Pool, Idaho"
df$model_reservoir[df$model_reservoir=="Green Peter Reservoir, OR"]="Green Peter Reservoir, Oregon"
df$model_reservoir[df$model_reservoir=="Foster Reservoir, OR"]="Foster Reservoir, Oregon"

graph_out_file<-"C:/...../ResponseGraphs"

df$model_reservoir<-as.factor(df$model_reservoir)
  
levels(as.factor(df$ResponseVar))
str(df)

levels(df$model_reservoir)
sort_order<-df %>%
  group_by(model_reservoir) %>%
  mutate(model_reservoir_order = as.numeric(model_reservoir)) %>%
  summarise(area = max(FPV_covered_area),
            model_reservoir_order= mean(model_reservoir_order)) %>%
  arrange(area) %>%
  dplyr::select(model_reservoir) %>% c() %>% unlist
as.character(sort_order)

df$model_reservoir <- factor(df$model_reservoir, levels = as.character(sort_order))



seasons_names <- list(
  'winter'="Jan & Feb",
  'summer'="July & Aug"
)
  
  season_labeller <- function(variable,value){
    return(seasons_names[value])
  }

  My_Theme = theme( axis.text = element_text( size = 16 , family = "Arial"),
                    axis.text.x = element_text( size = 16 , family = "Arial"),
                    axis.title = element_text(size=20, family = "Arial"),
                    legend.title  = element_text( size = 16, face = "bold" , family = "Arial"),
                    legend.text = element_text(size=14, family = "Arial"),
                    strip.text = element_text(size = 20, family = "Arial"))

  
head(df, n=20)



#adjusting DO min and DO max calculation to change in mg/L
unique(df$ResponseVar)
df1<-df %>%
  dplyr::filter(ResponseVar=="do.min",
         ResType=="value") %>%
  group_by(model_reservoir, Season, Location) %>%
  mutate(do.ref = first(RespValue),
         RespValue = RespValue-do.ref,
         ResType = "change") %>%
  dplyr::select(-do.ref) %>%
  dplyr::filter(Iteration!="0")

df2<-df %>%
  dplyr::filter(ResponseVar=="do.max",
                ResType=="value") %>%
  group_by(model_reservoir, Season, Location) %>%
  mutate(do.ref = first(RespValue),
         RespValue = RespValue-do.ref,
         ResType = "change") %>%
  dplyr::select(-do.ref) %>%
  dplyr::filter(Iteration!="0")

df3<-df %>%
  dplyr::filter(ResponseVar=="range.outflow.temp",
                ResType=="value") %>%
  group_by(model_reservoir, Season, Location) %>%
  mutate(val.ref = first(RespValue),
         RespValue = RespValue-val.ref,
         ResType = "change") %>%
  dplyr::select(-val.ref) %>%
  dplyr::filter(Iteration!="0")

df4<-df %>%
  dplyr::filter(ResponseVar=="temp.max",
                ResType=="value") %>%
  group_by(model_reservoir, Season, Location) %>%
  mutate(val.ref = first(RespValue),
         RespValue = RespValue-val.ref,
         ResType = "change") %>%
  dplyr::select(-val.ref) %>%
  dplyr::filter(Iteration!="0")

df5<-df %>%
  dplyr::filter(ResponseVar=="temp.min",
                ResType=="value") %>%
  group_by(model_reservoir, Season, Location) %>%
  mutate(val.ref = first(RespValue),
         RespValue = RespValue-val.ref,
         ResType = "change") %>%
  dplyr::select(-val.ref) %>%
  dplyr::filter(Iteration!="0")

df_out<-df %>%
  dplyr::filter(!(ResponseVar=="do.min" & ResType=="percent_change")) %>%
  dplyr::filter(!(ResponseVar=="do.max" & ResType=="percent_change")) %>%
  dplyr::filter(!(ResponseVar=="range.outflow.temp" & ResType=="percent_change")) %>%
  dplyr::filter(!(ResponseVar=="temp.max" & ResType=="percent_change")) %>%
  dplyr::filter(!(ResponseVar=="temp.min" & ResType=="percent_change"))
View(df_out)

df_out<-rbind(df_out,df1,df2,df3,df4,df5) %>%
  group_by(model_reservoir) %>%
  arrange(Iteration, .by_group = TRUE)

write.csv(df_out, paste0(graph_out_file,"/all_resp_backup_Aug24.csv"))

# filtering 'value' from data
df_out2<- df_out %>%
  dplyr::filter(Iteration!="0") %>%
  dplyr::filter(ResType!="value")

df_out_v<- df_out %>%
  # dplyr::filter(Iteration!="0") %>%
  dplyr::filter(ResType=="value")

View(df_out2)
View(df_out_v)

write.csv(df_out2, "C:/...../backup/all_res_output.csv")
write.csv(df_out2, paste0(graph_out_file,"/all_resp_change.csv"))
# df<-read.csv( "C:/...../all_res_output.csv")



for(i in c(0.05, 0.20, 0.35, 0.50)){
  percent_off = i
  df_loop<-df %>%
    # dplyr::filter(model_reservoir == "J. Percy Priest Reservoir, Tennessee") %>% 
    mutate(per_off_v=abs(percentFPV-percent_off))%>%
    # arrange(model_reservoir,per_off_v) %>%
    group_by(model_reservoir)%>%
    filter(per_off_v == min(per_off_v),
           Location!="center",
           ResType=="value") %>%
    group_by(ResponseVar, Season)%>%
    summarise(n = length(na.omit(RespValue)),
              Mean_Resp=mean(RespValue, na.rm = TRUE),
              SE_Resp=(sd(RespValue, na.rm = TRUE)/sqrt(length(na.omit(RespValue))))) %>%
    arrange(Season)
  colnames(df_loop)[4]<-paste0("Mean_",percent_off)
  colnames(df_loop)[5]<-paste0("SE_",percent_off)
  if(i==0.05){
    df_loop_out_value<-df_loop
  }else{
    df_loop_out_value<-cbind(df_loop_out_value,df_loop[,c(4:5)])
  }
}
View(df_loop_out_value)
write.csv(df_loop_out_value, paste0(graph_out_file,"/sum_stat_value.csv"))

for(i in c(0.05, 0.1,0.15,0.2, 0.25, 0.3, 0.35, 0.4)){
  percent_off = i
  df_loop<-df %>%
    dplyr::filter(model_reservoir == "J. Percy Priest Reservoir, Tennessee") %>% 
    mutate(per_off_v=abs(percentFPV-percent_off))%>%
    # arrange(model_reservoir,per_off_v) %>%
    group_by(model_reservoir)%>%
    filter(per_off_v == min(per_off_v),
           Location!="center",
           ResType=="value") %>%
    group_by(ResponseVar, Season)%>%
    summarise(n = length(na.omit(RespValue)),
              Mean_Resp=mean(RespValue, na.rm = TRUE),
              SE_Resp=(sd(RespValue, na.rm = TRUE)/sqrt(length(na.omit(RespValue))))) %>%
    arrange(Season)
  colnames(df_loop)[4]<-paste0("Mean_",percent_off)
  colnames(df_loop)[5]<-paste0("SE_",percent_off)
  if(i==0.05){
    df_loop_out_value<-df_loop
  }else{
    df_loop_out_value<-cbind(df_loop_out_value,df_loop[,c(4:5)])
  }
}
View(df_loop_out_value)

write.csv(df_loop_out_value, paste0(graph_out_file,"/JPPriest_stat_value.csv"))

for(i in c(0.05, 0.20, 0.35, 0.50)){
  percent_off = i
  df_loop<-df_out2 %>%
    mutate(per_off_v=abs(percentFPV-percent_off))%>%
    # arrange(model_reservoir,per_off_v) %>%
    group_by(model_reservoir)%>%
    filter(per_off_v == min(per_off_v),
           Location!="center",
           ResType!="value") %>%
    group_by(ResponseVar, Season)%>%
    summarise(n = length(na.omit(RespValue)),
              Mean_Resp=mean(RespValue, na.rm = TRUE),
              SE_Resp=(sd(RespValue, na.rm = TRUE)/sqrt(length(na.omit(RespValue))))) %>%
    arrange(Season)
  colnames(df_loop)[4]<-paste0("Mean_",percent_off)
  colnames(df_loop)[5]<-paste0("SE_",percent_off)
  if(i==0.05){
    df_loop_out<-df_loop
  }else{
    df_loop_out<-cbind(df_loop_out,df_loop[,c(4:5)])
  }
}
View(df_loop_out)
write.csv(df_loop_out, paste0(graph_out_file,"/sum_stat_perc.csv"))

for(i in c(0.05, 0.1,0.15,0.2, 0.25, 0.3, 0.35, 0.4)){
  percent_off = i
  df_loop<-df_out2 %>%
    dplyr::filter(model_reservoir == "J. Percy Priest Reservoir, Tennessee") %>% 
    mutate(per_off_v=abs(percentFPV-percent_off))%>%
    # arrange(model_reservoir,per_off_v) %>%
    group_by(model_reservoir)%>%
    filter(per_off_v == min(per_off_v),
           Location!="center",
           ResType!="value") %>%
    group_by(ResponseVar, Season)%>%
    summarise(n = length(na.omit(RespValue)),
              Mean_Resp=mean(RespValue, na.rm = TRUE),
              SE_Resp=(sd(RespValue, na.rm = TRUE)/sqrt(length(na.omit(RespValue))))) %>%
    arrange(Season)
  colnames(df_loop)[4]<-paste0("Mean_",percent_off)
  colnames(df_loop)[5]<-paste0("SE_",percent_off)
  if(i==0.05){
    df_loop_out<-df_loop
  }else{
    df_loop_out<-cbind(df_loop_out,df_loop[,c(4:5)])
  }
}
View(df_loop_out)
write.csv(df_loop_out, paste0(graph_out_file,"/JPPriest_stat_perc.csv"))

# df<- df%>%
#   dplyr::filter(model_reservoir == "J. Percy Priest Reservoir, Tennessee")



#### plots ####
df %>%   group_by(model_reservoir) %>%
  mutate(model_reservoir=as.factor(paste0(as.double(model_reservoir),"-",model_reservoir))) %>%
  # filter(Season == "winter") %>%
  filter(Location =="outflow", ResType=="percent_change") %>%
  filter(ResponseVar=="stratified.days") %>%
  mutate(RespValuePerc= RespValue *100,
         percentFPV = percentFPV *100)%>%
  mutate (label = ifelse(percentFPV == max(percentFPV), (model_reservoir), NA_character_)) %>%
  ggplot(aes(x=percentFPV, y=RespValuePerc, color=model_reservoir))+
  facet_wrap("Season", labeller = season_labeller)+ # scales = "free_y",
  geom_line(linewidth=0.9, alpha=1,show.legend = TRUE)+
  labs(x="Percent Coverage of Floating PV", y="Percent Change in Days Stratified", color ="Reservoir Model")+
  geom_label_repel(aes(label = label), force = 1, force_pull = 1, show.legend = FALSE,xlim = c(5, NA),
                   max.overlaps = 50,nudge_x = 13,
                   na.rm = TRUE)+
  scale_x_continuous(breaks = c(0,25,50,75,100),expand = expansion(add=c(3,15)))+
  theme_bw()+My_Theme+ # scale_y_continuous(trans= 'pseudo_log')
  geom_smooth(data = . %>% group_by(percentFPV, Season) %>% summarise(RespValuePerc=mean(RespValuePerc)), color="black", linewidth=1.5,se = TRUE)
ggsave(paste0(graph_out_file,"/fig1.png"), dpi = 200, width = 16, height = 8)


df %>%   group_by(model_reservoir) %>%
  mutate(model_reservoir=as.factor(paste0(as.double(model_reservoir),"-",model_reservoir))) %>%
  # filter(Season == "winter") %>%
  filter(Location =="outflow", ResType=="percent_change") %>%
  filter(ResponseVar=="do.mean") %>%
  mutate(RespValuePerc= RespValue *100,
         percentFPV = percentFPV *100)%>%
  mutate (label = ifelse(percentFPV == max(percentFPV), (model_reservoir), NA_character_)) %>%
  ggplot(aes(x=percentFPV, y=RespValuePerc, color=model_reservoir))+
  facet_wrap("Season", labeller = season_labeller)+ # scales = "free_y",
  geom_line(linewidth=0.9, alpha=1,show.legend = TRUE)+
  labs(x="Percent Coverage of Floating PV", y="Percent Change in Mean Profile Dissolved O2 at Outflow", color ="Reservoir Model")+
  geom_label_repel(aes(label = label), force = 1, force_pull = 1, show.legend = FALSE,xlim = c(5, NA),
                   max.overlaps = 50,nudge_x = 13,
                   na.rm = TRUE)+
  scale_x_continuous(breaks = c(0,25,50,75,100),expand = expansion(add=c(3,15)))+
  theme_bw()+My_Theme+ # scale_y_continuous(trans= 'pseudo_log')
  geom_smooth(data = . %>% group_by(percentFPV, Season) %>% summarise(RespValuePerc=mean(RespValuePerc)), color="black", linewidth=1.5,se = TRUE)

ggsave(paste0(graph_out_file,"/fig1A.png"), dpi = 200, width = 16, height = 8)


df %>%   group_by(model_reservoir) %>%
  mutate(model_reservoir=as.factor(paste0(as.double(model_reservoir),"-",model_reservoir))) %>%
  # filter(Season == "winter") %>%
  filter(Location =="outflow", ResType=="percent_change") %>%
  filter(ResponseVar=="thermocline.depth") %>%
  mutate(RespValuePerc= RespValue *100,
         percentFPV = percentFPV *100)%>%
  mutate (label = ifelse(percentFPV == max(percentFPV), (model_reservoir), NA_character_)) %>%
  ggplot(aes(x=percentFPV, y=RespValuePerc, color=model_reservoir))+
  facet_wrap("Season", labeller = season_labeller)+ # scales = "free_y",
  geom_line(linewidth=0.9, alpha=1,show.legend = TRUE)+
  labs(x="Percent Coverage of Floating PV", y="Percent Change in Thermocline Depth", color ="Reservoir Model")+
  geom_label_repel(aes(label = label), force = 1, force_pull = 1, show.legend = FALSE,xlim = c(5, NA),
                   max.overlaps = 50,nudge_x = 13,
                   na.rm = TRUE)+
  scale_x_continuous(breaks = c(0,25,50,75,100),expand = expansion(add=c(3,15)))+
  theme_bw()+My_Theme+ # scale_y_continuous(trans= 'pseudo_log')
  geom_smooth(data = . %>% group_by(percentFPV, Season) %>% summarise(RespValuePerc=mean(RespValuePerc)), color="black", linewidth=1.5,se = TRUE)

ggsave(paste0(graph_out_file,"/fig2.png"), dpi = 200, width = 16, height = 8)


df %>%   group_by(model_reservoir) %>%
  mutate(model_reservoir=as.factor(paste0(as.double(model_reservoir),"-",model_reservoir))) %>%
  # filter(Season == "winter") %>%
  filter(Location =="full", ResType=="percent_change") %>%
  filter(ResponseVar=="mean.surface.temp") %>%
  mutate(RespValuePerc= RespValue *100,
         percentFPV = percentFPV *100)%>%
  mutate (label = ifelse(percentFPV == max(percentFPV), (model_reservoir), NA_character_)) %>%
  ggplot(aes(x=percentFPV, y=RespValuePerc, color=model_reservoir))+
  facet_wrap("Season", labeller = season_labeller)+ # scales = "free_y",
  geom_line(linewidth=0.9, alpha=1,show.legend = TRUE)+
  labs(x="Percent Coverage of Floating PV", y="Percent Change in Mean Surface Temperature", color ="Reservoir Model")+
  geom_label_repel(aes(label = label), force = 1, force_pull = 1, show.legend = FALSE,xlim = c(5, NA),
                   max.overlaps = 50,nudge_x = 13,
                   na.rm = TRUE)+
  scale_x_continuous(breaks = c(0,25,50,75,100),expand = expansion(add=c(3,15)))+
  theme_bw()+My_Theme+ # scale_y_continuous(trans= 'pseudo_log')
  geom_smooth(data = . %>% group_by(percentFPV, Season) %>% summarise(RespValuePerc=mean(RespValuePerc)), color="black", linewidth=1.5,se = TRUE)

ggsave(paste0(graph_out_file,"/fig3.png"), dpi = 200, width = 16, height = 8)



df %>%   group_by(model_reservoir) %>%
  mutate(model_reservoir=as.factor(paste0(as.double(model_reservoir),"-",model_reservoir))) %>%
  # filter(Season == "winter") %>%
  filter(Location =="full", ResType=="percent_change") %>%
  filter(ResponseVar=="mean.outflow.temp") %>%
  mutate(RespValuePerc= RespValue *100,
         percentFPV = percentFPV *100)%>%
  mutate (label = ifelse(percentFPV == max(percentFPV), (model_reservoir), NA_character_)) %>%
  ggplot(aes(x=percentFPV, y=RespValuePerc, color=model_reservoir))+
  facet_wrap("Season", labeller = season_labeller)+ # scales = "free_y",
  geom_line(linewidth=0.9, alpha=1,show.legend = TRUE)+
  labs(x="Percent Coverage of Floating PV", y="Percent Change in Mean Outflow Temperature", color ="Reservoir Model")+
  geom_label_repel(aes(label = label), force = 1, force_pull = 1, show.legend = FALSE,xlim = c(5, NA),
                   max.overlaps = 50,nudge_x = 13,
                   na.rm = TRUE)+
  scale_x_continuous(breaks = c(0,25,50,75,100),expand = expansion(add=c(3,15)))+
  theme_bw()+My_Theme+ # scale_y_continuous(trans= 'pseudo_log')
  geom_smooth(data = . %>% group_by(percentFPV, Season) %>% summarise(RespValuePerc=mean(RespValuePerc)), color="black", linewidth=1.5,se = TRUE)

ggsave(paste0(graph_out_file,"/fig4.png"), dpi = 200, width = 16, height = 8)


df %>%   group_by(model_reservoir) %>%
  mutate(model_reservoir=as.factor(paste0(as.double(model_reservoir),"-",model_reservoir))) %>%
  # filter(Season == "winter") %>%
  filter(Location =="outflow", ResType=="percent_change") %>%
  filter(ResponseVar=="schmidt.stability") %>%
  mutate(RespValuePerc= RespValue *100,
         percentFPV = percentFPV *100)%>%
  mutate (label = ifelse(percentFPV == max(percentFPV), (model_reservoir), NA_character_)) %>%
  ggplot(aes(x=percentFPV, y=RespValuePerc, color=model_reservoir))+
  facet_wrap("Season", labeller = season_labeller)+ # scales = "free_y",
  geom_line(linewidth=0.9, alpha=1,show.legend = TRUE)+
  labs(x="Percent Coverage of Floating PV", y="Percent Change in Schmidt Stability Index", color ="Reservoir Model")+
  geom_label_repel(aes(label = label), force = 1, force_pull = 1, show.legend = FALSE,xlim = c(5, NA),
                   max.overlaps = 50,nudge_x = 13,
                   na.rm = TRUE)+
  scale_x_continuous(breaks = c(0,25,50,75,100),expand = expansion(add=c(3,15)))+
  theme_bw()+My_Theme+ # scale_y_continuous(trans= 'pseudo_log')
  geom_smooth(data = . %>% group_by(percentFPV, Season) %>% summarise(RespValuePerc=mean(RespValuePerc)), color="black", linewidth=1.5,se = TRUE)

ggsave(paste0(graph_out_file,"/fig5.png"), dpi = 200, width = 16, height = 8)


df %>%   group_by(model_reservoir) %>%
  mutate(model_reservoir=as.factor(paste0(as.double(model_reservoir),"-",model_reservoir))) %>%
  # filter(Season == "winter") %>%
  filter(Location =="outflow", ResType=="percent_change") %>%
  filter(ResponseVar=="temp.mean") %>%
  mutate(RespValuePerc= RespValue *100,
         percentFPV = percentFPV *100)%>%
  mutate (label = ifelse(percentFPV == max(percentFPV), (model_reservoir), NA_character_)) %>%
  ggplot(aes(x=percentFPV, y=RespValuePerc, color=model_reservoir))+
  facet_wrap("Season", labeller = season_labeller)+ # scales = "free_y",
  geom_line(linewidth=0.9, alpha=1,show.legend = TRUE)+
  labs(x="Percent Coverage of Floating PV", y="Percent Change in Mean Profile Temp at Outflow", color ="Reservoir Model")+
  geom_label_repel(aes(label = label), force = 1, force_pull = 1, show.legend = FALSE,xlim = c(5, NA),
                   max.overlaps = 50,nudge_x = 13,
                   na.rm = TRUE)+
  scale_x_continuous(breaks = c(0,25,50,75,100),expand = expansion(add=c(3,15)))+
  theme_bw()+My_Theme+ # scale_y_continuous(trans= 'pseudo_log')
  geom_smooth(data = . %>% group_by(percentFPV, Season) %>% summarise(RespValuePerc=mean(RespValuePerc)), color="black", linewidth=1.5,se = TRUE)

ggsave(paste0(graph_out_file,"/fig6.png"), dpi = 200, width = 16, height = 8)



DO_list <- df %>%   group_by(model_reservoir) %>%
  # mutate(model_reservoir=as.factor(paste0(as.double(model_reservoir),"-",model_reservoir))) %>%
  # filter(Season == "winter") %>%
  filter(Location =="outflow", ResType=="percent_change") %>%
  filter(ResponseVar=="do.mean") %>%
  summarise(DO_res=is.na(RespValue)) %>%
  filter(DO_res==FALSE) %>%
  dplyr::select(model_reservoir) %>%
  unique()
# DO_list

df %>%  group_by(model_reservoir) %>%
  mutate(DO_res= model_reservoir %in% c(DO_list)$model_reservoir) %>% 
  mutate(model_reservoir=as.factor(paste0(as.double(model_reservoir),"-",model_reservoir))) %>%
  filter(Location =="full", ResType=="percent_change") %>%
  filter(ResponseVar=="warmwater.fish" & DO_res==TRUE) %>%
  mutate(RespValuePerc= RespValue,
         percentFPV = percentFPV *100)%>%
  mutate (label = ifelse(percentFPV == max(percentFPV), (model_reservoir), NA_character_)) %>% 
  ggplot(aes(x=percentFPV, y=RespValuePerc, color=model_reservoir))+
  facet_wrap("Season", labeller = season_labeller)+ # scales = "free_y",
  geom_line(linewidth=0.9, alpha=1,show.legend = TRUE)+
  labs(x="Percent Coverage of Floating PV", y="Percent Change in Warmwater Fish Habitat", color ="Reservoir Model")+
  geom_label_repel(aes(label = label), force = 1, force_pull = 1, show.legend = FALSE,xlim = c(5, NA),
                   max.overlaps = 50,nudge_x = 13,
                   na.rm = TRUE)+
  scale_x_continuous(breaks = c(0,25,50,75,100),expand = expansion(add=c(3,15)))+
  theme_bw()+My_Theme+ # scale_y_continuous(trans= 'pseudo_log')
  geom_smooth(data = . %>% group_by(percentFPV, Season) %>% summarise(RespValuePerc=mean(RespValuePerc)), color="black", linewidth=1.5,se = TRUE)

ggsave(paste0(graph_out_file,"/fig7.png"), dpi = 200, width = 16, height = 8)


df %>%  group_by(model_reservoir) %>%
  mutate(DO_res= model_reservoir %in% c(DO_list)$model_reservoir) %>% 
  mutate(model_reservoir=as.factor(paste0(as.double(model_reservoir),"-",model_reservoir))) %>%
  filter(Location =="full", ResType=="percent_change") %>%
  filter(ResponseVar=="coldwater.fish" & DO_res==TRUE) %>%
  mutate(RespValuePerc= RespValue,
         percentFPV = percentFPV *100)%>%
  mutate (label = ifelse(percentFPV == max(percentFPV), (model_reservoir), NA_character_)) %>% 
  ggplot(aes(x=percentFPV, y=RespValuePerc, color=model_reservoir))+
  facet_wrap("Season", labeller = season_labeller)+ # scales = "free_y",
  geom_line(linewidth=0.9, alpha=1,show.legend = TRUE)+
  labs(x="Percent Coverage of Floating PV", y="Percent Change in Coldwater Fish Habitat", color ="Reservoir Model")+
  geom_label_repel(aes(label = label), force = 1, force_pull = 1, show.legend = FALSE,xlim = c(5, NA),
                   max.overlaps = 50,nudge_x = 13,
                   na.rm = TRUE)+
  scale_x_continuous(breaks = c(0,25,50,75,100),expand = expansion(add=c(3,15)))+
  theme_bw()+My_Theme+ # scale_y_continuous(trans= 'pseudo_log')
  geom_smooth(data = . %>% group_by(percentFPV, Season) %>% summarise(RespValuePerc=mean(RespValuePerc)), color="black", linewidth=1.5,se = TRUE)

ggsave(paste0(graph_out_file,"/fig8.png"), dpi = 200, width = 16, height = 8)



#### empty plots ####
df %>%   group_by(model_reservoir) %>%
  mutate(model_reservoir=as.factor(paste0(as.double(model_reservoir),"-",model_reservoir))) %>%
  # filter(Season == "winter") %>%
  filter(Location =="outflow", ResType=="percent_change") %>%
  filter(ResponseVar=="stratified.days") %>%
  mutate(RespValuePerc= RespValue *100,
         percentFPV = percentFPV *100)%>%
  mutate (label = ifelse(percentFPV == max(percentFPV), (model_reservoir), NA_character_)) %>%
  ggplot(aes(x=percentFPV, y=RespValuePerc, color=model_reservoir))+
  facet_wrap("Season", labeller = season_labeller)+ # scales = "free_y",
  geom_line(linewidth=0.9, alpha=0,show.legend = TRUE)+
  labs(x="Percent Coverage of Floating PV", y="Percent Change in Days Stratified", color ="Reservoir Model")+
  geom_label_repel(aes(label = label), force = 1, alpha=0,force_pull = 1, show.legend = FALSE,xlim = c(5, NA),
                   max.overlaps = 50,nudge_x = 13,
                   na.rm = TRUE)+
  scale_x_continuous(breaks = c(0,25,50,75,100),expand = expansion(add=c(3,15)))+
  theme_bw()+My_Theme#+ # scale_y_continuous(trans= 'pseudo_log')
  # geom_smooth(data = . %>% group_by(percentFPV, Season) %>% summarise(RespValuePerc=mean(RespValuePerc)),color="black", linewidth=1.5,se = TRUE)
ggsave(paste0(graph_out_file,"/fig1_n.png"), dpi = 200, width = 16, height = 8)


df %>%   group_by(model_reservoir) %>%
  mutate(model_reservoir=as.factor(paste0(as.double(model_reservoir),"-",model_reservoir))) %>%
  # filter(Season == "winter") %>%
  filter(Location =="outflow", ResType=="percent_change") %>%
  filter(ResponseVar=="do.mean") %>%
  mutate(RespValuePerc= RespValue *100,
         percentFPV = percentFPV *100)%>%
  mutate (label = ifelse(percentFPV == max(percentFPV), (model_reservoir), NA_character_)) %>%
  ggplot(aes(x=percentFPV, y=RespValuePerc, color=model_reservoir))+
  facet_wrap("Season", labeller = season_labeller)+ # scales = "free_y",
  geom_line(linewidth=0.9, alpha=0,show.legend = TRUE)+
  labs(x="Percent Coverage of Floating PV", y="Percent Change in Mean Profile Dissolved O2 at Outflow", color ="Reservoir Model")+
  geom_label_repel(aes(label = label), force = 1, force_pull = 1, show.legend = FALSE,xlim = c(5, NA),
                   max.overlaps = 50,nudge_x = 13, alpha=0,
                   na.rm = TRUE)+
  scale_x_continuous(breaks = c(0,25,50,75,100),expand = expansion(add=c(3,15)))+
  theme_bw()+My_Theme#+ # scale_y_continuous(trans= 'pseudo_log')
  # geom_smooth(data = . %>% group_by(percentFPV, Season) %>% summarise(RespValuePerc=mean(RespValuePerc)), color="black", linewidth=1.5,se = TRUE)

ggsave(paste0(graph_out_file,"/fig1A_n.png"), dpi = 200, width = 16, height = 8)


df %>%   group_by(model_reservoir) %>%
  mutate(model_reservoir=as.factor(paste0(as.double(model_reservoir),"-",model_reservoir))) %>%
  # filter(Season == "winter") %>%
  filter(Location =="outflow", ResType=="percent_change") %>%
  filter(ResponseVar=="thermocline.depth") %>%
  mutate(RespValuePerc= RespValue *100,
         percentFPV = percentFPV *100)%>%
  mutate (label = ifelse(percentFPV == max(percentFPV), (model_reservoir), NA_character_)) %>%
  ggplot(aes(x=percentFPV, y=RespValuePerc, color=model_reservoir))+
  facet_wrap("Season", labeller = season_labeller)+ # scales = "free_y",
  geom_line(linewidth=0.9, alpha=0,show.legend = TRUE)+
  labs(x="Percent Coverage of Floating PV", y="Percent Change in Thermocline Depth", color ="Reservoir Model")+
  geom_label_repel(aes(label = label), force = 1, force_pull = 1, show.legend = FALSE,xlim = c(5, NA),
                   max.overlaps = 50,nudge_x = 13,alpha=0,
                   na.rm = TRUE)+
  scale_x_continuous(breaks = c(0,25,50,75,100),expand = expansion(add=c(3,15)))+
  theme_bw()+My_Theme#+ # scale_y_continuous(trans= 'pseudo_log')
  # geom_smooth(data = . %>% group_by(percentFPV, Season) %>% summarise(RespValuePerc=mean(RespValuePerc)), color="black", linewidth=1.5,se = TRUE)

ggsave(paste0(graph_out_file,"/fig2_n.png"), dpi = 200, width = 16, height = 8)


df %>%   group_by(model_reservoir) %>%
  mutate(model_reservoir=as.factor(paste0(as.double(model_reservoir),"-",model_reservoir))) %>%
  # filter(Season == "winter") %>%
  filter(Location =="full", ResType=="percent_change") %>%
  filter(ResponseVar=="mean.surface.temp") %>%
  mutate(RespValuePerc= RespValue *100,
         percentFPV = percentFPV *100)%>%
  mutate (label = ifelse(percentFPV == max(percentFPV), (model_reservoir), NA_character_)) %>%
  ggplot(aes(x=percentFPV, y=RespValuePerc, color=model_reservoir))+
  facet_wrap("Season", labeller = season_labeller)+ # scales = "free_y",
  geom_line(linewidth=0.9, alpha=0,show.legend = TRUE)+
  labs(x="Percent Coverage of Floating PV", y="Percent Change in Mean Surface Temperature", color ="Reservoir Model")+
  geom_label_repel(aes(label = label), force = 1, force_pull = 1, show.legend = FALSE,xlim = c(5, NA),
                   max.overlaps = 50,nudge_x = 13,alpha=0,
                   na.rm = TRUE)+
  scale_x_continuous(breaks = c(0,25,50,75,100),expand = expansion(add=c(3,15)))+
  theme_bw()+My_Theme#+ # scale_y_continuous(trans= 'pseudo_log')
  # geom_smooth(data = . %>% group_by(percentFPV, Season) %>% summarise(RespValuePerc=mean(RespValuePerc)), color="black", linewidth=1.5,se = TRUE)

ggsave(paste0(graph_out_file,"/fig3_n.png"), dpi = 200, width = 16, height = 8)



df %>%   group_by(model_reservoir) %>%
  mutate(model_reservoir=as.factor(paste0(as.double(model_reservoir),"-",model_reservoir))) %>%
  # filter(Season == "winter") %>%
  filter(Location =="full", ResType=="percent_change") %>%
  filter(ResponseVar=="mean.outflow.temp") %>%
  mutate(RespValuePerc= RespValue *100,
         percentFPV = percentFPV *100)%>%
  mutate (label = ifelse(percentFPV == max(percentFPV), (model_reservoir), NA_character_)) %>%
  ggplot(aes(x=percentFPV, y=RespValuePerc, color=model_reservoir))+
  facet_wrap("Season", labeller = season_labeller)+ # scales = "free_y",
  geom_line(linewidth=0.9, alpha=0,show.legend = TRUE)+
  labs(x="Percent Coverage of Floating PV", y="Percent Change in Mean Outflow Temperature", color ="Reservoir Model")+
  geom_label_repel(aes(label = label), force = 1, force_pull = 1, show.legend = FALSE,xlim = c(5, NA),
                   max.overlaps = 50,nudge_x = 13,alpha=0,
                   na.rm = TRUE)+
  scale_x_continuous(breaks = c(0,25,50,75,100),expand = expansion(add=c(3,15)))+
  theme_bw()+My_Theme#+ # scale_y_continuous(trans= 'pseudo_log')
  # geom_smooth(data = . %>% group_by(percentFPV, Season) %>% summarise(RespValuePerc=mean(RespValuePerc)), color="black", linewidth=1.5,se = TRUE)

ggsave(paste0(graph_out_file,"/fig4_n.png"), dpi = 200, width = 16, height = 8)


df %>%   group_by(model_reservoir) %>%
  mutate(model_reservoir=as.factor(paste0(as.double(model_reservoir),"-",model_reservoir))) %>%
  # filter(Season == "winter") %>%
  filter(Location =="outflow", ResType=="percent_change") %>%
  filter(ResponseVar=="schmidt.stability") %>%
  mutate(RespValuePerc= RespValue *100,
         percentFPV = percentFPV *100)%>%
  mutate (label = ifelse(percentFPV == max(percentFPV), (model_reservoir), NA_character_)) %>%
  ggplot(aes(x=percentFPV, y=RespValuePerc, color=model_reservoir))+
  facet_wrap("Season", labeller = season_labeller)+ # scales = "free_y",
  geom_line(linewidth=0.9, alpha=0,show.legend = TRUE)+
  labs(x="Percent Coverage of Floating PV", y="Percent Change in Schmidt Stability Index", color ="Reservoir Model")+
  geom_label_repel(aes(label = label), force = 1, force_pull = 1, show.legend = FALSE,xlim = c(5, NA),
                   max.overlaps = 50,nudge_x = 13,alpha=0,
                   na.rm = TRUE)+
  scale_x_continuous(breaks = c(0,25,50,75,100),expand = expansion(add=c(3,15)))+
  theme_bw()+My_Theme#+ # scale_y_continuous(trans= 'pseudo_log')
  # geom_smooth(data = . %>% group_by(percentFPV, Season) %>% summarise(RespValuePerc=mean(RespValuePerc)), color="black", linewidth=1.5,se = TRUE)

ggsave(paste0(graph_out_file,"/fig5_n.png"), dpi = 200, width = 16, height = 8)


df %>%   group_by(model_reservoir) %>%
  mutate(model_reservoir=as.factor(paste0(as.double(model_reservoir),"-",model_reservoir))) %>%
  # filter(Season == "winter") %>%
  filter(Location =="outflow", ResType=="percent_change") %>%
  filter(ResponseVar=="temp.mean") %>%
  mutate(RespValuePerc= RespValue *100,
         percentFPV = percentFPV *100)%>%
  mutate (label = ifelse(percentFPV == max(percentFPV), (model_reservoir), NA_character_)) %>%
  ggplot(aes(x=percentFPV, y=RespValuePerc, color=model_reservoir))+
  facet_wrap("Season", labeller = season_labeller)+ # scales = "free_y",
  geom_line(linewidth=0.9, alpha=0,show.legend = TRUE)+
  labs(x="Percent Coverage of Floating PV", y="Percent Change in Mean Profile Temp at Outflow", color ="Reservoir Model")+
  geom_label_repel(aes(label = label), force = 1, force_pull = 1, show.legend = FALSE,xlim = c(5, NA),
                   max.overlaps = 50,nudge_x = 13,alpha=0,
                   na.rm = TRUE)+
  scale_x_continuous(breaks = c(0,25,50,75,100),expand = expansion(add=c(3,15)))+
  theme_bw()+My_Theme#+ # scale_y_continuous(trans= 'pseudo_log')
  # geom_smooth(data = . %>% group_by(percentFPV, Season) %>% summarise(RespValuePerc=mean(RespValuePerc)), color="black", linewidth=1.5,se = TRUE)

ggsave(paste0(graph_out_file,"/fig6_n.png"), dpi = 200, width = 16, height = 8)



DO_list <- df %>%   group_by(model_reservoir) %>%
  # mutate(model_reservoir=as.factor(paste0(as.double(model_reservoir),"-",model_reservoir))) %>%
  # filter(Season == "winter") %>%
  filter(Location =="outflow", ResType=="percent_change") %>%
  filter(ResponseVar=="do.mean") %>%
  summarise(DO_res=is.na(RespValue)) %>%
  filter(DO_res==FALSE) %>%
  dplyr::select(model_reservoir) %>%
  unique()
# DO_list

df %>%  group_by(model_reservoir) %>%
  mutate(DO_res= model_reservoir %in% c(DO_list)$model_reservoir) %>% 
  mutate(model_reservoir=as.factor(paste0(as.double(model_reservoir),"-",model_reservoir))) %>%
  filter(Location =="full", ResType=="percent_change") %>%
  filter(ResponseVar=="warmwater.fish" & DO_res==TRUE) %>%
  mutate(RespValuePerc= RespValue,
         percentFPV = percentFPV *100)%>%
  mutate (label = ifelse(percentFPV == max(percentFPV), (model_reservoir), NA_character_)) %>% 
  ggplot(aes(x=percentFPV, y=RespValuePerc, color=model_reservoir))+
  facet_wrap("Season", labeller = season_labeller)+ # scales = "free_y",
  geom_line(linewidth=0.9, alpha=0,show.legend = TRUE)+
  labs(x="Percent Coverage of Floating PV", y="Percent Change in Warmwater Fish Habitat", color ="Reservoir Model")+
  geom_label_repel(aes(label = label), force = 1, force_pull = 1, show.legend = FALSE,xlim = c(5, NA),
                   max.overlaps = 50,nudge_x = 13,alpha=0,
                   na.rm = TRUE)+
  scale_x_continuous(breaks = c(0,25,50,75,100),expand = expansion(add=c(3,15)))+
  theme_bw()+My_Theme#+ # scale_y_continuous(trans= 'pseudo_log')
  # geom_smooth(data = . %>% group_by(percentFPV, Season) %>% summarise(RespValuePerc=mean(RespValuePerc)), color="black", linewidth=1.5,se = TRUE)

ggsave(paste0(graph_out_file,"/fig7_n.png"), dpi = 200, width = 16, height = 8)


df %>%  group_by(model_reservoir) %>%
  mutate(DO_res= model_reservoir %in% c(DO_list)$model_reservoir) %>% 
  mutate(model_reservoir=as.factor(paste0(as.double(model_reservoir),"-",model_reservoir))) %>%
  filter(Location =="full", ResType=="percent_change") %>%
  filter(ResponseVar=="coldwater.fish" & DO_res==TRUE) %>%
  mutate(RespValuePerc= RespValue,
         percentFPV = percentFPV *100)%>%
  mutate (label = ifelse(percentFPV == max(percentFPV), (model_reservoir), NA_character_)) %>% 
  ggplot(aes(x=percentFPV, y=RespValuePerc, color=model_reservoir))+
  facet_wrap("Season", labeller = season_labeller)+ # scales = "free_y",
  geom_line(linewidth=0.9, alpha=0,show.legend = TRUE)+
  labs(x="Percent Coverage of Floating PV", y="Percent Change in Coldwater Fish Habitat", color ="Reservoir Model")+
  geom_label_repel(aes(label = label), force = 1, force_pull = 1, show.legend = FALSE,xlim = c(5, NA),
                   max.overlaps = 50,nudge_x = 13,alpha=0,
                   na.rm = TRUE)+
  scale_x_continuous(breaks = c(0,25,50,75,100),expand = expansion(add=c(3,15)))+
  theme_bw()+My_Theme#+ # scale_y_continuous(trans= 'pseudo_log')
  # geom_smooth(data = . %>% group_by(percentFPV, Season) %>% summarise(RespValuePerc=mean(RespValuePerc)), color="black", linewidth=1.5,se = TRUE)

ggsave(paste0(graph_out_file,"/fig8_n.png"), dpi = 200, width = 16, height = 8)




#### plots v2 ####

df_out_v

# format_out<-"pdf"
# format_out<-"png"
format_out<-"svg"

My_Theme = theme( axis.text = element_text( size = 16 , family = "Arial"),
                  axis.text.x = element_text( size = 16 , family = "Arial"),
                  axis.title = element_text(size=20, family = "Arial"),
                  legend.title  = element_text( size = 16, face = "bold" , family = "Arial"),
                  legend.text = element_text(size=14, family = "Arial"),
                  strip.text = element_text(size = 20, family = "Arial"))

# https://mk.bcgsc.ca/colorblind/palettes.mhtml#12-color-palette-for-colorbliness
# https://mk.bcgsc.ca/colorblind/palettes/12.color.blindness.palette.txt
palette_new <- grDevices::colorRampPalette(colors = c("#9F0162","#009F81","#FF5AAF","#00FCCF","#8400CD","#008DF9",
             "#00C2F9","#FFB2FD","#A40122","#E20134","#FF6E3A","#FFC33B"))(12)


# figure 1 panels
fig_loop<-data.frame(val=c(1:4),
           resp=c("mean.surface.temp","thermocline.depth","schmidt.stability","mean.outflow.temp"),
           label=c("Mean surface temp (°C)","Thermocline depth (m)",
                   "Schmidt stability index", "Mean outflow temp (°C)")
           )
for(i in c("winter", "summer")){
  for(j in c(1:4)){
  df %>%   group_by(model_reservoir) %>%
    mutate(model_reservoir=as.factor(paste0(as.double(model_reservoir),"-",model_reservoir))) %>%
    filter(Season == i) %>%
    filter(Location !="center", ResType=="value") %>%
    filter(ResponseVar==fig_loop$resp[j]) %>%
    mutate(RespValuePerc= RespValue ,
           percentFPV = percentFPV *100)%>%
    mutate (label = ifelse(percentFPV == max(percentFPV), (model_reservoir), NA_character_)) %>%
    ggplot(aes(x=percentFPV, y=RespValuePerc, color=model_reservoir))+
    geom_line(linewidth=0.9, alpha=1,show.legend = TRUE)+
    scale_color_manual(values = palette_new)+
    labs(x="Percent Coverage of Floating PV", y=fig_loop$label[j], color ="Reservoir Model")+
    geom_label_repel(aes(label = label), force = 1, force_pull = 1, show.legend = FALSE,xlim = c(5, NA),
                     # max.overlaps = 40,nudge_x = 5, size=1, label.padding =0.1,min.segment.length=0.1,
                     max.overlaps = 50,nudge_x = 13,
                     na.rm = TRUE)+
    scale_x_continuous(breaks = c(0,50,100),expand = expansion(add=c(3,15)))+
    theme_classic()+My_Theme+theme(legend.position="none")#+ # scale_y_continuous(trans= 'pseudo_log')
    # geom_smooth(data = . %>% group_by(percentFPV, Season) %>% summarise(RespValuePerc=mean(RespValuePerc)), color="black", linewidth=1.5,se = TRUE)
  ggsave(paste0(graph_out_file,"/Loop_Fig/value_",j,"_",i,".",format_out), dpi = 300, width = 6, height = 4) #width = 45, height = 35, units = "mm"
  }
}

fig_loop<-data.frame(val=c(1:4),
                     resp=c("mean.surface.temp","thermocline.depth","schmidt.stability","mean.outflow.temp"),
                     label=c("%Δ surface temp","%Δ thermocline depth",
                             "%Δ Schmidt stability index", "%Δ outflow temp")
)
for(i in c("winter", "summer")){
  for(j in c(1:4)){
  df %>%   group_by(model_reservoir) %>%
    mutate(model_reservoir=as.factor(paste0(as.double(model_reservoir),"-",model_reservoir))) %>%
    filter(Season == i) %>%
    filter(Location !="center", ResType=="percent_change") %>%
    filter(ResponseVar==fig_loop$resp[j]) %>%
    mutate(RespValuePerc= RespValue*100 ,
           percentFPV = percentFPV *100)%>%
    mutate (label = ifelse(percentFPV == max(percentFPV), (model_reservoir), NA_character_)) %>%
    ggplot(aes(x=percentFPV, y=RespValuePerc, color=model_reservoir))+
    geom_line(linewidth=0.9, alpha=1,show.legend = TRUE)+
    scale_color_manual(values = palette_new)+
    labs(x="Percent Coverage of Floating PV", y=fig_loop$label[j], color ="Reservoir Model")+
    geom_label_repel(aes(label = label), force = 1, force_pull = 1, show.legend = FALSE,xlim = c(5, NA),
                     max.overlaps = 50,nudge_x = 13,
                     na.rm = TRUE)+
    scale_x_continuous(breaks = c(0,50,100),expand = expansion(add=c(3,15)))+
    theme_classic()+My_Theme+theme(legend.position="none")+ 
    geom_smooth(data = . %>% group_by(percentFPV) %>% summarise(RespValuePerc=mean(RespValuePerc)), color="black", linewidth=1.5,se = TRUE)
  ggsave(paste0(graph_out_file,"/Loop_Fig/pchange_",j,"_",i,".",format_out), dpi = 300, width = 6, height = 4)
  }
}

df %>%   group_by(model_reservoir) %>%
  mutate(model_reservoir=as.factor(paste0(as.double(model_reservoir),"-",model_reservoir))) %>%
  filter(Season == i) %>%
  filter(Location !="center", ResType=="percent_change") %>%
  filter(ResponseVar==fig_loop$resp[j]) %>%
  mutate(RespValuePerc= RespValue*100 ,
         percentFPV = percentFPV *100)%>%
  mutate (label = ifelse(percentFPV == max(percentFPV), (model_reservoir), NA_character_)) %>%
  ggplot(aes(x=percentFPV, y=RespValuePerc, color=model_reservoir))+
  geom_line(linewidth=0.9, alpha=1,show.legend = TRUE)+
  scale_color_manual(values = palette_new)+
  labs(x="  ", y=fig_loop$label[j], color ="Reservoir Model")+
  geom_label_repel(aes(label = label), force = 1, force_pull = 1, show.legend = FALSE,xlim = c(5, NA),
                   max.overlaps = 50,nudge_x = 13,
                   na.rm = TRUE)+
  scale_x_continuous(breaks = c(0,50,100),expand = expansion(add=c(3,15)))+
  theme_classic()+My_Theme+ 
  geom_smooth(data = . %>% group_by(percentFPV) %>% summarise(RespValuePerc=mean(RespValuePerc)), color="black", linewidth=1.5,se = TRUE)
ggsave(paste0(graph_out_file,"/Loop_Fig/fig1_legend.",format_out), dpi = 300, width = 6, height = 4)

# figure 2 panels


DO_list <- df %>%   group_by(model_reservoir) %>%
  # mutate(model_reservoir=as.factor(paste0(as.double(model_reservoir),"-",model_reservoir))) %>%
  # filter(Season == "winter") %>%
  filter(Location =="outflow", ResType=="percent_change") %>%
  filter(ResponseVar=="do.mean") %>%
  summarise(DO_res=is.na(RespValue)) %>%
  filter(DO_res==FALSE) %>%
  dplyr::select(model_reservoir) %>%
  unique()
# DO_list
fig_loop<-data.frame(val=c(1:3),
                     resp=c("do.mean","warmwater.fish","coldwater.fish"),
                     label=c("Mean dissolved O² (g/m³)","% warmwater fish habitat",
                             "% coldwater fish habitat")
)

fig_color_num<-df %>%   group_by(model_reservoir) %>%
  mutate(DO_res= model_reservoir %in% c(DO_list)$model_reservoir) %>% 
  filter(DO_res==TRUE) %>%
  mutate(model_reservoir_num=as.double(model_reservoir))
color_n<-sort(unique(fig_color_num$model_reservoir_num))


for(i in c("winter", "summer")){
  for(j in c(1:3)){
    df %>%   group_by(model_reservoir) %>%
      mutate(DO_res= model_reservoir %in% c(DO_list)$model_reservoir) %>% 
      mutate(model_reservoir=as.factor(paste0(as.double(model_reservoir),"-",model_reservoir))) %>%
      filter(Season == i) %>%
      filter(Location !="center", ResType=="value") %>%
      filter(ResponseVar==fig_loop$resp[j] & DO_res==TRUE) %>%
      mutate(RespValuePerc= RespValue ,
             percentFPV = percentFPV *100)%>%
      mutate (label = ifelse(percentFPV == max(percentFPV), (model_reservoir), NA_character_)) %>%
      ggplot(aes(x=percentFPV, y=RespValuePerc, color=model_reservoir))+
      geom_line(linewidth=0.9, alpha=1,show.legend = TRUE)+
      scale_color_manual(values = palette_new[color_n])+
      labs(x="Percent Coverage of Floating PV", y=fig_loop$label[j], color ="Reservoir Model")+
      geom_label_repel(aes(label = label), force = 1, force_pull = 1, show.legend = FALSE,xlim = c(5, NA),
                       max.overlaps = 50,nudge_x = 13,
                       na.rm = TRUE)+
      scale_x_continuous(breaks = c(0,50,100),expand = expansion(add=c(3,15)))+
      theme_classic()+My_Theme+theme(legend.position="none")#+ # scale_y_continuous(trans= 'pseudo_log')
    # geom_smooth(data = . %>% group_by(percentFPV, Season) %>% summarise(RespValuePerc=mean(RespValuePerc)), color="black", linewidth=1.5,se = TRUE)
    ggsave(paste0(graph_out_file,"/Loop_Fig/DOvalue_",j,"_",i,".",format_out), dpi = 300, width = 6, height = 4)
  }
}

fig_loop<-data.frame(val=c(1:3),
                     resp=c("do.mean","warmwater.fish","coldwater.fish"),
                     label=c("%Δ mean dissolved O²","%Δ warmwater fish habitat",
                             "%Δ coldwater fish habitat"),
                     scale=c(100,1,1)
)
for(i in c("winter", "summer")){
  for(j in c(1:3)){
    df %>%   group_by(model_reservoir) %>%
      mutate(DO_res= model_reservoir %in% c(DO_list)$model_reservoir) %>% 
      mutate(model_reservoir=as.factor(paste0(as.double(model_reservoir),"-",model_reservoir))) %>%
      filter(Season == i) %>%
      filter(Location !="center", ResType=="percent_change") %>%
      filter(ResponseVar==fig_loop$resp[j] & DO_res==TRUE) %>%
      mutate(RespValuePerc= RespValue*fig_loop$scale[j] ,
             percentFPV = percentFPV *100)%>%
      mutate (label = ifelse(percentFPV == max(percentFPV), (model_reservoir), NA_character_)) %>%
      ggplot(aes(x=percentFPV, y=RespValuePerc, color=model_reservoir))+
      geom_line(linewidth=0.9, alpha=1,show.legend = TRUE)+
      scale_color_manual(values = palette_new[color_n])+
      labs(x="Percent Coverage of Floating PV", y=fig_loop$label[j], color ="Reservoir Model")+
      geom_label_repel(aes(label = label), force = 1, force_pull = 1, show.legend = FALSE,xlim = c(5, NA),
                       max.overlaps = 50,nudge_x = 13,
                       na.rm = TRUE)+
      scale_x_continuous(breaks = c(0,50,100),expand = expansion(add=c(3,15)))+
      theme_classic()+My_Theme+theme(legend.position="none")+ 
      geom_smooth(data = . %>% group_by(percentFPV) %>% summarise(RespValuePerc=mean(RespValuePerc)), color="black", linewidth=1.5,se = TRUE)
    ggsave(paste0(graph_out_file,"/Loop_Fig/DOpchange_",j,"_",i,".",format_out), dpi = 300, width = 6, height = 4)
  }
}

df %>%   group_by(model_reservoir) %>%
  mutate(DO_res= model_reservoir %in% c(DO_list)$model_reservoir) %>% 
  mutate(model_reservoir=as.factor(paste0(as.double(model_reservoir),"-",model_reservoir))) %>%
  filter(Season == i) %>%
  filter(Location !="center", ResType=="percent_change") %>%
  filter(ResponseVar==fig_loop$resp[j] & DO_res==TRUE) %>%
  mutate(RespValuePerc= RespValue*fig_loop$scale[j] ,
         percentFPV = percentFPV *100)%>%
  mutate (label = ifelse(percentFPV == max(percentFPV), (model_reservoir), NA_character_)) %>%
  ggplot(aes(x=percentFPV, y=RespValuePerc, color=model_reservoir))+
  geom_line(linewidth=0.9, alpha=1,show.legend = TRUE)+
  scale_color_manual(values = palette_new[color_n])+
  labs(x=" ", y=fig_loop$label[j], color ="Reservoir Model")+
  geom_label_repel(aes(label = label), force = 1, force_pull = 1, show.legend = FALSE,xlim = c(5, NA),
                   max.overlaps = 50,nudge_x = 13,
                   na.rm = TRUE)+
  scale_x_continuous(breaks = c(0,50,100),expand = expansion(add=c(3,15)))+
  theme_classic()+My_Theme+
  geom_smooth(data = . %>% group_by(percentFPV) %>% summarise(RespValuePerc=mean(RespValuePerc)), color="black", linewidth=1.5,se = TRUE)
ggsave(paste0(graph_out_file,"/Loop_Fig/fig2_legend.",format_out), dpi = 300, width = 6, height = 4)


 #### plot v2 small ####

format_out<-"svg"

# https://mk.bcgsc.ca/colorblind/palettes.mhtml#12-color-palette-for-colorbliness
# https://mk.bcgsc.ca/colorblind/palettes/12.color.blindness.palette.txt
palette_new <- grDevices::colorRampPalette(colors = c("#9F0162","#009F81","#FF5AAF","#00FCCF","#8400CD","#008DF9",
                                                      "#00C2F9","#FFB2FD","#A40122","#E20134","#FF6E3A","#FFC33B"))(12)

My_Theme2 = theme( axis.text = element_text( size = 5 , family = "Arial"),
                  axis.text.x = element_text( size = 5 , family = "Arial"),
                  axis.title = element_text(size=5, family = "Arial"),
                  legend.title  = element_text( size = 5, face = "bold" , family = "Arial"),
                  legend.text = element_text(size=5, family = "Arial"),
                  strip.text = element_text(size = 5, family = "Arial"))

fig_loop<-data.frame(val=c(1:4),
                     resp=c("mean.surface.temp","thermocline.depth","schmidt.stability","mean.outflow.temp"),
                     label=c("Mean surface temp (°C)","Thermocline depth (m)",
                             "Schmidt stability index", "Mean outflow temp (°C)")
)

for(i in c("winter", "summer")){
  for(j in c(1:4)){
    df %>%   group_by(model_reservoir) %>%
      mutate(model_reservoir=as.factor(paste0(as.double(model_reservoir),"-",model_reservoir))) %>%
      filter(Season == i) %>%
      filter(Location !="center", ResType=="value") %>%
      filter(ResponseVar==fig_loop$resp[j]) %>%
      mutate(RespValuePerc= RespValue ,
             percentFPV = percentFPV *100)%>%
      mutate (label = ifelse(percentFPV == max(percentFPV), (model_reservoir), NA_character_)) %>%
      ggplot(aes(x=percentFPV, y=RespValuePerc, color=model_reservoir))+
      geom_line(linewidth=0.4, alpha=1,show.legend = TRUE)+
      scale_color_manual(values = palette_new)+
      labs(x="Percent Coverage of Floating PV", y=fig_loop$label[j], color ="Reservoir Model")+
      geom_label_repel(aes(label = label), force = 1, force_pull = 1, show.legend = FALSE,xlim = c(80, NA),
                       max.overlaps = 40,nudge_x = 5, size=1, label.padding =0.1,min.segment.length=0.1,
                       na.rm = TRUE)+
      scale_x_continuous(breaks = c(0,50,100),expand = expansion(add=c(3,15)))+
      theme_classic()+My_Theme2+theme(legend.position="none")#+ # scale_y_continuous(trans= 'pseudo_log')
    # geom_smooth(data = . %>% group_by(percentFPV, Season) %>% summarise(RespValuePerc=mean(RespValuePerc)), color="black", linewidth=1.5,se = TRUE)
    ggsave(paste0(graph_out_file,"/Loop_Fig/value_",j,"_",i,".",format_out), dpi = 300, width = 45, height = 35, units = "mm")
}}

fig_loop<-data.frame(val=c(1:4),
                     resp=c("mean.surface.temp","thermocline.depth","schmidt.stability","mean.outflow.temp"),
                     label=c("%Δ surface temp","%Δ thermocline depth",
                             "%Δ Schmidt stability index", "%Δ outflow temp")
)
# i<-"winter"
# j<-2
for(i in c("winter", "summer")){
  for(j in c(1:4)){
    df %>%   group_by(model_reservoir) %>%
      mutate(model_reservoir=as.factor(paste0(as.double(model_reservoir),"-",model_reservoir))) %>%
      filter(Season == i) %>%
      filter(Location !="center", ResType=="percent_change") %>%
      filter(ResponseVar==fig_loop$resp[j]) %>%
      mutate(RespValuePerc= RespValue*100 ,
             percentFPV = percentFPV *100)%>%
      mutate (label = ifelse(percentFPV == max(percentFPV), (model_reservoir), NA_character_)) %>%
      group_modify(~ add_row(.x,RespValue=0,RespValuePerc=0,percentFPV=0,Iteration=0,.before=0)) %>%
      tidyr::fill(Season, Location, ResponseVar, .direction = "up") %>%
       # %>% View()
      ggplot(aes(x=percentFPV, y=RespValuePerc, color=model_reservoir))+
      geom_line(linewidth=0.4, alpha=1,show.legend = TRUE)+
      # geom_line(data=. %>% dplyr::filter(model_reservoir=="1-Foster Reservoir, Oregon" &is.na(RespValuePerc)==FALSE), 
      #           linetype = "dotted",linewidth=0.3, alpha=1,show.legend = FALSE)+
      scale_color_manual(values = palette_new)+
      labs(x="Percent Coverage of Floating PV", y=fig_loop$label[j], color ="Reservoir Model")+
      geom_label_repel(aes(label = label), force = 1, force_pull = 1, show.legend = FALSE,xlim = c(80, NA),
                       max.overlaps = 40,nudge_x = 5, size=1, label.padding =0.1,min.segment.length=0.1,
                       na.rm = TRUE)+
      scale_x_continuous(breaks = c(0,50,100),expand = expansion(add=c(3,15)))+
      theme_classic()+My_Theme2+theme(legend.position="none")+
      geom_smooth(data = . %>% group_by(percentFPV) %>% summarise(RespValuePerc=mean(RespValuePerc)), color="black", linewidth=0.7,se = TRUE)
    ggsave(paste0(graph_out_file,"/Loop_Fig/pchange_",j,"_",i,".",format_out), dpi = 300, width = 45, height = 35, units = "mm")
  }
}

df %>%   group_by(model_reservoir) %>%
  mutate(model_reservoir=as.factor(paste0(as.double(model_reservoir),"-",model_reservoir))) %>%
  filter(Season == i) %>%
  filter(Location !="center", ResType=="percent_change") %>%
  filter(ResponseVar==fig_loop$resp[j]) %>%
  mutate(RespValuePerc= RespValue*100 ,
         percentFPV = percentFPV *100)%>%
  mutate (label = ifelse(percentFPV == max(percentFPV), (model_reservoir), NA_character_)) %>%
  group_modify(~ add_row(.x,RespValue=0,RespValuePerc=0,percentFPV=0,Iteration=0,.before=0)) %>%
  tidyr::fill(Season, Location, ResponseVar, .direction = "up") %>%
  ggplot(aes(x=percentFPV, y=RespValuePerc, color=model_reservoir))+
  geom_line(linewidth=0.4, alpha=1,show.legend = TRUE)+
  scale_color_manual(values = palette_new)+
  labs(x="  ", y=fig_loop$label[j], color ="Reservoir Model")+
  geom_label_repel(aes(label = label), force = 1, force_pull = 1, show.legend = FALSE,xlim = c(80, NA),
                   max.overlaps = 40,nudge_x = 5, size=1, label.padding =0.1,min.segment.length=0.1,
                   na.rm = TRUE)+
  scale_x_continuous(breaks = c(0,50,100),expand = expansion(add=c(3,15)))+
  theme_classic()+My_Theme2+ 
  geom_smooth(data = . %>% group_by(percentFPV) %>% summarise(RespValuePerc=mean(RespValuePerc)), color="black", linewidth=0.7,se = TRUE)
ggsave(paste0(graph_out_file,"/Loop_Fig/fig1_legend.",format_out), dpi = 300, width = 100, height = 80, units = "mm")

# figure 2 panels


DO_list <- df %>%   group_by(model_reservoir) %>%
  # mutate(model_reservoir=as.factor(paste0(as.double(model_reservoir),"-",model_reservoir))) %>%
  # filter(Season == "winter") %>%
  filter(Location =="outflow", ResType=="percent_change") %>%
  filter(ResponseVar=="do.mean") %>%
  summarise(DO_res=is.na(RespValue)) %>%
  filter(DO_res==FALSE) %>%
  dplyr::select(model_reservoir) %>%
  unique()
# DO_list
fig_loop<-data.frame(val=c(1:3),
                     resp=c("do.mean","warmwater.fish","coldwater.fish"),
                     label=c("Mean dissolved O² (g/m³)","% volume warmwater fish habitat",
                             "% volume coldwater fish habitat"),
                     maxy=c(NA,100,100),
                     miny=c(NA,0,0)
)

fig_color_num<-df %>%   group_by(model_reservoir) %>%
  mutate(DO_res= model_reservoir %in% c(DO_list)$model_reservoir) %>% 
  filter(DO_res==TRUE) %>%
  mutate(model_reservoir_num=as.double(model_reservoir))
color_n<-sort(unique(fig_color_num$model_reservoir_num))


for(i in c("winter", "summer")){
  for(j in c(1:3)){
    df %>%   group_by(model_reservoir) %>%
      mutate(DO_res= model_reservoir %in% c(DO_list)$model_reservoir) %>% 
      mutate(model_reservoir=as.factor(paste0(as.double(model_reservoir),"-",model_reservoir))) %>%
      filter(Season == i) %>%
      filter(Location !="center", ResType=="value") %>%
      filter(ResponseVar==fig_loop$resp[j] & DO_res==TRUE) %>%
      mutate(RespValuePerc= RespValue ,
             percentFPV = percentFPV *100)%>%
      mutate (label = ifelse(percentFPV == max(percentFPV), (model_reservoir), NA_character_)) %>%
      ggplot(aes(x=percentFPV, y=RespValuePerc, color=model_reservoir))+
      geom_line(linewidth=0.4, alpha=1,show.legend = TRUE)+
      scale_color_manual(values = palette_new[color_n])+
      labs(x="Percent Coverage of Floating PV", y=fig_loop$label[j], color ="Reservoir Model")+
      geom_label_repel(aes(label = label), force = 1, force_pull = 1, show.legend = FALSE,xlim = c(80, NA),
                       max.overlaps = 40,nudge_x = 5, size=1, label.padding =0.1,min.segment.length=0.1,
                       na.rm = TRUE)+
      scale_y_continuous(limits = c(fig_loop$miny[j],fig_loop$maxy[j]))+
      scale_x_continuous(breaks = c(0,50,100),expand = expansion(add=c(3,15)))+
      theme_classic()+My_Theme2+theme(legend.position="none")#+ # scale_y_continuous(trans= 'pseudo_log')
    # geom_smooth(data = . %>% group_by(percentFPV, Season) %>% summarise(RespValuePerc=mean(RespValuePerc)), color="black", linewidth=1.5,se = TRUE)
    ggsave(paste0(graph_out_file,"/Loop_Fig/DOvalue_",j,"_",i,".",format_out), dpi = 300,width = 45, height = 35, units = "mm")
  }
}

fig_loop<-data.frame(val=c(1:3),
                     resp=c("do.mean","warmwater.fish","coldwater.fish"),
                     label=c("%Δ mean dissolved O²","Δ percentage points of\nvolume warmwater fish habitat",
                             "Δ percentage points of\nvolume coldwater fish habitat"),
                     scale=c(100,1,1),
                     maxy=c(NA,NA,NA),
                     miny=c(NA,NA,NA)
)
for(i in c("winter", "summer")){
  for(j in c(1:3)){
    df %>%   group_by(model_reservoir) %>%
      mutate(DO_res= model_reservoir %in% c(DO_list)$model_reservoir) %>% 
      mutate(model_reservoir=as.factor(paste0(as.double(model_reservoir),"-",model_reservoir))) %>%
      filter(Season == i) %>%
      filter(Location !="center", ResType=="percent_change") %>%
      filter(ResponseVar==fig_loop$resp[j] & DO_res==TRUE) %>%
      mutate(RespValuePerc= RespValue*fig_loop$scale[j] ,
             percentFPV = percentFPV *100)%>%
      mutate (label = ifelse(percentFPV == max(percentFPV), (model_reservoir), NA_character_)) %>%
      group_modify(~ add_row(.x,RespValue=0,RespValuePerc=0,percentFPV=0,Iteration=0,.before=0)) %>%
      tidyr::fill(Season, Location, ResponseVar, .direction = "up") %>%
      ggplot(aes(x=percentFPV, y=RespValuePerc, color=model_reservoir))+
      geom_line(linewidth=0.4, alpha=1,show.legend = TRUE)+
      scale_color_manual(values = palette_new[color_n])+
      labs(x="Percent Coverage of Floating PV", y=fig_loop$label[j], color ="Reservoir Model")+
      geom_label_repel(aes(label = label), force = 1, force_pull = 1, show.legend = FALSE,xlim = c(80, NA),
                       max.overlaps = 40,nudge_x = 5, size=1, label.padding =0.1,min.segment.length=0.1,
                       na.rm = TRUE)+
      scale_y_continuous(limits = c(fig_loop$miny[j],fig_loop$maxy[j]))+
      scale_x_continuous(breaks = c(0,50,100),expand = expansion(add=c(3,15)))+
      theme_classic()+My_Theme2+theme(legend.position="none")+ 
      geom_smooth(data = . %>% group_by(percentFPV) %>% summarise(RespValuePerc=mean(RespValuePerc)), color="black", linewidth=0.7,se = TRUE)
    ggsave(paste0(graph_out_file,"/Loop_Fig/DOpchange_",j,"_",i,".",format_out), dpi = 300, width = 45, height = 35, units = "mm")
  }
}

df %>%   group_by(model_reservoir) %>%
  mutate(DO_res= model_reservoir %in% c(DO_list)$model_reservoir) %>% 
  mutate(model_reservoir=as.factor(paste0(as.double(model_reservoir),"-",model_reservoir))) %>%
  filter(Season == i) %>%
  filter(Location !="center", ResType=="percent_change") %>%
  filter(ResponseVar==fig_loop$resp[j] & DO_res==TRUE) %>%
  mutate(RespValuePerc= RespValue*fig_loop$scale[j] ,
         percentFPV = percentFPV *100)%>%
  mutate (label = ifelse(percentFPV == max(percentFPV), (model_reservoir), NA_character_)) %>%
  group_modify(~ add_row(.x,RespValue=0,RespValuePerc=0,percentFPV=0,Iteration=0,.before=0)) %>%
  tidyr::fill(Season, Location, ResponseVar, .direction = "up") %>%
  ggplot(aes(x=percentFPV, y=RespValuePerc, color=model_reservoir))+
  geom_line(linewidth=0.4, alpha=1,show.legend = TRUE)+
  scale_color_manual(values = palette_new[color_n])+
  labs(x=" ", y=fig_loop$label[j], color ="Reservoir Model")+
  geom_label_repel(aes(label = label), force = 1, force_pull = 1, show.legend = FALSE,xlim = c(80, NA),
                   max.overlaps = 40,nudge_x = 5, size=1, label.padding =0.1,min.segment.length=0.1,
                   na.rm = TRUE)+
  scale_y_continuous(limits = c(fig_loop$miny[1],fig_loop$maxy[1]))+
  scale_x_continuous(breaks = c(0,50,100),expand = expansion(add=c(3,15)))+
  theme_classic()+My_Theme2+
  geom_smooth(data = . %>% group_by(percentFPV) %>% summarise(RespValuePerc=mean(RespValuePerc)), color="black", linewidth=0.7,se = TRUE)
ggsave(paste0(graph_out_file,"/Loop_Fig/fig2_legend.",format_out), dpi = 300, width = 100, height = 60, units = "mm")


