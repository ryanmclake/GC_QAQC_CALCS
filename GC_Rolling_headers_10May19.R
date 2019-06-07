
### Sript for Shimadzu GC QCQA, analyses, and conversions using Henry's laws ###
### Script published by RPM ###
### Last update on 10May2019: By RPM ### 

### Load in the packages we will need for these analyses
pacman::p_load(tidyverse)

### Read in the data tables that were exported from the GC
fid <- read.csv("C:/Users/Owner/Dropbox/2019/GC_Carey_Lab/GC_QAQC_CALCS/FCR_dissolved_ebu_04June19_TCD.txt", sep = "\t", header = F) ### Will need to change WD and File name each time
tcd <- read.csv("C:/Users/Owner/Dropbox/2019/GC_Carey_Lab/GC_QAQC_CALCS/FCR_dissolved_ebu_04June19_TCD.txt", sep = "\t", header = F) ### Will need to change WD and File name each time


### ROLLING QA CHARTS
### Extract the ambient air samples from the FID and the TCD. 

ch4_air <- fid %>%
  filter(V4 == "air") %>%
  select(V3, V4, V8) %>%
  rename(date = V3) %>%
  rename(sample = V4) %>%
  rename(peak_area = V8)

add_row(ch4_air, date = ch4_air$date, sample = "air", peak_area = ch4_air$peak_area) %>% group_by(date)

  ggplot(ch4_air, aes(x=date, y=as.numeric(as.character(peak_area))))+
  geom_point(size = 3)+
  ylim(0,5000)+
  ylab("Air Peak Area")+
  geom_hline(yintercept=2000)+
  geom_hline(yintercept=500)+
  theme_bw()

  

