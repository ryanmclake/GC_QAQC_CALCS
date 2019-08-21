pacman::p_load(tidyverse,
               zoo, 
               see,
               gganimate)

ghg_19 <- read_csv("./GC_TS_2019_Rready.csv")

### BVR
#############################################################
### Organize and Plot the 2019 BVR GHG dissolved datasets ###

### Extract just BVR ###
bvr_ghg_19 <- ghg_19 %>%
  filter(Reservoir =="BVR") %>%
  arrange(Depth_m, Date)

### Determine the mean of the two reps (bad samples have been QA/QC'd in excel) ###
bvr_ghg_19_0.1 <- bvr_ghg_19 %>%filter(Depth_m == 0.1) %>%group_by(Date)%>%summarise_all(funs(mean)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

bvr_ghg_19_3 <- bvr_ghg_19 %>%filter(Depth_m == 3) %>%group_by(Date)%>%summarise_all(funs(mean)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

bvr_ghg_19_6 <- bvr_ghg_19 %>%filter(Depth_m == 6) %>%group_by(Date)%>%summarise_all(funs(mean)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

bvr_ghg_19_9 <- bvr_ghg_19 %>%filter(Depth_m == 9) %>%group_by(Date)%>%summarise_all(funs(mean)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

bvr_ghg_19_11 <- bvr_ghg_19 %>%filter(Depth_m == 11) %>%group_by(Date)%>%summarise_all(funs(mean)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)


### Calculatre the SD of the mean ###
bvr_ghg_19_0.1_sd <- bvr_ghg_19 %>%filter(Depth_m == 0.1) %>%group_by(Date)%>%summarise_all(funs(sd)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

bvr_ghg_19_3_sd <- bvr_ghg_19 %>%filter(Depth_m == 3) %>%group_by(Date)%>%summarise_all(funs(sd)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

bvr_ghg_19_6_sd <- bvr_ghg_19 %>%filter(Depth_m == 6) %>%group_by(Date)%>%summarise_all(funs(sd)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

bvr_ghg_19_9_sd <- bvr_ghg_19 %>%filter(Depth_m == 9) %>%group_by(Date)%>%summarise_all(funs(sd)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

bvr_ghg_19_11_sd <- bvr_ghg_19 %>%filter(Depth_m == 11) %>%group_by(Date)%>%summarise_all(funs(sd)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

### Bind it all together ###
bvr_ghg_19_all <- rbind(bvr_ghg_19_0.1, bvr_ghg_19_3, bvr_ghg_19_6, bvr_ghg_19_9, bvr_ghg_19_11, deparse.level = 1)
bvr_ghg_19_all_sd <- rbind(bvr_ghg_19_0.1_sd, bvr_ghg_19_3_sd, bvr_ghg_19_6_sd, bvr_ghg_19_9_sd, bvr_ghg_19_11_sd, deparse.level = 1)

### Bind it all together ###
bvr_ghg_19_all_plus_sd <- cbind(bvr_ghg_19_all, bvr_ghg_19_all_sd[,3:4], deparse.level = 1)
names(bvr_ghg_19_all_plus_sd)[5] <- "ch4_sd"
names(bvr_ghg_19_all_plus_sd)[6] <- "co2_sd"



### Make the TS plots of the gases in the Water Column ### 
cols <- c("0.1" = "#4cff00", "3" = "#ff00cc", "6" = "#b300ff", "9" = "#ffb300", "11" = "#ff3300")

pdf("BVR_GHG_CH4_2019_Depths.pdf", width=12, height=8)
ggplot(bvr_ghg_19_all_plus_sd, aes(x = Date, y = as.numeric(ch4_umolL), group = as.factor(Depth_m), color = as.factor(Depth_m), fill = as.factor(Depth_m)))+
  geom_line(lwd = 1.5)+
  geom_point(size = 4,pch = 21)+
  geom_errorbar(aes(ymin=ch4_umolL-ch4_sd, ymax=ch4_umolL+ch4_sd), width=5,
                position=position_dodge(0))+
  scale_colour_manual(values = cols, aesthetics = c("color", "fill"))+
  ylim(0,1000)+
  xlab("")+
  ylab(expression(umol/L~CH[4]))+
  theme_blackboard()+
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=27),
        legend.position=c(.1, .8),
        legend.title = element_blank())
dev.off()


#CO2
pdf("BVR_GHG_CO2_2019_Depths.pdf", width=12, height=8)
ggplot(bvr_ghg_19_all_plus_sd, aes(x = Date, y = co2_umolL, group = as.factor(Depth_m), color = as.factor(Depth_m), fill = as.factor(Depth_m)))+
  geom_line(lwd = 1.5)+
  geom_point(size = 4,pch = 21)+
  geom_errorbar(aes(ymin=co2_umolL-co2_sd, ymax=co2_umolL+co2_sd), width=5,
                position=position_dodge(0))+
  scale_colour_manual(values = cols, aesthetics = c("color", "fill"))+
  ylim(0,1000)+
  xlab("")+
  ylab(expression(umol/L~CO[2]))+
  theme_blackboard()+
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=27),
        legend.position=c(.1, .8),
        legend.title = element_blank())
dev.off()
#############################################################

### FCR 
#############################################################
### Organize and Plot the 2019 FCR GHG dissolved datasets ###

### Extract just FCR ###
fcr_ghg_19 <- ghg_19 %>%
  filter(Reservoir =="FCR") %>%
  arrange(Depth_m, Date)

fcr_ghg_19$ch4_umolL <- na.approx(fcr_ghg_19$ch4_umolL)
fcr_ghg_19$co2_umolL <- na.approx(fcr_ghg_19$co2_umolL)

### Determine the mean of the two reps (bad samples have been QA/QC'd in excel) ###
fcr_ghg_19_0.1 <- fcr_ghg_19 %>%filter(Depth_m == 0.1) %>%group_by(Date)%>%summarise_all(funs(mean)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

fcr_ghg_19_1.6 <- fcr_ghg_19 %>%filter(Depth_m == 1.6) %>%group_by(Date)%>%summarise_all(funs(mean)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

fcr_ghg_19_3.8 <- fcr_ghg_19 %>%filter(Depth_m == 3.8) %>%group_by(Date)%>%summarise_all(funs(mean)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

fcr_ghg_19_5 <- fcr_ghg_19 %>%filter(Depth_m == 5) %>%group_by(Date)%>%summarise_all(funs(mean)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

fcr_ghg_19_6.2 <- fcr_ghg_19 %>%filter(Depth_m == 6.2) %>%group_by(Date)%>%summarise_all(funs(mean)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

fcr_ghg_19_8 <- fcr_ghg_19 %>%filter(Depth_m == 8) %>%group_by(Date)%>%summarise_all(funs(mean)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

fcr_ghg_19_9 <- fcr_ghg_19 %>%filter(Depth_m == 9) %>%group_by(Date)%>%summarise_all(funs(mean)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

fcr_ghg_19_100 <- fcr_ghg_19 %>%filter(Depth_m == 100) %>%group_by(Date)%>%summarise_all(funs(mean)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

fcr_ghg_19_200 <- fcr_ghg_19 %>%filter(Depth_m == 200) %>%group_by(Date)%>%summarise_all(funs(mean)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

### Calculatre the SD of the mean ###
fcr_ghg_19_0.1_sd <- fcr_ghg_19 %>%filter(Depth_m == 0.1) %>%group_by(Date)%>%summarise_all(funs(sd)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

fcr_ghg_19_1.6_sd <- fcr_ghg_19 %>%filter(Depth_m == 1.6) %>%group_by(Date)%>%summarise_all(funs(sd)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

fcr_ghg_19_3.8_sd <- fcr_ghg_19 %>%filter(Depth_m == 3.8) %>%group_by(Date)%>%summarise_all(funs(sd)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

fcr_ghg_19_5_sd <- fcr_ghg_19 %>%filter(Depth_m == 5) %>%group_by(Date)%>%summarise_all(funs(sd)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

fcr_ghg_19_6.2_sd <- fcr_ghg_19 %>%filter(Depth_m == 6.2) %>%group_by(Date)%>%summarise_all(funs(sd)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

fcr_ghg_19_8_sd <- fcr_ghg_19 %>%filter(Depth_m == 8) %>%group_by(Date)%>%summarise_all(funs(sd)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

fcr_ghg_19_9_sd <- fcr_ghg_19 %>%filter(Depth_m == 9) %>%group_by(Date)%>%summarise_all(funs(sd)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

fcr_ghg_19_100_sd <- fcr_ghg_19 %>%filter(Depth_m == 100) %>%group_by(Date)%>%summarise_all(funs(sd)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

fcr_ghg_19_200_sd <- fcr_ghg_19 %>%filter(Depth_m == 200) %>%group_by(Date)%>%summarise_all(funs(sd)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

### Bind it all together ###
fcr_ghg_19_all <- rbind(fcr_ghg_19_0.1, fcr_ghg_19_1.6, fcr_ghg_19_3.8, fcr_ghg_19_5, fcr_ghg_19_6.2,fcr_ghg_19_8,fcr_ghg_19_9, deparse.level = 1)
fcr_ghg_19_all_sd <- rbind(fcr_ghg_19_0.1_sd, fcr_ghg_19_1.6_sd, fcr_ghg_19_3.8_sd, fcr_ghg_19_5_sd, fcr_ghg_19_6.2_sd,fcr_ghg_19_8_sd,fcr_ghg_19_9_sd, deparse.level = 1)

fcr_ghg_19_all_inf <- rbind(fcr_ghg_19_100, fcr_ghg_19_200, deparse.level = 1)
fcr_ghg_19_all_inf_sd <- rbind(fcr_ghg_19_100_sd, fcr_ghg_19_200_sd, deparse.level = 1)

### Bind it all together ###
fcr_ghg_19_all_plus_sd <- cbind(fcr_ghg_19_all, fcr_ghg_19_all_sd[,3:4], deparse.level = 1)
names(fcr_ghg_19_all_plus_sd)[5] <- "ch4_sd"
names(fcr_ghg_19_all_plus_sd)[6] <- "co2_sd"

fcr_ghg_19_all_inf_plus_sd <- cbind(fcr_ghg_19_all_inf, fcr_ghg_19_all_inf_sd[,3:4], deparse.level = 1)
names(fcr_ghg_19_all_inf_plus_sd)[5] <- "ch4_sd"
names(fcr_ghg_19_all_inf_plus_sd)[6] <- "co2_sd"



### Make the TS plots of the gases in the Water Column ### 
cols_fcr <- c("0.1" = "#ccff00", "1.6" = "#3300ff", "3.8" = "#ffb300", "5" = "#4cff00", "6.2" = "#b300ff", "8" = "#ff00cc", "9" = "#00ccff")
cols_fcr_inf <- c("100" = "#ccff00", "200" = "#3300ff")

pdf("FCR_GHG_CH4_2019_Depths.pdf", width=12, height=8)
#CH4
ggplot(fcr_ghg_19_all_plus_sd, aes(x = Date, y = ch4_umolL, group = as.factor(Depth_m), color = as.factor(Depth_m), fill = as.factor(Depth_m)))+
  geom_line(lwd = 1.5)+
  geom_point(size = 4,pch = 21)+
  geom_errorbar(aes(ymin=ch4_umolL-ch4_sd, ymax=ch4_umolL+ch4_sd), width=5,
                position=position_dodge(0))+
  scale_colour_manual(values = cols_fcr, aesthetics = c("color", "fill"))+
  ylim(0,30)+
  xlab("")+
  geom_vline(xintercept = as.POSIXct("2019-06-03"))+
  geom_vline(xintercept = 5)+
  geom_vline(xintercept = 5)+
  geom_vline(xintercept = 5)+
  ylab(expression(umol/L~CH[4]))+
  theme_blackboard()+
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=27),
        legend.position=c(.1, .8),
        legend.title = element_blank())
dev.off()

pdf("FCR_GHG_CH4_2019_inflows.pdf", width=12, height=8)
#inf CH4
ggplot(fcr_ghg_19_all_inf_plus_sd, aes(x = Date, y = ch4_umolL, group = as.factor(Depth_m), color = as.factor(Depth_m), fill = as.factor(Depth_m)))+
  geom_line(lwd = 1.5)+
  geom_point(size = 4,pch = 21)+
  geom_errorbar(aes(ymin=ch4_umolL-ch4_sd, ymax=ch4_umolL+ch4_sd), width=5,
                position=position_dodge(0))+
  scale_colour_manual(values = cols_fcr_inf, aesthetics = c("color", "fill"))+
  ylim(0,15)+
  xlab("")+
  ylab(expression(umol/L~CH[4]))+
  theme_blackboard()+
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=27),
        legend.position=c(.1, .8),
        legend.title = element_blank())
dev.off()

pdf("FCR_GHG_CO2_2019_inflows.pdf", width=12, height=8)
#inf CO2
ggplot(fcr_ghg_19_all_inf_plus_sd, aes(x = Date, y = co2_umolL, group = as.factor(Depth_m), color = as.factor(Depth_m), fill = as.factor(Depth_m)))+
  geom_line(lwd = 1.5)+
  geom_point(size = 4,pch = 21)+
  geom_errorbar(aes(ymin=co2_umolL-co2_sd, ymax=co2_umolL+co2_sd), width=5,
                position=position_dodge(0))+
  scale_colour_manual(values = cols_fcr_inf, aesthetics = c("color", "fill"))+
  ylim(0,700)+
  xlab("")+
  ylab(expression(umol/L~CO[2]))+
  theme_blackboard()+
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=27),
        legend.position=c(.1, .8),
        legend.title = element_blank())
dev.off()

  
pdf("FCR_GHG_CO2_2019_Depths.pdf", width=12, height=8)
#CO2
ggplot(fcr_ghg_19_all_plus_sd, aes(x = Date, y = co2_umolL, group = as.factor(Depth_m), color = as.factor(Depth_m), fill = as.factor(Depth_m)))+
  geom_line(lwd = 1.5)+
  geom_point(size = 4,pch = 21)+
  geom_errorbar(aes(ymin=co2_umolL-co2_sd, ymax=co2_umolL+co2_sd), width=5,
                position=position_dodge(0))+
  scale_colour_manual(values = cols_fcr, aesthetics = c("color", "fill"))+
  ylim(0,700)+
  xlab("")+
  ylab(expression(umol/L~CO[2]))+
  theme_blackboard()+
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=27),
        legend.position=c(.1, .8),
        legend.title = element_blank())
dev.off()
#############################################################

