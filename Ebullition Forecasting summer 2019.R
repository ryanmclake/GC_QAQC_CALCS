
### Pull in all the packages we will likely need at some point ###
pacman::p_load(ggplot2,
               PerformanceAnalytics, 
               MuMIn, 
               zoo, 
               grid, 
               gridExtra, 
               rcompanion, 
               MASS, 
               knitr, 
               dplyr,
               ResourceSelection, 
               tidyverse,
               ncdf4,
               reshape2, 
               ggjoy, 
               ggridges,
               see, 
               gganimate, 
               gifski,
               mvtnorm)

SE <- function(x) sd(x)/sqrt(length(x))

###build an intermediate model using 2018 catwalk data and the hobos from 2018 sediments in FCR transect 10
#############################################################################################################
cat <- read_csv("Catwalk.csv", skip = 1)
hobo_18 <- read_csv("fcrt10HOBObottom.csv")

hobo_18 <- hobo_18 %>%
  rename(TIMESTAMP = Date)

cat_sum <- cat %>% filter(TIMESTAMP >= "2018-07-05 12:00:00") %>%
  select(TIMESTAMP, wtr_2, wtr_3) %>%
  filter(wtr_2 != "NAN") %>%
  filter(wtr_3 != "NAN") %>%
  filter(TIMESTAMP != "NAN") %>%
  filter(TIMESTAMP != "YYYY_MM_DD_HH_MM_SS")%>%
  mutate(mean_ws_temp = (as.numeric(wtr_2)+as.numeric(wtr_3))/2) %>%
  select(TIMESTAMP, mean_ws_temp)

cat_sum$TIMESTAMP <- as.POSIXct(strptime(cat_sum$TIMESTAMP, '%Y-%m-%d %H:%M:%S', tz = 'EST'))


temp_model_18 <- left_join(hobo_18, cat_sum, by = "TIMESTAMP")
temp_model_18 <- na.omit(temp_model_18)


temp_model_18_lm <- lm(temp_C~mean_ws_temp, data = temp_model_18)
summary(temp_model_18_lm)

ggplot(temp_model_18, aes(mean_ws_temp,temp_C, color = TIMESTAMP))+
  geom_point(size = 3)+
  geom_smooth(method = "lm")

#############################################################################################################

#Forecast for 03 June 19
#############################################################################################################
### These are the focal depths from GLM-AED FLARE RUNS ###
the_depths_init <- c(0.1, 0.33, 0.66, 1.00, 1.33,1.66,2.00,2.33,2.66,3.0,3.33,3.66,4.0,4.33,4.66,5.0,5.33,5.66,6.0,6.33,6.66,7.00,7.33,7.66,8.0,8.33,8.66,9.00,9.33)

nc <- nc_open("C:/Users/Owner/Desktop/Ebullition_FCR_forecasts/FCRv1.beta2_H_2019526_2019527_F_16_2019527_5_25.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
nc_close(nc)

time_day <- as.data.frame(full_time_day)
temp_2.33 <- cbind(time_day, temp[1:18,1:1050,8])
temp_2.66 <- cbind(time_day, temp[1:18,1:1050,9]) ### these are the 17 forecast days and the 21 different ensembles for 2.66m glm-FLARE data
temp_3.00 <- cbind(time_day, temp[1:18,1:1050,10]) ### these are the 17 forecast days and the 21 different ensembles for 2.66m glm-FLARE data

all_ensembles <- rbind(temp_2.33,temp_2.66,temp_3.00, deparse.level = 1)
all_ensembles <- all_ensembles %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)

all_ensembles <- as.data.frame(all_ensembles)
colnames(all_ensembles)[-1] = paste0('ens_',colnames(all_ensembles)[-1])

x <- data.frame("full_time_day" = as.POSIXct("2019-05-27"), "Ensemble" = "observed", "ebullition_prediction" = 0.336616092702002, "SE" = -0.096549363, "Data" = "observation")


temp_up <- 1.13502*all_ensembles-2.16569
temp_up <- cbind(time_day, temp_up[,2:1051], deparse.level = 1)


temp_up$full_time_day <- as.POSIXct(strptime(temp_up$full_time_day, '%Y-%m-%d', tz = 'EST'))

temp_up <- temp_up %>% filter(full_time_day >= "2019-05-27") %>% filter(full_time_day <= "2019-06-03") %>% select(-full_time_day) %>%
  summarise_all(funs(mean))

log_ebu_rate_forecast_63 <- 0.336616092702002 * 0.1782 + 0.4628 * temp_up - 7.9455 ### Change ebullition Rate and parameters based on JAGS MODEL UPDATE!
log_ebu_rate_forecast_63$full_time_day <- "2019-06-03"

log_ebu_rate_forecast_63 <- melt(log_ebu_rate_forecast_63, id.vars = c("full_time_day"),
                              variable.name = "Ensemble", 
                              value.name = "ebullition_prediction")
log_ebu_rate_forecast_63$SE = NA
log_ebu_rate_forecast_63$Data = "forecast"

log_ebu_rate_forecast_observe_63 <- rbind(x,log_ebu_rate_forecast_63, deparse.level = 1)
#############################################################################################################


#Forecast for 10 June 19
#############################################################################################################
### These are the focal depths from GLM-AED FLARE RUNS ###
the_depths_init <- c(0.1, 0.33, 0.66, 1.00, 1.33,1.66,2.00,2.33,2.66,3.0,3.33,3.66,4.0,4.33,4.66,5.0,5.33,5.66,6.0,6.33,6.66,7.00,7.33,7.66,8.0,8.33,8.66,9.00,9.33)

nc <- nc_open("C:/Users/Owner/Desktop/Ebullition_FCR_forecasts/FCRv1.beta2_H_201962_201963_F_16_201963_5_24.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
nc_close(nc)

time_day <- as.data.frame(full_time_day)
temp_2.33 <- cbind(time_day, temp[1:18,1:1050,8])
temp_2.66 <- cbind(time_day, temp[1:18,1:1050,9]) ### these are the 17 forecast days and the 21 different ensembles for 2.66m glm-FLARE data
temp_3.00 <- cbind(time_day, temp[1:18,1:1050,10]) ### these are the 17 forecast days and the 21 different ensembles for 2.66m glm-FLARE data

all_ensembles <- rbind(temp_2.33,temp_2.66,temp_3.00, deparse.level = 1)
all_ensembles <- all_ensembles %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)

all_ensembles <- as.data.frame(all_ensembles)
colnames(all_ensembles)[-1] = paste0('ens_',colnames(all_ensembles)[-1])

x <- data.frame("full_time_day" = as.POSIXct("2019-06-03"), "Ensemble" = "observed", "ebullition_prediction" =1.187791077,"SE" = 0.712927152, "Data" = "observation")

temp_up <- 1.13502*all_ensembles-2.16569
temp_up <- cbind(time_day, temp_up[,2:1051], deparse.level = 1)


temp_up$full_time_day <- as.POSIXct(strptime(temp_up$full_time_day, '%Y-%m-%d', tz = 'EST'))
temp_up <- temp_up %>% filter(full_time_day >= "2019-06-03") %>% filter(full_time_day <= "2019-06-10") %>% select(-full_time_day) %>%
  summarise_all(funs(mean))

log_ebu_rate_forecast_610 <- 1.187791077 * 0.1782 + 0.4628 * temp_up - 7.9455 ### Change ebullition Rate and parameters based on JAGS MODEL UPDATE!
log_ebu_rate_forecast_610$full_time_day <- "2019-06-10"

log_ebu_rate_forecast_610 <- melt(log_ebu_rate_forecast_610, id.vars = c("full_time_day"),
                                 variable.name = "Ensemble", 
                                 value.name = "ebullition_prediction")

log_ebu_rate_forecast_610$SE = NA
log_ebu_rate_forecast_610$Data = "forecast"

log_ebu_rate_forecast_observe_610 <- rbind(x,log_ebu_rate_forecast_610, deparse.level = 1)
#############################################################################################################


#Forecast for 17 June 19
#############################################################################################################
### These are the focal depths from GLM-AED FLARE RUNS ###
the_depths_init <- c(0.1, 0.33, 0.66, 1.00, 1.33,1.66,2.00,2.33,2.66,3.0,3.33,3.66,4.0,4.33,4.66,5.0,5.33,5.66,6.0,6.33,6.66,7.00,7.33,7.66,8.0,8.33,8.66,9.00,9.33)

nc <- nc_open("C:/Users/Owner/Desktop/Ebullition_FCR_forecasts/FCRv1.beta2_H_201969_2019610_F_16_2019610_8_37.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
nc_close(nc)

time_day <- as.data.frame(full_time_day)
temp_2.33 <- cbind(time_day, temp[1:18,1:1050,8])
temp_2.66 <- cbind(time_day, temp[1:18,1:1050,9]) ### these are the 17 forecast days and the 21 different ensembles for 2.66m glm-FLARE data
temp_3.00 <- cbind(time_day, temp[1:18,1:1050,10]) ### these are the 17 forecast days and the 21 different ensembles for 2.66m glm-FLARE data

all_ensembles <- rbind(temp_2.33,temp_2.66,temp_3.00, deparse.level = 1)
all_ensembles <- all_ensembles %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)

all_ensembles <- as.data.frame(all_ensembles)
colnames(all_ensembles)[-1] = paste0('ens_',colnames(all_ensembles)[-1])

### CHANGE THE DATE AND THE EBULLITION PREDICTION VALUE!
x <- data.frame("full_time_day" = as.POSIXct("2019-06-10"), "Ensemble" = "observed", "ebullition_prediction" = 1.656739228, "SE" = 1.139866013, "Data" = "observation")

temp_up <- 1.13502*all_ensembles-2.16569
temp_up <- cbind(time_day, temp_up[,2:1051], deparse.level = 1)

temp_up$full_time_day <- as.POSIXct(strptime(temp_up$full_time_day, '%Y-%m-%d', tz = 'EST'))
temp_up <- temp_up %>% filter(full_time_day >= "2019-06-10") %>% filter(full_time_day <= "2019-06-17") %>% select(-full_time_day) %>%
  summarise_all(funs(mean))

log_ebu_rate_forecast_617 <- 1.656739228 * 0.2329 + 0.3783 * temp_up - 6.2011 ### Change ebullition Rate and parameters based on JAGS MODEL UPDATE!
log_ebu_rate_forecast_617$full_time_day <- "2019-06-17"

log_ebu_rate_forecast_617 <- melt(log_ebu_rate_forecast_617, id.vars = c("full_time_day"),
                                  variable.name = "Ensemble", 
                                  value.name = "ebullition_prediction")

log_ebu_rate_forecast_617$SE = NA
log_ebu_rate_forecast_617$Data = "forecast"

log_ebu_rate_forecast_observe_617 <- rbind(x,log_ebu_rate_forecast_617, deparse.level = 1)
#############################################################################################################


#Forecast for 24 June 19
#############################################################################################################
### These are the focal depths from GLM-AED FLARE RUNS ###
the_depths_init <- c(0.1, 0.33, 0.66, 1.00, 1.33,1.66,2.00,2.33,2.66,3.0,3.33,3.66,4.0,4.33,4.66,5.0,5.33,5.66,6.0,6.33,6.66,7.00,7.33,7.66,8.0,8.33,8.66,9.00,9.33)

nc <- nc_open("C:/Users/Owner/Desktop/Ebullition_FCR_forecasts/FCRv1.beta2_H_2019617_2019618_F_16_2019618_5_52.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
nc_close(nc)

time_day <- as.data.frame(full_time_day)
temp_2.33 <- cbind(time_day, temp[1:18,1:1050,8])
temp_2.66 <- cbind(time_day, temp[1:18,1:1050,9]) ### these are the 17 forecast days and the 21 different ensembles for 2.66m glm-FLARE data
temp_3.00 <- cbind(time_day, temp[1:18,1:1050,10]) ### these are the 17 forecast days and the 21 different ensembles for 2.66m glm-FLARE data

all_ensembles <- rbind(temp_2.33,temp_2.66,temp_3.00, deparse.level = 1)
all_ensembles <- all_ensembles %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)

all_ensembles <- as.data.frame(all_ensembles)
colnames(all_ensembles)[-1] = paste0('ens_',colnames(all_ensembles)[-1])

### CHANGE THE DATE AND THE EBULLITION PREDICTION VALUE!
x <- data.frame("full_time_day" = as.POSIXct("2019-06-17"), "Ensemble" = "observed", "ebullition_prediction" = 1.506745532,"SE" = 0.834511137, "Data" = "observation")

temp_up <- 1.13502*all_ensembles-2.16569
temp_up <- cbind(time_day, temp_up[,2:1051], deparse.level = 1)

temp_up$full_time_day <- as.POSIXct(strptime(temp_up$full_time_day, '%Y-%m-%d', tz = 'EST'))
temp_up <- temp_up %>% filter(full_time_day >= "2019-06-17") %>% filter(full_time_day <= "2019-06-24") %>% select(-full_time_day) %>%
  summarise_all(funs(mean))

log_ebu_rate_forecast_624 <- 1.506745532 * 0.2307 + 0.3820 * temp_up - 6.2775 ### Change ebullition Rate and parameters based on JAGS MODEL UPDATE!
log_ebu_rate_forecast_624$full_time_day <- "2019-06-24"

log_ebu_rate_forecast_624 <- melt(log_ebu_rate_forecast_624, id.vars = c("full_time_day"),
                                  variable.name = "Ensemble", 
                                  value.name = "ebullition_prediction")

log_ebu_rate_forecast_624$SE = NA
log_ebu_rate_forecast_624$Data = "forecast"

log_ebu_rate_forecast_observe_624 <- rbind(x,log_ebu_rate_forecast_624, deparse.level = 1)
#############################################################################################################


#Forecast for 1 July 19
#############################################################################################################
### These are the focal depths from GLM-AED FLARE RUNS ###
the_depths_init <- c(0.1, 0.33, 0.66, 1.00, 1.33,1.66,2.00,2.33,2.66,3.0,3.33,3.66,4.0,4.33,4.66,5.0,5.33,5.66,6.0,6.33,6.66,7.00,7.33,7.66,8.0,8.33,8.66,9.00,9.33)

nc <- nc_open("C:/Users/Owner/Desktop/Ebullition_FCR_forecasts/FCRv1.beta2_H_2019623_2019624_F_16_2019624_5_56.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
nc_close(nc)

time_day <- as.data.frame(full_time_day)
temp_2.33 <- cbind(time_day, temp[1:18,1:1050,8])
temp_2.66 <- cbind(time_day, temp[1:18,1:1050,9]) ### these are the 17 forecast days and the 21 different ensembles for 2.66m glm-FLARE data
temp_3.00 <- cbind(time_day, temp[1:18,1:1050,10]) ### these are the 17 forecast days and the 21 different ensembles for 2.66m glm-FLARE data

all_ensembles <- rbind(temp_2.33,temp_2.66,temp_3.00, deparse.level = 1)
all_ensembles <- all_ensembles %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)

all_ensembles <- as.data.frame(all_ensembles)
colnames(all_ensembles)[-1] = paste0('ens_',colnames(all_ensembles)[-1])

### CHANGE THE DATE AND THE EBULLITION PREDICTION VALUE!
x <- data.frame("full_time_day" = as.POSIXct("2019-06-24"), "Ensemble" = "observed", "ebullition_prediction" = 1.821390708, "SE" = 0.786843220082752, "Data" = "observation")
  
temp_up <- 1.13502*all_ensembles-2.16569
temp_up <- cbind(time_day, temp_up[,2:1051], deparse.level = 1)

temp_up$full_time_day <- as.POSIXct(strptime(temp_up$full_time_day, '%Y-%m-%d', tz = 'EST'))
temp_up <- temp_up %>% filter(full_time_day >= "2019-06-24") %>% filter(full_time_day <= "2019-07-01") %>% select(-full_time_day) %>%
  summarise_all(funs(mean))

log_ebu_rate_forecast_701 <- 1.821390708 * 0.2152 + 0.4042 * temp_up - 6.7387 ### Change ebullition Rate and parameters based on JAGS MODEL UPDATE!
log_ebu_rate_forecast_701$full_time_day <- "2019-07-01"

log_ebu_rate_forecast_701 <- melt(log_ebu_rate_forecast_701, id.vars = c("full_time_day"),
                                  variable.name = "Ensemble", 
                                  value.name = "ebullition_prediction")

log_ebu_rate_forecast_701$SE = NA
log_ebu_rate_forecast_701$Data = "forecast"

log_ebu_rate_forecast_observe_701 <- rbind(x,log_ebu_rate_forecast_701, deparse.level = 1)
#############################################################################################################

#Forecast for 8 July 19
#############################################################################################################
### These are the focal depths from GLM-AED FLARE RUNS ###
the_depths_init <- c(0.1, 0.33, 0.66, 1.00, 1.33,1.66,2.00,2.33,2.66,3.0,3.33,3.66,4.0,4.33,4.66,5.0,5.33,5.66,6.0,6.33,6.66,7.00,7.33,7.66,8.0,8.33,8.66,9.00,9.33)

nc <- nc_open("C:/Users/Owner/Desktop/Ebullition_FCR_forecasts/FCRv1.beta2_H_201971_201972_F_16_201973_21_51.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
nc_close(nc)

time_day <- as.data.frame(full_time_day)
temp_2.33 <- cbind(time_day, temp[1:18,1:1050,8])
temp_2.66 <- cbind(time_day, temp[1:18,1:1050,9]) ### these are the 17 forecast days and the 21 different ensembles for 2.66m glm-FLARE data
temp_3.00 <- cbind(time_day, temp[1:18,1:1050,10]) ### these are the 17 forecast days and the 21 different ensembles for 2.66m glm-FLARE data

all_ensembles <- rbind(temp_2.33,temp_2.66,temp_3.00, deparse.level = 1)
all_ensembles <- all_ensembles %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)

all_ensembles <- as.data.frame(all_ensembles)
colnames(all_ensembles)[-1] = paste0('ens_',colnames(all_ensembles)[-1])

### CHANGE THE DATE AND THE EBULLITION PREDICTION VALUE!
x <- data.frame("full_time_day" = as.POSIXct("2019-07-01"), "Ensemble" = "observed", "ebullition_prediction" = 2.577082428, "SE" = 1.30319631524252, "Data" = "observation")

temp_up <- 1.13502*all_ensembles-2.16569
temp_up <- cbind(time_day, temp_up[,2:1051], deparse.level = 1)

temp_up$full_time_day <- as.POSIXct(strptime(temp_up$full_time_day, '%Y-%m-%d', tz = 'EST'))
temp_up <- temp_up %>% filter(full_time_day >= "2019-07-01") %>% filter(full_time_day <= "2019-07-08") %>% select(-full_time_day) %>%
  summarise_all(funs(mean))

log_ebu_rate_forecast_708 <- 2.577082428 * 0.2136 + 0.4071 * temp_up - 6.7995 ### Change ebullition Rate and parameters based on JAGS MODEL UPDATE!
log_ebu_rate_forecast_708$full_time_day <- "2019-07-08"

log_ebu_rate_forecast_708 <- melt(log_ebu_rate_forecast_708, id.vars = c("full_time_day"),
                                  variable.name = "Ensemble", 
                                  value.name = "ebullition_prediction")

log_ebu_rate_forecast_708$SE = NA
log_ebu_rate_forecast_708$Data = "forecast"

log_ebu_rate_forecast_observe_708 <- rbind(x,log_ebu_rate_forecast_708, deparse.level = 1)
#############################################################################################################

#Forecast for 15 July 19
#############################################################################################################
### These are the focal depths from GLM-AED FLARE RUNS ###
the_depths_init <- c(0.1, 0.33, 0.66, 1.00, 1.33,1.66,2.00,2.33,2.66,3.0,3.33,3.66,4.0,4.33,4.66,5.0,5.33,5.66,6.0,6.33,6.66,7.00,7.33,7.66,8.0,8.33,8.66,9.00,9.33)

nc <- nc_open("C:/Users/Owner/Desktop/Ebullition_FCR_forecasts/FCRv1.beta2_H_201978_201979_F_16_201979_5_24.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
nc_close(nc)

time_day <- as.data.frame(full_time_day)
temp_2.33 <- cbind(time_day, temp[1:18,1:1050,8])
temp_2.66 <- cbind(time_day, temp[1:18,1:1050,9]) ### these are the 17 forecast days and the 21 different ensembles for 2.66m glm-FLARE data
temp_3.00 <- cbind(time_day, temp[1:18,1:1050,10]) ### these are the 17 forecast days and the 21 different ensembles for 2.66m glm-FLARE data

all_ensembles <- rbind(temp_2.33,temp_2.66,temp_3.00, deparse.level = 1)
all_ensembles <- all_ensembles %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)

all_ensembles <- as.data.frame(all_ensembles)
colnames(all_ensembles)[-1] = paste0('ens_',colnames(all_ensembles)[-1])

### CHANGE THE DATE AND THE EBULLITION PREDICTION VALUE!
x <- data.frame("full_time_day" = as.POSIXct("2019-07-08"), "Ensemble" = "observed", "ebullition_prediction" = 2.914710062, "SE" = 1.4977898301594, "Data" = "observation")

temp_up <- 1.13502*all_ensembles-2.16569
temp_up <- cbind(time_day, temp_up[,2:1051], deparse.level = 1)

temp_up$full_time_day <- as.POSIXct(strptime(temp_up$full_time_day, '%Y-%m-%d', tz = 'EST'))
temp_up <- temp_up %>% filter(full_time_day >= "2019-07-01") %>% filter(full_time_day <= "2019-07-08") %>% select(-full_time_day) %>%
  summarise_all(funs(mean))

log_ebu_rate_forecast_715 <- 2.914710062 * 0.2973 + 0.2768 * temp_up - 4.1170 ### Change ebullition Rate and parameters based on JAGS MODEL UPDATE!
log_ebu_rate_forecast_715$full_time_day <- "2019-07-15"

log_ebu_rate_forecast_715 <- melt(log_ebu_rate_forecast_715, id.vars = c("full_time_day"),
                                  variable.name = "Ensemble", 
                                  value.name = "ebullition_prediction")

log_ebu_rate_forecast_715$SE = NA
log_ebu_rate_forecast_715$Data = "forecast"

log_ebu_rate_forecast_observe_715 <- rbind(x,log_ebu_rate_forecast_715, deparse.level = 1)
#############################################################################################################


### MAKE UP THE Forecast PLOT
#############################################################################################################

ebu_forecast_all <- rbind(log_ebu_rate_forecast_observe_63,
                          log_ebu_rate_forecast_observe_610,
                          log_ebu_rate_forecast_observe_617,
                          log_ebu_rate_forecast_observe_624,
                          log_ebu_rate_forecast_observe_701,
                          log_ebu_rate_forecast_observe_708,
                          log_ebu_rate_forecast_observe_715,
                          deparse.level = 1)

ebu_forecast_all$ebullition_prediction <- exp(ebu_forecast_all$ebullition_prediction)
ebu_forecast_all$SE <- exp(ebu_forecast_all$SE)

write_csv(ebu_forecast_all, "Aggregated_Ebullition_Forecast.csv")

### make the plots of the forecasts aggregated each week
cols <- c("observation" = "#4cff00", "forecast" = "#ff00cc")

ggplot(ebu_forecast_all, aes(x=full_time_day, y=ebullition_prediction, group = Data))+
  geom_point(aes(shape=Data, color=Data, size = Data))+
  geom_errorbar(aes(ymin=ebullition_prediction-SE, ymax=ebullition_prediction+SE), width=600000,
                position=position_dodge(0.05), color = "#4cff00")+
  scale_colour_manual(values = cols, aesthetics = c("color", "fill"))+
  scale_size_manual(values=c(8,3))+
  ylim(0,40)+
  xlim(as.POSIXct("2019-05-20"), as.POSIXct("2019-10-20"))+
  xlab("")+
  labs(title="Methane ebullition rate forecasts from FCR's riverine zone  (Transect average of four traps)")+
  ylab(expression(mg~CH[4]~m^{-2}~d^{-1}))+
  theme_blackboard()+
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=27),
        legend.position=c(.1, .9),
        legend.title = element_blank())
#############################################################################################################
