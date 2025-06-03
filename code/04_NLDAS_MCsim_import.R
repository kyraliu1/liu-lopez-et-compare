# monte carlo sim for penman monteith using NLDAS inputs
# inputs nldas forcing
# saves as RDS

# setup -------------------------------------------------------------------


wd <- '/media/kyra/KL_REU231'
#wd<- '/home/kyra/Documents/research/CSULA_REU'
setwd(wd)
library(terra)
library(dplyr)
library(openxlsx)

# watershed import --------------------------------------------------------
desired <- 'mad'#readline(prompt = "enter desired watershed: \n")
if (grepl("mad",desired,ignore.case = T)){
  nldas <- read.table("./data/august/NLDAS_data_AF.txt") # mad river
 
  finame = './4_results/august/nldas_sim_mad_12'
  start_date <- as.POSIXct("2012-10-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")
  target_date <- as.POSIXct("2020-08-16 00:00:00", format = "%Y-%m-%d %H:%M:%S")
  z0_pre = runif(10000,0.5,0.8) # m roughness 
  alpha_values_pre <- runif(10000, 0.05, 0.20) # albedo range forested
  eps_values_pre <- runif(10000, 0.97, 0.99) # emissivity range; change for post
  eps_max = 0.99
  G_values <- rnorm(10000, 0, 10)  # ground flux range (W/m^2)
  
  
}else if (grepl("slate",desired,ignore.case = T)){
  nldas <- read.xlsx('./data/delta/NLDAS_data_slate.xlsx') # slate creek
  start_date <- as.POSIXct("2012-10-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")
  target_date <- as.POSIXct("2018-9-05 00:00:00", format = "%Y-%m-%d %H:%M:%S")
  z0_pre = rep(.1,10000)

  alpha_values_pre <- runif(10000,0.16,0.26)
  eps_values_pre <- runif(10000, 0.9 ,0.95) # emissivity range; change for post
  eps_max = 0.95
  G_values <- runif(10000, 0, 20)  # ground flux range (W/m^2)
  
  # change based on land cover type
  # forest
  # z0_pre = runif(10000,0.5,0.8) # m roughness
  # alpha_values_pre <- runif(10000, 0.05, 0.20) # albedo range forested
  # eps_values_pre <- runif(10000, 0.97, 0.99) # emissivity range; change for post
  # eps_max = 0.99
  # G_values <- rnorm(10000, 0, 10)  # ground flux range (W/m^2)
  # 
  
  finame = './4_results/delta/nldas_sim_slate'
}else if (grepl("chiquito",desired,ignore.case = T)){
  nldas <- read.table('./data/creek/NLDAS_creek_fire.txt') # mad river
  finame = './4_results/creek/nldas_sim_chiquito'
  start_date <- as.POSIXct("2012-10-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")
  target_date <- as.POSIXct("2020-09-04 00:00:00", format = "%Y-%m-%d %H:%M:%S")
  z0_pre = runif(10000,0.5,0.8) # m roughness 
  alpha_values_pre <- runif(10000, 0.05, 0.20) # albedo range forested
  eps_values_pre <- runif(10000, 0.97, 0.99) # emissivity range; change for post
  eps_max = 0.99
  G_values <- rnorm(10000, 0, 10)  # ground flux range (W/m^2)
  
  
}else{
  
  stop("watershed not correct, try again\n") 
}

#start_date <- as.POSIXct("2018-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")

hourly <- seq(from = start_date, by = "hour",length.out = nrow(nldas))
index_fire <- which(hourly == target_date)
# penman monteith PET -----------------------------------------------------


names(nldas) <- c("DSWR", 
 "DLWR", 
  "APCP", 
  "Temp", 
  "UGRD", 
 "VGRD", 
  "Press", 
  "SPFH") 
# function to estimate ET using Penman-Monteith equation



Rs <- nldas$DSWR # (w/m^2) *.0854 # w/m^2 to MJ/m^2
Rl <- nldas$DLWR # (w/m^2)*.0854 # w/m^2 to MJ/m^2
P <- nldas$APCP #mm/s
temp <- nldas$Temp # K

U <- nldas$UGRD
V <- nldas$VGRD
Pa <- nldas$Press # (Pa) /1000 #Pa to KPa
q <- nldas$SPFH #kg/kg

# C air and constant pressure
Cp <-1004 # J/kg/K

# air temperature from Kelvin to Celsius
T_celsius <- temp - 273.15

# psychrometric constant (gamma)
gamma <- Pa * Cp/(0.622*2.5e6) #Pa/K

# saturation vapor pressure (es)
es <- 610.8 * exp((2.5e6/461)*((1/273.16)-(1/temp))) # Pa

# slope of the saturation vapor pressure curve (delta)
delta <- (2.5e6 * es) /(461* temp^2) #Pa/K

# saturated specific humidity
qs <- 0.622*es/Pa #kg/kg

# relative humidity (RH)
RH <- q/qs

# actual vapor pressure (ea)
ea <- RH*es # Pa


# wind speed (u2) at 10 meters above the ground
# nldas = x and y components, 10m above ground
u2 <- sqrt(U^2 + V^2)

# air density
rho <- Pa/(287*temp*(1+0.608*q)) #kg/m^2

# aerodynamic resistance
# z0 bare soil = 0.01m
# z0 open shrublands = 0.1m
# z0 evergreen = 1.0m

# mc sim

nsim <- 10000
set.seed(123)
# alpha and epsilon values for herbaceous scrub, pre & post fire
alpha_values_post <- runif(10000, 0.05, 0.40) # albedo range
z0_post <- runif(10000,0.05,max(z0_pre))
eps_values_post <- runif(10000, 0.84, eps_max) # emissivity range; change for post



# vectors to store max, min, and average ET values
max_ET <- numeric(nrow(nldas))
min_ET <- numeric(nrow(nldas))
avg_ET <- numeric(nrow(nldas))
pb <- txtProgressBar(1,nrow(nldas),1)

# take random for each 
# change NLDAS inputs post burn
for (i in 1:nrow(nldas)) {
  ET_row <- numeric(nsim)  # store ET values for the current row
  
  for (j in 1:nsim) {
   if (i <  index_fire){
    alpha <- alpha_values_pre[j]
    eps <-eps_values_pre[j]
    z0 <- z0_pre[j] # pre fire (m) 
  }else{
    alpha <- alpha_values_post[j]
    eps <-eps_values_post[j]

    z0 <- z0_post[j] # post fire (m) bare

  }
    Rn <- (nldas$DSWR[i] * (1 - alpha) + nldas$DLWR[i] - eps * 5.67e-8 * temp[i]^4) # W/m^2
    


      G <- G_values[j]
      
      # aerodynamic resistance
      h = z0/0.1 # m
      d = 0.7*h # m
      ran <- (log((10-d)/z0))^2/(0.4^2*u2[i]) # s/m
      
      # LAI open shrub
      # lai = 3.5
      
      # ET
      
      lep <- ( delta[i]/gamma[i] * (Rn - G) +(rho[i]*2.5e6*(qs[i]-q[i]))/ran) / (1+(delta[i]/gamma[i])) #W/m^2 
      etp <-3600*lep/(2.5e6) #mm/h
      # Store the ET value for the current simulation
      ET_row[j] <- etp
      if (ET_row[j]<0){
        ET_row[j] <-0
      }
    }
    setTxtProgressBar(pb,i)
    
  
 
  
  # storing max, min, and avg ET 
  max_ET[i] <- max(ET_row)
  min_ET[i] <- min(ET_row)
  avg_ET[i] <- mean(ET_row)
}


close(pb)


nldas$datetime <- hourly

nldas$EThourly_min <- min_ET
nldas$EThourly_max <- max_ET
nldas$EThourly_mean <- avg_ET


# nldas$G <- G
# nldas$Rn <- Rn
# group by date and calculate the daily sum of ET
daily_sum_ET_mean <- nldas %>%
  group_by(date = as.Date(datetime)) %>%
  summarize(ETdaily_mean = sum(EThourly_mean, na.rm = TRUE))

daily_sum_ET_min <- nldas %>%
  group_by(date = as.Date(datetime)) %>%
  summarize(ETdailymin = sum(EThourly_min, na.rm = TRUE))

daily_sum_ET_max <-  nldas %>%
  group_by(date = as.Date(datetime)) %>%
  summarize(ETdaily_max = sum(EThourly_max , na.rm = TRUE))

nldas_daily <- merge(daily_sum_ET_mean,daily_sum_ET_min, by = 'date')
nldas_daily <- merge(nldas_daily,daily_sum_ET_max,by = 'date')
# plot(nldas_daily$EThourly_min,type = 'l')
# lines(nldas_daily$EThourly_max,col = 'darkgreen')

calendar_moving_average <- function(data, dates, window_size) {
  ma <- numeric(length(data))
  for (i in 1:length(data)) {
    current_date <- dates[i]
    start_date <- current_date - (window_size - 1)
    relevant_data <- data[dates >= start_date & dates <= current_date]
    ma[i] <- mean(relevant_data, na.rm = TRUE)
  }
  return(ma)
}
nldas_daily$ET_60day_avg <- calendar_moving_average(nldas_daily$ETdaily_mean, nldas_daily$date, 60)
plot(nldas_daily$date,nldas_daily$ET_60day_avg,type = 'l')
saveRDS(nldas_daily,finame)

#write.csv(nldas_daily,'./nldas_output.csv',row.names = F)

