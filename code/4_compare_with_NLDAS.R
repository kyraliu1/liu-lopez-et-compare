# plots data from all three sensors
# finds correlation stats between all three

# setup -------------------------------------------------------------------


wd <- '/media/kyra/KL_REU231'
# wd <- 'D:/'
#wd<- '/home/kyra/Documents/research/CSULA_REU'
setwd(wd)
library(terra)

library(dplyr)
library(corrplot)
source("./processing_scripts/functions/fclos.R")

# watershed import --------------------------------------------------------
desired <- 'chiquito'
#readline(prompt = "enter desired watershed: \n")

if (grepl("mad",desired,ignore.case = T)){
  polygon_n = 3
  fire = 'august'
  alexi <- readRDS('./4_results/august/alexi_mad_ET' )
  creek <- vect('./watershedbounds/madbounds') # mad river
  nldas <- readRDS('./4_results/august/nldas_sim_mad_12')
  
  modis <- readRDS('./4_results/august/MODIS_mad_ET2')
  target_date <- as.Date("2020-08-16") # fire start (august)
  wtr_n = polygon_n +7
}else if (grepl("slate",desired,ignore.case = T)){
  alexi <- readRDS("./4_results/delta/alexi_creek_ET")
  target_date <- as.Date("2018-09-05") # fire start (delta)
  fire = 'delta'
  nldas <- readRDS('./4_results/delta/nldas_sim_slate')
  creek  <- vect("./watershedbounds/slatebounds") # slate creek sac river
    
  modis<- readRDS("./4_results/delta/MODIS_slate_ET2")#readRDS("./4_results/delta/MODIS_creek_ET")#

  polygon_n = 7
  wtr_n = polygon_n
  
  if (polygon_n %in% c(6,7)){
    nldas <- readRDS('./4_results/delta/nldas_sim_slate_forest')
  }
  if (polygon_n == 4){
    nldas <- readRDS("./4_results/delta/nldas_sim_slate_forest_unburn")
  }
  nldas$year <- as.numeric(substr(nldas$date,1,4))
}else if (grepl("chiquito",desired,ignore.case = T)){
  alexi <- readRDS("./4_results/creek/alexi_chiquito_ET")
  target_date <- as.Date("2020-09-04") # fire start (creek)
  fire = 'creek'
  nldas <- readRDS('./4_results/creek/nldas_sim_chiquito')
  modis <- readRDS("./4_results/creek/MODIS_chiquito_ET2")
  creek <- vect('./watershedbounds/chiquitobounds') # slate creek sac river
  creek <- project(creek,"epsg:4326")
  polygon_n = 2
  wtr_n = polygon_n+10
  #nldas$year <- as.numeric(substr(nldas$begindate,1,4))
  # modis <- readRDS("./4_results/creek/MODIS_chiquito_ET")
  
}else{
  
  stop("watershed not correct, try again\n") 
}

nldas$year <- as.numeric(substr(nldas$date,1,4))
# loading data ------------------------------------------------------------
creek <- creek[polygon_n]
creek$name <- gsub("-.*","",creek$name)
modis1 <- modis


flist <- lapply(alexi$ETdaily_mean, function(x) x[polygon_n]) 
nanflist <- lapply(alexi$pnan, function(x) x[polygon_n]) 

alexi$ETslate <- unlist(flist)
alexi$pnan_pol = unlist(nanflist)
flist <- lapply(modis$ETdaily_mean, function(x) x[polygon_n])


modis$ETslate <- unlist(flist)
nanflist <- lapply(modis$pnan, function(x) x[polygon_n]) 
modis$pnan_pol = unlist(nanflist)

# alexi$ETslate[alexi$ETslate==0] <- NA
alexi<-alexi[!is.na(alexi$ETslate),]
alexi<-alexi[complete.cases(alexi$ETslate),]# [!is.nan(alexi$ETslate),]

# compare for 2018 on
# stop()
modis <- modis[modis$year >= 2018, ]
modis <- modis[modis$year <= 2022, ]

#modis <- modis[20:235,]
modis$ETslate <- modis$ETslate*(1/8) # day conversion!

eco <- alexi



# date columns to Date class
alexi$begindate <- as.Date(alexi$begindate)
modis$begindate <- as.Date(modis$begindate)

# moving avg -----------------------------------------------------------

# function to calculate moving average
calendar_moving_average <- function(data, dates, window_size) {
  ma <- numeric(length(data))
  n <- numeric(length(data))
  sdev <- numeric(length(data))
  for (i in 1:length(data)) {
    current_date <- dates[i]
    start_date <- current_date - (window_size - 1)
    relevant_data <- data[dates >= start_date & dates <= current_date]
    n[i] = length(relevant_data)
    ma[i] <- mean(relevant_data, na.rm = TRUE)
    sdev[i]<-sd(relevant_data,na.rm=T)
  }
  return(data.frame(mean_value = ma,obs = n,sdev=sdev))
}

# calculating 60 day moving avg
window_size <- 60
alexi$ET_60day_avg <- calendar_moving_average(alexi$ETslate, alexi$begindate, window_size)$mean_value
alexi$n_days <- calendar_moving_average(alexi$ETslate, alexi$begindate, window_size)$obs
alexi$ET_sd <- calendar_moving_average(alexi$ETslate, alexi$begindate, window_size)$sdev

modis$ET_60day_avg <- calendar_moving_average(modis$ETslate, modis$begindate, window_size)$mean_value
modis$n_days <- calendar_moving_average(modis$ETslate, modis$begindate, window_size)$obs
modis$ET_sd <- calendar_moving_average(modis$ETslate, modis$begindate, window_size)$sdev

#nldas$ET_60day_avg <- calendar_moving_average(nldas$ETdaily_mean, nldas$date, window_size)
nldas$ET_min_60d_avg <- calendar_moving_average(nldas$ETdailymin,nldas$date,60)$mean_value
nldas$ET_max_60d_avg <- calendar_moving_average(nldas$ETdaily_max,nldas$date,60)$mean_value



# plotting 60-day moving avgs ---------------------------------------------
# png(filename = paste0("./4_results/",fire,"/",creek$name,'_60_day.png'),width = 650, height = 480) # comment out if not saving image
plot(modis$begindate, modis$ET_60day_avg, col = "black",type='l',cex=0.7,
     xlab = "Date", ylab = "ET (mm/day)",pch=20, main = 
      paste0(creek$name," (",wtr_n,")"),ylim = c(0,max(nldas$ET_max_60d_avg)+5),lwd = 1.5,las =1)#,xaxt = "n")
#axis(1, modis$begindate,format(modis$begindate,"%m-%Y"),las = 2,cex = 0.5) # change based on watershed
rect(0, par("usr")[3], as.numeric(target_date), par("usr")[4], col = rgb(.82,.82,.82,0.5), border = NA)

lines(modis$begindate, modis$ET_60day_avg, col = "black",type='l',cex=0.7,
      xlab = "Date", ylab = "ET (mm/day)",pch=20, main = 
        creek$name,ylim = c(0,10),lwd = 1.5)
lines(alexi$begindate, alexi$ET_60day_avg, type = "l", col = "red",lwd = 1.5)#,pch=20)
lines(nldas$date,nldas$ET_60day_avg,type = 'l',col='darkorange',lty = 1,lwd = 1.25)
polygon(c(nldas$date, rev(nldas$date)),
        c(nldas$ET_min_60d_avg, rev(nldas$ET_max_60d_avg)),
        col = rgb(0.8, 0.6, 0.2, 0.25), border = NA)
#abline(v = as.numeric(as.Date("09-05-2018", format = '%m-%d-%Y')), col = 'blue')

legend("topright", legend = c("ECOSTRESS", "MODIS","NLDAS"), col = c( "red","black","darkorange"),
       lty=c(1,1,1),cex = .75,horiz = F,lwd = 2,bg=NA)
legend('topleft', legend = c('pre-fire', 'post-fire'),  fill = c(rgb(.82, .82, .82, 0.5), 'white'), bty = 'n',pt.bg = 'black')

# dev.off()

# ci test -----------------------------------------------------------------

alexi$ET_sd[is.na(alexi$ET_sd)] <- 0
#  ALEXI

alexi$upper_ci <- alexi$ET_60day_avg + 1.96 * alexi$ET_sd / sqrt(alexi$n_days)
alexi$lower_ci <- alexi$ET_60day_avg - 1.96 * alexi$ET_sd / sqrt(alexi$n_days)

#  MODIS

modis$upper_ci <- modis$ET_60day_avg + 1.96 * modis$ET_sd / sqrt(modis$n_days)
modis$lower_ci <- modis$ET_60day_avg - 1.96 * modis$ET_sd / sqrt(modis$n_days)

# plotting with conf ints
# png(filename = paste0("./4_results/",fire,"/",creek$name,'_60_day_CI.png'), width = 650, height = 480)
plot(modis$begindate, modis$ET_60day_avg, col = "black", type='n',
     xlab = "", ylab = "", #main = creek$name,
     ylim = c(0, max(nldas$ET_max_60d_avg, modis$upper_ci, alexi$upper_ci, na.rm = TRUE) + 5),
     las = 1)

#  pre-fire background
rect(par("usr")[1], par("usr")[3], as.numeric(target_date), par("usr")[4], 
     col = rgb(.82,.82,.82,0.5), border = NA)

# modis ci
polygon(c(modis$begindate, rev(modis$begindate)),
        c(modis$upper_ci, rev(modis$lower_ci)),
        col = rgb(0, 0, 0, 0.2), border = NA)

# ecostress ci
polygon(c(alexi$begindate, rev(alexi$begindate)),
        c(alexi$upper_ci, rev(alexi$lower_ci)),
        col = rgb(1, 0, 0, 0.2), border = NA)

# nldas ci
polygon(c(nldas$date, rev(nldas$date)),
        c(nldas$ET_max_60d_avg, rev(nldas$ET_min_60d_avg)),
        col = rgb(0.8, 0.6, 0.2, 0.25), border = NA)

# 60 day means
lines(modis$begindate, modis$ET_60day_avg, col = "black", lwd = 1.25)
lines(alexi$begindate, alexi$ET_60day_avg,col = "red", lwd = 1.25)
lines(nldas$date, nldas$ET_60day_avg, col = "darkorange", lwd = 1.25)

# legend
# legend("topright", 
#        legend = c("ECOSTRESS", "MODIS", "NLDAS"), 
#        col = c("red", "black", "darkorange"),
#        lty = 1, cex = 0.75, lwd = 2, bg = NA)
# 
# legend('topleft', 
#        legend = c('pre-fire', 'post-fire'),  
#        fill = c(rgb(.82, .82, .82, 0.5), 'white'), 
#        bty = 'n')
# dev.off()
# stop()
# plot test ---------------------------------------------------------------

# correlation plots -------------------------------------------------------
cl <- fclos(alexi,modis)
eco_modis_cmp <- modis[,c('begindate','ET_60day_avg')]
names(eco_modis_cmp) <- c('moddate','modis')
eco_modis_cmp$alexi <- alexi$ET_60day_avg[cl$ind]
eco_modis_cmp$alexi_pnan <- alexi$pnan_pol[cl$ind]
eco_modis_cmp$aldate <- alexi$begindate[cl$ind]
eco_modis_cmp <- eco_modis_cmp[!is.na(cl$ind),]

cor_mod_eco <- cor(eco_modis_cmp$alexi,eco_modis_cmp$modis,use = "complete.obs")

pct_diff <- 100*(eco_modis_cmp$alexi-eco_modis_cmp$modis)/eco_modis_cmp$modis

# png(filename = paste0("./4_results/",fire,'/',desired,'_',creek$name,'_pnan_pdiff.png'),width = 650, height = 480,res= 100)

plot(eco_modis_cmp$alexi_pnan,pct_diff,pch = 20,col = rgb(0,0,0,0.5),
     xlab = "Proportion of Watershed with Missing Data",
     ylab = "% Difference between MODIS and ECOSTRESS",main = creek$name)
m <- lm(pct_diff~eco_modis_cmp$alexi_pnan)
abline(m,col='red')

# dev.off()

# finding closest nldas dates to modis

names(nldas)[1] <- "begindate"

ncl <- fclos(modis, nldas)
mod_nldas_cmp <- nldas[,c('begindate','ET_60day_avg')]
names(mod_nldas_cmp) <- c('nlddate','nldas')
mod_nldas_cmp$modis <- modis$ET_60day_avg[ncl$ind]
mod_nldas_cmp$moddate <- ncl$cldate
mod_nldas_cmp <- mod_nldas_cmp[!is.na(ncl$ind),]
cor_nld_mod <- cor(mod_nldas_cmp$modis, mod_nldas_cmp$nldas)
modiscmp <- modis[ncl$ind,]

nldec <- fclos(nldas,eco)

eco_nld_cmp <- data.frame(ecodate = eco$begindate,ecoav = alexi$ET_60day_avg)
eco_nld_cmp$nldas <- nldas$ET_60day_avg[nldec$ind]
eco_nld_cmp$nlddate <- nldec$cldate
eco_nld_cmp <- eco_nld_cmp[!is.na(nldec$ind),]
cor_eco_nld <- cor(eco_nld_cmp$ecoav,eco_nld_cmp$nldas)

cdf <-matrix(nrow=3,ncol=3)
cdf[1,2] <- cor_mod_eco
cdf[1,3] <- cor_nld_mod
cdf[3,2] <- cor_eco_nld
cdf[2,3] <- cor_eco_nld


rmse <- function(predictions, actual_values) {
  mse <- mean((predictions - actual_values)^2, na.rm = TRUE)
  rmse_value <- sqrt(mse)
  return(rmse_value)
}

pb<- function(predictions, actual_values) {
  a <- predictions - actual_values
  numerator <- sum(a,na.rm=T)
  denominator <- sum(actual_values,na.rm=T)
  pb <- (numerator / denominator) * 100
  return(pb)
}
rmse_eco_mod <- rmse(eco_modis_cmp$alexi,eco_modis_cmp$modis)
rmse_mod_nldas <- rmse(mod_nldas_cmp$nldas,mod_nldas_cmp$modis)
rmse_eco_nld <- rmse(eco_nld_cmp$nldas,eco_nld_cmp$ecoav)


pb_mod_eco <- pb(eco_modis_cmp$alexi,eco_modis_cmp$modis)
pb_mod_nldas<- pb(mod_nldas_cmp$nldas,mod_nldas_cmp$modis)
pb_eco_nld <- pb(eco_nld_cmp$nldas,eco_nld_cmp$ecoav)


results <- data.frame(
  comparison = c("MODIS vs. ECOSTRESS", "MODIS vs. NLDAS", "ECOSTRESS vs. NLDAS"),
  correlation = c(cdf[1,2], cdf[1,3], cdf[3,2]),
  rmse = c(rmse_eco_mod, rmse_mod_nldas, rmse_eco_nld),
  percent_bias = c(pb_mod_eco, pb_mod_nldas, pb_eco_nld)
)

results




