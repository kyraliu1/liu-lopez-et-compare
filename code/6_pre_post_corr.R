# get stats for pre and post fire separately, save to csv

# setup -------------------------------------------------------------------


wd <- '/media/kyra/KL_REU231'

setwd(wd)
library(terra)

library(dplyr)
library(corrplot)
source("./processing_scripts/functions/fclos.R")

# watershed import --------------------------------------------------------
desired <- 'chiquito'#readline(prompt = "enter desired watershed: \n")                                                 

if (grepl("mad",desired,ignore.case = T)){
  id = 7
  polygon_n = 1

  fire = 'august'
  alexi <- readRDS('./4_results/august/alexi_mad_ET' )
  creek <- vect('./watershedbounds/madbounds') # mad river
  nldas <- readRDS('./4_results/august/nldas_sim_mad_12')
  
  modis <- readRDS('./4_results/august/MODIS_mad_ET2')
  target_date <- as.Date("2020-08-16") # fire start (august)
}else if (grepl("slate",desired,ignore.case = T)){
  id=0
  alexi <- readRDS("./4_results/delta/alexi_creek_ET")
  target_date <- as.Date("2018-09-05") # fire start (delta)
  fire = 'delta'
  nldas <- readRDS('./4_results/delta/nldas_sim_slate')
  creek  <- vect("./watershedbounds/slatebounds") # slate creek sac river
  
  polygon_n = 1
  
  if (polygon_n %in% c(6,7)){
    nldas <- readRDS('./4_results/delta/nldas_sim_slate_forest')
  }
  if (polygon_n == 4){
    nldas <- readRDS("./4_results/delta/nldas_sim_slate_forest_unburn")
  }
  nldas$year <- as.numeric(substr(nldas$date,1,4))
  #modis <- readRDS('./modis_creek_ET')
  modis<-readRDS("./4_results/delta/MODIS_slate_ET2")
}else if (grepl("chiquito",desired,ignore.case = T)){
  id = 10
  alexi <- readRDS("./4_results/creek/alexi_chiquito_ET")
  target_date <- as.Date("2020-09-04") # fire start (creek)
  fire = 'creek'
  nldas <- readRDS('./4_results/creek/nldas_sim_chiquito')
  modis <- readRDS("./4_results/creek/MODIS_chiquito_ET2")
  creek <- vect('./watershedbounds/chiquitobounds') # slate creek sac river
  creek <- project(creek,"epsg:4326")
  polygon_n = 2

}else{
  
  stop("watershed not correct, try again\n") 
}

nldas$year <- as.numeric(substr(nldas$date,1,4))
# loading data ------------------------------------------------------------
creek <- creek[polygon_n]
#polygon_n <- polygon_n + 7
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

alexi<-alexi[!is.na(alexi$ETslate),]


# compare for 2018 on

modis <- modis[modis$year >= 2018, ]
modis <- modis[modis$year <= 2022, ]

#modis <- modis[20:235,]
modis$ETslate <- modis$ETslate*(1/8) # unit conversion!

eco <- alexi
# find closest date (eco , modis)

cl <- fclos(eco,modis)
eco_modis_cmp <- modis[,c('begindate','ETslate')]

# closest dates
modis$closest_alexidate <- cl$cldate
modis$alexi_values <- NA
modis$alexi_values[is.na(cl$ind)] <- alexi$ETslate[is.na(cl$ind)]
dif <- modis$ETslate- modis$alexi_values

difor <- order(abs(dif),decreasing = T)
ord <- data.frame(av = modis$alexi_values[difor], mv = modis$ETslate[difor], 
                  mdate = modis$begindate[difor], edate = alexi$begindate[difor],
                  dif = dif[difor])




# date 
alexi$begindate <- as.Date(alexi$begindate)
modis$begindate <- as.Date(modis$begindate)

# moving avg -----------------------------------------------------------

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

# calculating 60 day moving avg
window_size <- 60
alexi$ET_60day_avg <- calendar_moving_average(alexi$ETslate, alexi$begindate, window_size)
modis$ET_60day_avg <- calendar_moving_average(modis$ETslate, modis$begindate, window_size)
nldas$ET_min_60d_avg <- calendar_moving_average(nldas$ETdailymin,nldas$date,60)
nldas$ET_max_60d_avg <- calendar_moving_average(nldas$ETdaily_max,nldas$date,60)

modis$al_60day <- calendar_moving_average(modis$alexi_values, modis$begindate, window_size)

# 8 day moving avg for nldas

nldas$ET_8day_avg <- calendar_moving_average(nldas$ETdaily_mean, nldas$date, 8)

nldas1 <- nldas[seq(from= 1,by = 8, length.out = 453),]




# correlation plots -------------------------------------------------------

cl <- fclos(alexi,modis)
eco_modis_cmp <- modis[,c('begindate','ET_60day_avg')]
names(eco_modis_cmp) <- c('moddate','modis')
eco_modis_cmp$alexi <-alexi$ET_60day_avg[cl$ind]
eco_modis_cmp$alexi_pnan <- alexi$pnan_pol[cl$ind]
eco_modis_cmp$aldate <- cl$cldate
eco_modis_cmp <- eco_modis_cmp[!is.na(cl$ind),]
cor_mod_eco <- cor(eco_modis_cmp$alexi,eco_modis_cmp$modis)

pct_diff <- 100*(eco_modis_cmp$alexi-eco_modis_cmp$modis)/eco_modis_cmp$modis

# png(filename = paste0("./4_results/",fire,'/',desired,'_',creek$name,'_pnan_pdiff.png'),width = 650, height = 480,res= 100)

plot(eco_modis_cmp$alexi_pnan,pct_diff,pch = 20,col = rgb(0,0,0,0.5),
     xlab = "Proportion of Watershed with Missing Data",
     ylab = "% Difference between MODIS and ECOSTRESS",main = creek$name)
m <- lm(pct_diff~eco_modis_cmp$alexi_pnan)
abline(m,col='red')# dev.off()



# finding closest nldas dates to modis

names(nldas)[1] <- "begindate"

ncl <- fclos(modis, nldas)
mod_nldas_cmp <- nldas[,c('begindate','ET_60day_avg')]
names(mod_nldas_cmp) <- c('nlddate','nldas')
mod_nldas_cmp$modis <- modis$ET_60day_avg[ncl$ind]
mod_nldas_cmp$moddate <- ncl$cldate
nld_modis_cmp <- mod_nldas_cmp[!is.na(ncl$ind),]
cor_mod_nld <- cor(mod_nldas_cmp$modis, mod_nldas_cmp$nldas)
modiscmp <- modis[ncl$ind,]
nldec <- fclos(nldas,eco)

eco_nld_cmp <- data.frame(ecodate = eco$begindate,ecoav = alexi$ET_60day_avg)
eco_nld_cmp$nldas <- nldas$ET_60day_avg[nldec$ind]
eco_nld_cmp$nlddate <- nldec$cldate
eco_nld_cmp <- eco_nld_cmp[!is.na(nldec$ind),]
cor_eco_nld <- cor(eco_nld_cmp$ecoav,eco_nld_cmp$nldas)

# df <- data.frame(MODIS = modis$ET_60day_avg,ECOSTRESS = modis$al_60day,NLDAS = nldascmp$ET_60day_avg)
modis$ET_60day_avg
# c1 <- cor(df$MODIS[1:195],df$NLDAS[1:195])
# c2 <- cor(df$MODIS[1:108],df$ECOSTRESS[1:108])
# c3 <- cor(df$NLDAS[151:235],df$ECOSTRESS[56:140])
cdf <- matrix(nrow=3,ncol=3)
cdf[1,2] <- cor_mod_eco
cdf[1,3] <- cor_mod_nld
cdf[3,2] <- cor_eco_nld
cdf[2,3] <- cor_eco_nld

#plot(df$MODIS[1:108],df$ECOSTRESS[1:108])

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




# pre and post fire stats -------------------------------------------------


# eco and modis
cor(eco_modis_cmp$modis,eco_modis_cmp$alexi)
eco_modis_cmp_pre <- eco_modis_cmp[eco_modis_cmp$moddate < target_date,]
eco_modis_cmp_pre <- eco_modis_cmp[eco_modis_cmp$aldate < target_date,]

eco_nan_pre = mean(eco_modis_cmp_pre$alexi_pnan)
em_pre = cor(eco_modis_cmp_pre$alexi,eco_modis_cmp_pre$modis)

eco_modis_cmp_post <- eco_modis_cmp[eco_modis_cmp$moddate >= target_date,]
eco_modis_cmp_post <- eco_modis_cmp[eco_modis_cmp$aldate >= target_date,]
em_post = cor(eco_modis_cmp_post$alexi,eco_modis_cmp_post$modis)
eco_nan_post = mean(eco_modis_cmp_post$alexi_pnan)

# nld and modis

nld_modis_cmp_pre <- nld_modis_cmp[nld_modis_cmp$moddate < target_date,]
nld_modis_cmp_pre <- nld_modis_cmp[nld_modis_cmp$nlddate < target_date,]

nm_pre = cor(nld_modis_cmp_pre$nldas,nld_modis_cmp_pre$modis)

nld_modis_cmp_post <- nld_modis_cmp[nld_modis_cmp$moddate >= target_date,]
nld_modis_cmp_post <- nld_modis_cmp[nld_modis_cmp$nlddate >= target_date,]
nm_post = cor(nld_modis_cmp_post$nldas,nld_modis_cmp_post$modis)

# nld and eco
eco_nld_cmp_pre <- eco_nld_cmp[eco_nld_cmp$nlddate < target_date,]
eco_nld_cmp_pre <- eco_nld_cmp_pre[eco_nld_cmp$ecodate < target_date,]

ne_pre = cor(eco_nld_cmp_pre$ecoav,eco_nld_cmp_pre$nldas)

eco_nld_cmp_post <- eco_nld_cmp[eco_nld_cmp$nlddate >= target_date,]
eco_nld_cmp_post <- eco_nld_cmp[eco_nld_cmp$ecodate >= target_date,]
ne_post = cor(eco_nld_cmp_post$ecoav,eco_nld_cmp_post$nldas)


rmse_eco_mod_pre <- rmse(eco_modis_cmp_pre$alexi,eco_modis_cmp_pre$modis)
rmse_mod_nldas_pre <- rmse(nld_modis_cmp_pre$nldas,nld_modis_cmp_pre$modis)
rmse_eco_nld_pre <- rmse(eco_nld_cmp_pre$nldas,eco_nld_cmp_pre$ecoav)


pb_mod_eco_pre <- pb(eco_modis_cmp_pre$alexi,eco_modis_cmp_pre$modis)
pb_mod_nldas_pre <- pb(nld_modis_cmp_pre$nldas,nld_modis_cmp_pre$modis)
pb_eco_nld_pre <- pb(eco_nld_cmp_pre$nldas,eco_nld_cmp_pre$ecoav)



rmse_eco_mod_post <- rmse(eco_modis_cmp_post$alexi,eco_modis_cmp_post$modis)
rmse_mod_nldas_post <- rmse(nld_modis_cmp_post$nldas,nld_modis_cmp_post$modis)
rmse_eco_nld_post <- rmse(eco_nld_cmp_post$nldas,eco_nld_cmp_post$ecoav)


pb_mod_eco_post <- pb(eco_modis_cmp_post$alexi,eco_modis_cmp_post$modis)
pb_mod_nldas_post <- pb(nld_modis_cmp_post$nldas,nld_modis_cmp_post$modis)
pb_eco_nld_post <- pb(eco_nld_cmp_post$nldas,eco_nld_cmp_post$ecoav)

results <- data.frame(
  Comparison = c("MODIS vs. ECOSTRESS pre", "MODIS vs. NLDAS pre", "ECOSTRESS vs. NLDAS pre","MODIS vs. ECOSTRESS post", "MODIS vs. NLDAS post", "ECOSTRESS vs. NLDAS post"),
  n_observations = c(nrow(eco_modis_cmp_pre),nrow(nld_modis_cmp_pre),nrow(eco_nld_cmp_pre),nrow(eco_modis_cmp_post),nrow(nld_modis_cmp_post),nrow(eco_nld_cmp_post)),
  Correlation = c(em_pre,nm_pre,ne_pre,em_post,nm_post,ne_post),
  RMSE = c(rmse_eco_mod_pre, rmse_mod_nldas_pre, rmse_eco_nld_pre,rmse_eco_mod_post, rmse_mod_nldas_post, rmse_eco_nld_post),
  Percent_Bias = c(pb_mod_eco_pre, pb_mod_nldas_pre, pb_eco_nld_pre,pb_mod_eco_post, pb_mod_nldas_post, pb_eco_nld_post),
  pnan = c(rep(eco_nan_pre,3),rep(eco_nan_post,3))
)

print(results)
print(nrow(eco_modis_cmp_pre))
print(nrow(nld_modis_cmp_pre))
print(nrow(eco_nld_cmp_pre))

print(nrow(eco_modis_cmp_post))
print(nrow(nld_modis_cmp_post))
print(nrow(eco_nld_cmp_post))



write.csv(results,paste0("./4_results/output/",id+polygon_n,"_",gsub(" ","",creek$name),"_pre_post_stats.csv"))