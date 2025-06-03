
# water year comparison 
# water year sum and pct change over time

# setup -------------------------------------------------------------------


wd <- '/media/kyra/KL_REU231'

setwd(wd)
library(terra)
library(dplyr)
library(lubridate)

# loading data ------------------------------------------------------------
desired <-'chiquito' # readline(prompt = "enter desired watershed: \n")# 
if (grepl("mad",desired,ignore.case = T)){
  fire = data.frame(name = 'august', month = 8, day = 16, year = 2020 )
  
  alexi <- readRDS('./4_results/august/alexi_mad_ET')
  
  nldas <- readRDS('./4_results/august/nldas_sim_mad_12')
  creek <- vect('./watershedbounds/madbounds') # mad river
  
  modis <- readRDS('./4_results/august/MODIS_mad_ET')
  target_date <- as.Date("2020-08-16") # fire start (august)
  polygon_n = 3
  wtr_name = "Mad River"
  xm = 57.5
}else if (grepl("slate",desired,ignore.case = T)){
  alexi <- readRDS("./4_results/delta/alexi_creek_ET")
  xm=51.5
  target_date <- as.Date("2018-09-05") # fire start (delta)
  
  nldas <- readRDS('./4_results/delta/nldas_sim_slate')
  fire = data.frame(name = 'delta', month = 9, day = 5, year = 2018 )
  creek  <- vect("./watershedbounds/slatebounds") # slate creek sac river
  
  polygon_n = 1
  
  wtr_name = "Slate Creek"
  nldas2 <- readRDS('./4_results/delta/nldas_sim_slate_forest')
  

    nldas3 <- readRDS("./4_results/delta/nldas_sim_slate_forest_unburn")
  
  nldas$year <- as.numeric(substr(nldas$date,1,4))
  
  nldas$ETdaily_mean = (nldas$ETdaily_mean + nldas2$ETdaily_mean+nldas3$ETdaily_mean )/3
  
  nldas$ETdailymin = (nldas$ETdailymin + nldas2$ETdailymin+nldas3$ETdailymin )/3
  nldas$ETdaily_max = (nldas$ETdaily_max + nldas2$ETdaily_max+nldas3$ETdaily_max)/3
  
  
  nldas$year <- as.numeric(substr(nldas$date,1,4))
  modis <- readRDS("./4_results/delta/MODIS_slate_ET2")
  
}else if (grepl("chiquito",desired,ignore.case = T)){
  alexi <- readRDS("./4_results/creek/alexi_chiquito_ET")
  target_date <- as.Date("2020-09-04") # fire start (creek)
  fire =  data.frame(name = 'creek', month = 9, day = 4, year = 2020 )
  nldas <- readRDS('./4_results/creek/nldas_sim_chiquito')
  modis <- readRDS("./4_results/creek/MODIS_chiquito_ET")
  creek <- project(vect('./watershedbounds/chiquitobounds'),'epsg:4326') # slate creek sac river
  xm=54.5
  polygon_n = 1
  
  wtr_name = "Chiquito Creek"
  
}else{
  
  stop("watershed not correct, try again\n") 
}
creek$name <- gsub("-.*","",creek$name)


subwtr = creek[polygon_n]$name
nldas$year <- as.numeric(substr(nldas$date,1,4))



flist <- lapply(alexi$ETdaily_mean, function(x) x[polygon_n]) # 2 for lost creek, 1 for slate creek
alexi$ETslate <- unlist(flist)
flist <- lapply(modis$ET, sum)#function(x) x[polygon_n])
modis$ETslate <- unlist(flist)

alexi<-alexi[!is.na(alexi$ETslate),]


# water year --------------------------------------------------------------

modis$begindate <- as.Date(modis$begindate)
alexi$begindate <- as.Date(alexi$begindate)

#function to calculate the water year
getWaterYear <- function(date) {
  if (as.numeric(format(date, "%m")) >= 10) {
    return(as.numeric(format(date, "%Y")) + 1)
  } else {
    return(as.numeric(format(date, "%Y")))
  }
}

check_complete_water_year <- function(dates) {
  months_in_year <- unique(month(dates))
  return(length(months_in_year) == 12)
}



nldas$WaterYear <- sapply(nldas$date,getWaterYear)
modis$WaterYear <- sapply(modis$begindate, getWaterYear)
alexi$WaterYear <- sapply(alexi$begindate,getWaterYear)



filter_complete_water_years <- function(df, date_column) {
  df %>%
    group_by(WaterYear) %>%
    filter(check_complete_water_year(get(date_column))) %>%
    ungroup()
}


modis_complete <- modis %>%
  filter_complete_water_years("begindate")

nldas_complete <- nldas %>%
  filter_complete_water_years("date")

alexi_complete <- alexi %>%
  filter_complete_water_years("begindate")


# mean
mod_wys <- modis_complete %>% 
  group_by(WaterYear) %>% 
  summarize(sumValue = sum(ETslate, na.rm = TRUE))
nld_wys <- nldas_complete %>%
  group_by(WaterYear)%>%
  summarize(sumValue = sum(ETdaily_mean,na.rm = T),
            ET_min_sum = sum(ETdailymin, na.rm = TRUE),
            ET_max_sum = sum(ETdaily_max, na.rm = TRUE))
alx_wys <- alexi %>% 
  group_by(WaterYear) %>% 
  summarize(sumValue = sum(ETslate, na.rm = TRUE))
# regression --------------------------------------------------------------
mod_wys <- as.data.frame(mod_wys)
nld_wys <- as.data.frame(nld_wys)

alx_wys <- alx_wys[-6,]

mod_pre <- mean(mod_wys[1:(fire$year - min(mod_wys$WaterYear)),2]) # 2002 because took out 2001
mod_post <- mean(mod_wys[(fire$year + 1 -  min(mod_wys$WaterYear)):nrow(mod_wys),2])
nld_pre <- mean(nld_wys[1:(fire$year - min(nldas$year)),2])
nld_post <- mean(nld_wys[(fire$year+1 -  min(nldas$year)):nrow(nld_wys),2]) # nld post
(mod_post-mod_pre)/mod_pre
(nld_post-nld_pre)/nld_pre
# plotting ----------------------------------------------------------------
# png(filename = paste0("./4_results/",fire$name,'/',desired,'_',wtr_name,'_year_sum.png'),width = 650, height = 480,res= 100)
plot(mod_wys,  main = wtr_name,cex.main = 2,cex.lab = 1.2,cex.axis = 1,
        ylab = 'sum of ET over Water Year (mm/year)',xlab  = 'Water Year',
     pch=20,type = 'b',ylim =c(0,max(nld_wys$ET_max_sum)),col = 'black' ) 
rect(0, par("usr")[3],getWaterYear(target_date)+ as.numeric(format(target_date, "%m"))/12, par("usr")[4], col = rgb(.82,.82,.82,0.5), border = NA)
points(mod_wys,type = 'b',pch = 20)
points(nld_wys,pch = 20,type = 'b',col = 'darkorange',lwd = 2)
#points(alx_wys,type = 'b',col = 'red',lwd = 2)
abline(h = mod_pre,lwd = .75) # modis pre fire
abline(h = mod_post,lty = 2,lwd = .75) # modis post fire
abline(h = nld_pre,col='darkorange',lwd = .75) #nldas pre fire
abline(h = nld_post,lty = 2,lwd = 1,col='darkorange') # nldas post fire

polygon(c(nld_wys$WaterYear, rev(nld_wys$WaterYear)),
        c(nld_wys$ET_min_sum, rev(nld_wys$ET_max_sum)),
        col = rgb(0.8, 0.6, 0.2, 0.25), border = NA) #nldas mc sim


# dev.off()
nld_change <- nld_wys %>%
  arrange(WaterYear) %>%
  mutate(percent_change = (sumValue / lag(sumValue) - 1) * 100)


nld_change = nld_change[c("WaterYear","percent_change")]
mod_change <- mod_wys %>%
arrange(WaterYear) %>%
mutate(percent_change = (sumValue / lag(sumValue) - 1) * 100)

mod_change = mod_change[c("WaterYear","percent_change")]

merged_df <- merge(mod_change, nld_change, by = "WaterYear",all.x=T)
data_matrix <- as.matrix(merged_df[, -1])


rownames(data_matrix) <- merged_df$WaterYear
data_matrix <- data_matrix[-1,]

# png(filename = paste0("./4_results/",fire$name,'/',desired,'_',wtr_name,'_year_pct_diff.png'),width = 650, height = 480,res= 100)

barplot(  t(data_matrix),
  beside = TRUE,
  col = NA, 

  names.arg = rownames(data_matrix), 

  xlab = "Water Year",
  ylab = "% Change",
  border=NA
)
rect(0,-50,xm,30,border = NA,col = rgb(.5,.5,.5,.3))
barplot(  t(data_matrix),add=T,
          beside = TRUE,
          col = c("black", "darkorange"), 
          
          names.arg = rownames(data_matrix), 
          cex.lab = 1.35,
          xlab = "Year",
          ylab = "% Change",
          border=NA
)
# title(main = wtr_name,cex = 2,line = .5)
 # dev.off()
# legend(x = 1,y = -25,legend = c("MODIS","NLDAS"),bty = "n",fill = c('black','darkorange'))
print(c('nldas',
nld_pre,
nld_post,
100*(nld_post-nld_pre)/nld_pre))

print(c("modis",
mod_pre,
mod_post,
100*(mod_post-mod_pre)/mod_pre))

