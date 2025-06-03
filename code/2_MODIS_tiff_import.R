# imports modis ET data and gets data for desired watershed
# applies quality flag
# saves as rds
# setup -------------------------------------------------------------------


wd <- '/media/kyra/KL_REU23'

setwd(wd)
library(terra)

# watershed input ---------------------------------------------------------
desired <- "slate"#readline(prompt = "enter desired watershed: \n")
if (grepl("chiquito",desired,ignore.case = T)){
  fdir = './data/creek/chiq_modis'
  
  creek1  <- vect("./watershedbounds/chiquitobounds")
  finame = "./4_results/creek/MODIS_chiquito_ET2"
  
}else if (grepl("slate",desired,ignore.case = T)){
  fdir = "/media/kyra/KL_REU23/data/delta/slate_modis_2"
  
  creek1  <- vect("./watershedbounds/slatebounds")
  finame = "./4_results/delta/MODIS_slate_ET2"
}else if (grepl("mad",desired,ignore.case = T)){
  fdir = "/media/kyra/KL_REU23/data/august/mad_modis"

  
  creek  <- vect("./watershedbounds/madbounds")
  finame = "./4_results/august/MODIS_mad_ET2"
}else{
  
  stop("watershed not correct, try again\n") 
}

maskfi <- list.files(fdir,pattern = 'QC',full.names = T)
fi <- list.files(fdir,pattern = 'ET_500m',full.names = T) 
gid = list.files(fdir,pattern = 'ET_500m')
maskid = list.files(fdir,pattern = 'QC')

maskd <- substr(maskid,29,35) #quality flag id
fid <- substr(gid,26,32) # et file id

# processing --------------------------------------------------------------



ETcolors = colorRampPalette(c("#f6e8c3", "#d8b365", "#99974a", "#53792d", "#6bdfd2", "#1839c5"))(50)


modfi <- rast(fi[1])
creek <- project(creek1,crs(modfi))


ET <- numeric(length(fi))
ET_sum <- numeric(length(fi))
pnan <- numeric(length(fi))
dt <- character(length(fi))
stdev <- numeric(length(fi))
pb <- txtProgressBar(1,length(fi),1)

for (i in 1:length(fi)){
  a <- rast(fi[i])
  a[a>3270 | a <= -3276.7] <- NA 

  acreek = crop(a,creek,mask = T)
  acreek_nan <- freq(a,value = NA,zones = creek)
  totalin = extract(a,creek,fun = ncell)
  plot(a,col= ETcolors,box = F, axes = F)
  plot(creek,add = T,lwd = 2)
  # plot(m,add = T,alpha = 0.5)
  ET[i] <- zonal(a,creek,fun = 'mean',na.rm=T,exact=T)

  pnan[i] <- data.frame(acreek_nan$count/totalin[,2])
  stdev[i] <- extract(a,creek,fun = 'sd',na.rm = T, ID = F)
  dt[i] <- fid[i]
  setTxtProgressBar(pb,i)
  rm(a)
}
close(pb)


dt <- unlist(dt)
dd <- as.POSIXlt(dt, format = "%Y%j")
year <- as.numeric(substr(dt,1,4))
alexi <- data.frame( begindate = substr(dd,1,10), 
                     day = as.numeric(substr(dd,9,10)),
                     month = as.numeric(substr(dd,6,7)),
                     year=year)

alexi$ETdaily_mean = ET

alexi$stdev = stdev
alexi$pnan <- pnan

saveRDS(alexi,finame)

