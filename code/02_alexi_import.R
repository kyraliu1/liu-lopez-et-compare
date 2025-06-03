# imports ecostress disalexi ET data and gets data for desired watershed
# applies quality flag
# saves as RDS
# setup -------------------------------------------------------------------


wd <- '/media/kyra/KL_REU231'
setwd(wd)
library(terra)


# watershed input ---------------------------------------------------------
desired <- 'slate'#readline(prompt = "enter desired watershed: \n")
if (grepl("mad",desired,ignore.case = T)){
  fdir <- './data/august/alexi_mad'
 fire = 'august'
  creek <-  project(vect('./watershedbounds/madbounds'),'epsg:4326') # mad river
  finame = './4_results/august/alexi_mad_ET' 
  
}else if (grepl("slate",desired,ignore.case = T)){
  fdir <- './data/delta/alexi_slate'
  fire = 'delta'
  creek <- project(vect('./watershedbounds/slatebounds'),'epsg:4326') # slate creek sac river

  finame = "./4_results/delta/alexi_creek_ET"
}else if (grepl("chiquito",desired,ignore.case = T)){
  fdir <- './data/creek/alexi_chiq'
  fire ='creek'
  creek <- project(vect('./watershedbounds/chiquitobounds'),'epsg:4326') # slate creek sac river
  
  finame = "./4_results/creek/alexi_chiquito_ET"
}else{
 
  stop("watershed not correct, try again\n") 
}
# creek <- aggregate(creek)
maskfi <- list.files(fdir,pattern = 'QualityFlag_',full.names = T)
fi <- list.files(fdir,pattern = 'daily_',full.names = T)
gid = list.files(fdir,pattern = 'daily_')
maskid = list.files(fdir,pattern = 'QualityFlag_')

maskd <- substr(maskid,57,69) #quality flag id
fid <- substr(gid,53,65) # et file id
# processing --------------------------------------------------------------



ETcolors = colorRampPalette(c("#f6e8c3", "#d8b365", "#99974a", "#53792d", "#6bdfd2", "#1839c5"))(50)





# getting only quality flags that match data days

idx <- numeric(length(maskfi))
id2 <- numeric(length(fi))
for (i in 1:length(maskfi)){
  st <- maskd[i]
  for (j in 1:length(fi)){
    sw <- fi[j]
    if (grepl(st,sw)){
      idx[i] = j
      
    }
    
  }
  
  if (idx[i]>0){
    id2[i] = i
  }
}

maskfi <- maskfi[idx]




# importing
ET <- numeric(length(fi))
ET_sum <- numeric(length(fi))
pnan <- numeric(length(fi))
dt <- character(length(fi))
stdev <- numeric(length(fi))
pb <- txtProgressBar(1,length(fi),1)

for (i in 1:length(fi)){
  
 
  a <- rast(fi[i])
 if (i==1){
    creek <- project(creek,crs(a))
  }
  m <- rast(maskfi[i])
  m <- crop(m,ext(a))

  m <- resample(m,a)
  a <-mask(a, m ,inverse = T, maskvalues = 0)
  
  acreek = crop(a,creek,mask = T)
  acreek_nan <- freq(a,value = NA,zones = creek)
  totalin = extract(a,creek,fun = ncell)
# png(filename = paste0("./4_results/",fire,"/",desired,i,'map.png'),width = 650, height = 480)
  plot(creek,border=NA,col=NA,box=F,axes=F)
 plot(a,col= ETcolors,box = F, axes = F,add=T)
 plot(creek,add = T,lwd = 4)
 # plot(m,add = T,alpha = 0.5)

# dev.off()
 # ET_sum[i] <- extract(a,creek,fun = 'sum',na.rm=T,ID = F)
 ET[i] <- extract(a,creek,fun = 'mean',na.rm=T,ID = F)
 pnan[i] <-list(acreek_nan$count/totalin[,2])
 stdev[i] <- extract(a,creek,fun = 'sd',na.rm = T, ID = F)
 dt[i] <- fid[i]
# dt[i] <- as.POSIXlt(dt, format = "%Y%j%H%M%S")
 setTxtProgressBar(pb,i)
 
}


dt <- unlist(dt)
dd <- as.POSIXlt(dt, format = "%Y%j%H%M%S")
year <- as.numeric(substr(dt,1,4))
alexi <- data.frame( begindate = substr(dd,1,10), 
                    day = as.numeric(substr(dd,9,10)),
                    month = as.numeric(substr(dd,6,7)),
                    year=year )

alexi$ETdaily_mean = ET
alexi$stdev = stdev
alexi$pnan <- pnan
saveRDS(alexi,finame)


