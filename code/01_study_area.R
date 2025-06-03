# getting watershed bounds and land use data


# setup -------------------------------------------------------------------

wd <-"/media/kyra/KL_REU231/"
setwd(wd)
# loading packages

library(terra)
library(geodata)
library(openxlsx)

# data import -------------------------------------------------------------

# reading rasters

fr <- list.files("./CSULA_REU/unzipped", pattern = ".tif$", full.names = T)
huc12s <- list.files("./CSULA_REU/CA_HUC12/hydrologic_units_WBDHU12_ca_4130927_01",  full.names = T)

huc12s <- vect(huc12s)
# getting relevant watersheds

desired = 'mad' # mad, chiquito
if (desired == 'chiquito'){
creek <- huc12s[grep("1804000607",huc12s$huc12, ignore.case = T)]
name = "Chiquito Creek: HUC-1804000607"
pngname = "/media/kyra/KL_REU23/4_results/creek/chiquito_lu.png"
  if (!file.exists("./watershedbounds/chiquitobounds/chiquitobounds.shp")){
    dir.create('./watershedbounds/chiquitobounds')
    writeVector(creek,"./watershedbounds/chiquitobounds/chiquitobounds.shp")}

}else if (desired =='slate'){
  creek <- huc12s[grep("1802000503",huc12s$huc12, ignore.case = T)]
  name = "Slate Creek - Sacramento River: HUC-1802000503"
  pngname = "/media/kyra/KL_REU23/4_results/delta/slate_lu.png"

    if (!file.exists("./watershedbounds/slatebounds")){
      dir.create("./watershedbounds/slatebounds")
      writeVector(creek,"./watershedbounds/slatebounds/slatebounds.shp")
  
    }
}else if (desired =='mad'){
  creek <- huc12s[grep("1801010202",huc12s$huc12, ignore.case = T)]
  name = "Upper Mad River: HUC-1801010202"
  pngname = "/media/kyra/KL_REU23/4_results/delta/slate_lu.png"
  if (!file.exists("./watershedbounds/madbounds")){
    dir.create("./watershedbounds/madbounds")
    writeVector(creek,"./watershedbounds/madbounds/madbounds.shp")
  }
}
creek$areasqkm
# land use ----------------------------------------------------------------

## land use raster
lan <- rast(fr[[10]])
#creek <- creek
# cropping to desired
creek <- project(creek,crs(lan)) # projecting so crs matches
c2 <- creek[3]

creeklu <- crop(lan,c2, mask = T) # crop

clu <- cats(creeklu)[[1]]
creeklu <- categories(creeklu, layer = 1, active = 5, value = clu)
# png(pngname,width = 650, height = 480)

plot(creeklu,plg = list(cex = 0.8),axes = F,main = name)
plot(c2,add= T,col=NA,border = 'black')
text(c2,halo=T)
# dev.off()
# work around using summary
su <- summary(creeklu, size = 154840)
su <- as.character(su)

su <-strsplit(su, ":")
for (i in 1:7){
  su[[i]][2] <- gsub(" ","",su[[i]][2])
}

plu <- do.call(rbind, su)
plu <- as.data.frame(plu)
names(plu) <- c("type","count")
plu$count <- as.numeric(plu$count)

plu$percentage <- NA
for (i in 1:6){
  plu$percentage[i] <- 100*plu$count[i]/sum(plu$count[1:6])
}
# plu$percentage[7] <- NA

plu
