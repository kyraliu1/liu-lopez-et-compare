# map of california with watersheds

# setup -------------------------------------------------------------------


wd <- "/media/kyra/KL_REU231" # kyra pop wd
#wd <- 'D:/' # kyra windows wd
setwd(wd)

library(maptiles)
library(terra)
library(geodata)


# import ------------------------------------------------------------------


slate = vect('./watershedbounds/slatebounds')
mad <- vect('./watershedbounds/madbounds') # mad river
chiq <- project(vect('./watershedbounds/chiquitobounds'),'epsg:4326') # slate creek sac river

lev = 7

usa = gadm("usa",level =2,path = ".")
cali = usa[usa$NAME_1=="California"]

# plot --------------------------------------------------------------------


plot(cali,col='grey',border='black',box = F,axes=F)

plot(slate,col='red',add=T,border=NA)
plot(mad,col='red',add=T,border = NA)
plot(chiq,col = 'red',add=T,border = NA)
plot(cali,col=NA,border =rgb(0,0,0),lwd = 1,add=T)
sbar(200,xy = c(-121,33),divs = 3,type ='bar',below = "km",cex= .5)
# dev.off()
