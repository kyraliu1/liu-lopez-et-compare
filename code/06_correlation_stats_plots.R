
# setup -------------------------------------------------------------------


wd <- '/media/kyra/KL_REU23'

setwd(wd)


# import ------------------------------------------------------------------


fi = "./4_results/all_comparisons1.csv"

df = read.csv(fi)

names(df)[7] <-'percent_bias' 

ME = df[df$Comparison == "ME",]
range(ME$RMSE)

MN = df[df$Comparison == "MN",]
range(MN$percent_bias)

EN = df[df$Comparison == "EN",]
mean(EN[EN$Watershed =="Slate Creek",]$Correlation)

range(EN$percent_bias)
#par(mfrow = c(3,1))


# plor --------------------------------------------------------------------


cp = c("darkorange3","maroon","gold2")[as.factor(df$Comparison)]


png(filename = paste0("./4_results/corr_plot.png"),width = 650, height = 480,res= 100)
#na.arg = c(NA, 1, NA, NA, 2, NA, NA, 3, NA, NA, 4, NA, NA, 5, NA, NA, 6, NA, NA, 7, NA, NA, 8, NA, NA, 9, NA, NA, 10, NA, NA, 11, NA, NA, 12,NA)
barplot(df$Correlation,col=NA,border=NA,ylim = c(-.2,.9),ylab = "Correlation (r)")

rect(0,-.1,25.25,.9,border = NA,col = rgb(.5,.5,.5,.3))
rect(36,-.1,44,.9,border = NA,col = rgb(.5,.5,.5,.3))
barplot(df$Correlation,col=cp,add=T,border=NA)
# legend("top",legend = c("MODIS vs. ECOSTRESS","MODIS vs. NLDAS","NLDAS vs. ECOSTRESS")
#        ,fill = cp,cex = 0.75,bty="n",border=NA)
# 
# text(5.8, -0.18,"Delta Fire",cex = 0.7)
# text(30, -0.18,"August Complex Fire",cex = 0.7)
# text(40, -0.18,"Creek Fire",cex = 0.7)
title('Correlation (r)',line = 0.5)

dev.off()
# stop()
png(filename = paste0("./4_results/rmse_plot.png"),width = 650, height = 480,res=100)

barplot(df$RMSE,col=NA,border=NA,ylim = c(0,20),ylab = "RMSE")

rect(0,0,25.25,20,border = NA,col = rgb(.5,.5,.5,.3))
rect(36,0,44,20,border = NA,col = rgb(.5,.5,.5,.3))
barplot(df$RMSE,col=cp,add=T,border=NA)
# legend("top",legend = c("MODIS vs. ECOSTRESS","MODIS vs. NLDAS","NLDAS vs. ECOSTRESS")
#        ,fill = cp,cex = 0.75,bty="n",border=NA)
# 
# text(5.8, -2,"August Complex Fire",cex = 0.75,xpd=T)
# text(23.98845, -2,"Delta Fire",cex = 0.75,xpd=T)
# text(40, -2,"Creek Fire",cex = 0.75,xpd=T)
title('RMSE',line = 0.5)
dev.off()

png(filename = paste0("./4_results/pct_bias_plot.png"),width = 650, height = 480,res= 100)
# 
barplot(df$percent_bias,col=NA,border=NA,ylim = c(0,1600),ylab = "% bias")

rect(0,0,25.25,1500,border = NA,col = rgb(.5,.5,.5,.3))
rect(36,0,44,1500,border = NA,col = rgb(.5,.5,.5,.3))
barplot(df$percent_bias,col=cp,add=T,border=NA)
# legend("top",legend = c("MODIS vs. ECOSTRESS","MODIS vs. NLDAS","NLDAS vs. ECOSTRESS")
#        ,fill = cp,cex = 0.75,bty="n",border=NA)
# 
# text(5.8, -150,"August Complex Fire",cex = 0.75,xpd=T)
# text(23.98845, -150,"Delta Fire",cex = 0.75,xpd=T)
# text(40, -150,"Creek Fire",cex = 0.75,xpd=T)
title('% bias',line = 0.5)
# 

dev.off()

eco_mod_mean = mean(df[df$Comparison == "ME",]$Correlation)
nld_mod_mean = mean(df[df$Comparison == "MN",]$Correlation)
nld_eco_mean = mean(df[df$Comparison == "EN",]$Correlation)

# 
