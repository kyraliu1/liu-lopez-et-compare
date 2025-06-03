# pre vs post comparison plots


# setup -------------------------------------------------------------------


wd <- "/media/kyra/KL_REU231"
setwd(wd)


# import ------------------------------------------------------------------


files <-c(
  "./4_results/output/1_SlateCreek_pre_post_stats.csv",
  "./4_results/output/2_MosquitoCreek_pre_post_stats.csv",
  "./4_results/output/3_NorthSaltCreek_pre_post_stats.csv",
  "./4_results/output/4_MearsCreek_pre_post_stats.csv",
  "./4_results/output/5_ShotgunCreek_pre_post_stats.csv",
  "./4_results/output/6_DogCreek_pre_post_stats.csv",
  "./4_results/output/7_HazelCreek_pre_post_stats.csv",
  "./4_results/output/8_LostCreek_pre_post_stats.csv",
  "./4_results/output/9_RuthLake_pre_post_stats.csv",
  "./4_results/output/10_BarryCreek_pre_post_stats.csv",
  "./4_results/output/11_LowerChiquitoCreek_pre_post_stats.csv",
  "./4_results/output/12_UpperChiquitoCreek_pre_post_stats.csv"
)
d <- list()

for (i in 1:length(files)){
  d[[i]] <- read.csv(files[i])
  d[[i]]$wtr_n <- i
  d[[i]] <- subset(d[[i]],select = -c(X))
  
}

# separate pre and post ---------------------------------------------------


pre <- data.frame()

for (i in 1:length(d)){
  a <- d[[i]]
  pre <- rbind(pre, a[grepl("pre", a$Comparison),])
  
}
idx <- pre$n_observations>10 
# pre <- pre[pre$n_observations>10,]


# plot --------------------------------------------------------------------


cp = c("darkorange3","maroon","gold2")[as.factor(pre$Comparison)]

barplot(pre$Correlation,col=NA,border=NA,ylim = c(-.2,.9),ylab = "Correlation (r)")

rect(0,-.1,11,.9,border = NA,col = rgb(.5,.5,.5,.3))
rect(36,-.1,44,.9,border = NA,col = rgb(.5,.5,.5,.3))
barplot(pre$Correlation,col=cp,add=T,border=NA)
# legend("top",legend = c("MODIS vs. ECOSTRESS","MODIS vs. NLDAS","NLDAS vs. ECOSTRESS")
#        ,fill = cp,cex = 0.75,bty="n",border=NA)

text(5.8, -0.18,"August Complex Fire",cex = 0.75)
text(23.98845, -0.18,"Delta Fire",cex = 0.75)
text(40, -0.18,"Creek Fire",cex = 0.75)
title('Correlation (r)',line = 0)
# stop()
post <- data.frame()
for (i in 1:length(d)){
  # d[[i]] <- subset(d[[i]],select = -c(X))
  
  a <- d[[i]]
  post <- rbind(post, a[grepl("post", a$Comparison),])

}


cp = c("darkorange3","maroon","gold2")[as.factor(post$Comparison)]

barplot(post$Correlation,col=NA,border=NA,ylim = c(-.2,.9),ylab = "Correlation (r)")

rect(0,-.1,11,.9,border = NA,col = rgb(.5,.5,.5,.3))
rect(36,-.1,44,.9,border = NA,col = rgb(.5,.5,.5,.3))
barplot(post$Correlation,col=cp,add=T,border=NA)
# legend("top",legend = c("MODIS vs. ECOSTRESS","MODIS vs. NLDAS","NLDAS vs. ECOSTRESS")
#        ,fill = cp,cex = 0.75,bty="n",border=NA)

text(5.8, -0.18,"August Complex Fire",cex = 0.75)
text(23.98845, -0.18,"Delta Fire",cex = 0.75)
text(40, -0.18,"Creek Fire",cex = 0.75)
title('Correlation (r)',line = 0)
pre$Comparison <-gsub(" pre","",pre$Comparison)
post$Comparison <-gsub(" post","",post$Comparison)

both <-merge(pre,post,c("wtr_n",'Comparison'),suffixes =c("_pre","_post"))

# both <- both[both$wtr_n>7,]
both10 <- both[both$n_observations_pre>8,]
both0 <- both[both$n_observations_pre<=8,]
# both <- both[both$n_observations_pre>8,]
#post <- post[idx,]
pal <- c("darkorange3","maroon","gold2")[as.factor(both0$Comparison)]
cpal =c("darkorange3","maroon","gold2")[as.factor(both10$Comparison)]

png(filename = paste0("./4_results/post_v_pre.png"),width = 5, height = 4.2,unit="in",res= 600)

plot(both10$Correlation_pre,both10$Correlation_post,col='black' ,bg = cpal,pch = 21,cex=2,
     xlab = 'Pre-Fire Correlation',ylab = "Post-Fire Correlation",
     main = "Post-Fire vs. Pre-Fire Correlation",xlim = c(-1,1),ylim = c(-.5,1) )
points(both0$Correlation_pre,both0$Correlation_post, col='grey30',bg = pal,pch =24,cex=1.5)

text(both$Correlation_pre[both$Comparison !="MODIS vs. NLDAS"],
     both$Correlation_post[both$Comparison !="MODIS vs. NLDAS"], 
     labels = both$wtr_n[both$Comparison !="MODIS vs. NLDAS"],
     col = "white",
     cex = .55)

text(both$Correlation_pre[both$Comparison =="MODIS vs. NLDAS"], 
     both$Correlation_post[both$Comparison =="MODIS vs. NLDAS"], 
     labels = both$wtr_n[both$Comparison =="MODIS vs. NLDAS"],
     col = "black",
     cex = .55)
dev.off()
pal1 <- c("darkorange3","maroon","gold2")[as.factor(both$Comparison)]
both$change = (both$Correlation_post-both$Correlation_pre)
plot(both$n_observations_pre,both$change,col=pal1,pch=20,cex=1.5,
     xlab = "Pre-Fire Observations",ylab = "Change in Correlation")    
# m1 = lm(change~n_observations_pre,data = me1_7[me1_7$Comparison == "MODIS vs. ECOSTRESS",])
# m2 = lm(change~n_observations_pre,data = both[both$Comparison == "MODIS vs. NLDAS",])
# m3 = lm(change~n_observations_pre,data = both[both$Comparison == "ECOSTRESS vs. NLDAS",])
# 
# abline(m1,col='maroon')
# abline(m2,col='gold2')
# abline(m3,col='darkorange3')

#
ME = both[both$Comparison=="MODIS vs. ECOSTRESS",]
ME[ME$wtr_n %in% 8:10,]$Correlation_post
mean(ME[ME$wtr_n %in% 11:12,]$Correlation_pre)


MN = both[both$Comparison=="MODIS vs. NLDAS",]
mean(MN[MN$wtr_n %in% 11:12,]$Correlation_post)

EN = both[both$Comparison=="ECOSTRESS vs. NLDAS",]
mean(EN[EN$wtr_n %in% 11:12,]$Correlation_post)

# both17 = bo

me = ME[order(ME$wtr_n),]
saveRDS(me,"./4_results/mod_eco_pre_post")
