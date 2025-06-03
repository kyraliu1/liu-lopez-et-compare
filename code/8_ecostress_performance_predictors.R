# regression on ecostress performance


# setup -------------------------------------------------------------------


library(openxlsx)
library(MASS)
wd <- '/media/kyra/KL_REU231'

setwd(wd)
me = readRDS("./4_results/mod_eco_pre_post")
df <- read.xlsx("./4_results/pct_forested1.xlsx")

me_pre = me[,c('wtr_n','n_observations_pre','Correlation_pre', 'pnan_pre')]
names(me_pre) <- c("wtr_n",'n_observations',"cor_mod_eco",'pnan')
me_post = me[,c('wtr_n','n_observations_post','Correlation_post',
               'pnan_post')]
names(me_post) <- c("wtr_n",'n_observations',"cor_mod_eco",'pnan')

me <- rbind(me_pre,me_post)
df <- read.xlsx("./4_results/pct_forested3.xlsx")
names(df)[5] <- "n_observations"

df$watershed[1:7] <- "Slate Creek"
df$watershed[8:10] <- "Mad River"
df$watershed[11:12] <- "Chiquito Creek"

# df <- df[,c('subwatershed','pct_forested','area_sqkm','pct_severe')]
# df <- merge(df,me,by.x = 'subwatershed',by.y ="wtr_n",all.y = T )

plot(y = 100*df$pnan,
     x=df$pct_forested,
     xlab = "Percent Forested",ylab = "Average Percent Empty",pch=20)

plot(y = 100*df$pnan,
     x=df$area_sqkm,
     xlab = "Subwatershed Size (sqkm)",ylab = "Average Proportion of subwatershed with Missing Data",pch=20)

# single preds ------------------------------------------------------------


par(mfrow = c(2,3),mar = c(3.5,3,3,2))

plot(x = df$pnan,cex.lab=0.75,
     y=df$cor_mod_eco,
     ylab = "",xlab = "",pch=20)
title( ylab = "corr(MODIS, ECOSTRESS)",xlab = "Average Proportion Empty",line = 2)
title(main = "(a) ECOSTRESS Spatial\nCoverage",cex.main=1,line=0.25)

m0 <- lm(cor_mod_eco ~ pnan, data = df)
abline(m0,col='red')
summary(m0)
# text(0.39,0.35,"R2")

plot(x = df$area_sqkm,cex.lab=0.75,
     y=df$cor_mod_eco,
     ylab = "",xlab = "",pch=20)
title( ylab = "corr(MODIS, ECOSTRESS)",xlab = "Subwatershed Size (sqkm)",line = 2)
title(main = "(b) Subwatershed\nSize",cex.main=1,line=0.25)

m1 <- lm(cor_mod_eco~area_sqkm,data = df)

abline(m1,col='red')
summary(m1)

plot(x = df$pct_severe,cex.lab=0.75,
     y=df$cor_mod_eco,
     ylab = "",xlab = "",pch=20)
title(main = "(c) Proportion\nSeverely Burned",cex.main=1,line=0.25)

title(ylab = "corr(MODIS, ECOSTRESS)",xlab = "Proportion severely burned",line=2)
m2 <- lm(cor_mod_eco~pct_severe,data = df)
abline(m2,col='red')
text(x = 60,y=-0.5,"")
summary(m2)

plot(x = df$pct_forested,cex.lab=0.75,
     y=df$cor_mod_eco,
     ylab = "",xlab = "",pch=20)
title(ylab = "corr(MODIS, ECOSTRESS)",xlab = "Proportion Forested",line=2)
m3 <- lm(cor_mod_eco~pct_forested,data = df)
abline(m3,col='red')
title(main = "(d) Proportion Subwatershed\nForested",cex.main=1,line=0.25)

summary(m3)

plot(x = df$n_observations,cex.lab=0.75,
     y=df$cor_mod_eco,
     ylab = "",xlab = "",pch=20)
title(main = "(e) ECOSTRESS\nTemporal Coverage",cex.main=1,line=0.25)
title(ylab = "corr(MODIS, ECOSTRESS)",xlab = "Temporal Coverage",line=2)
m5 <- lm(cor_mod_eco~n_observations,data = df)
abline(m5,col='red')

summary(m5)

#df[c("pct_forested", "pnan", "area_sqkm", "pct_severe")] <- scale(df[c("pct_forested", "pnan", "area_sqkm", "pct_severe")])

# multiple preds ----------------------------------------------------------



m4 <- lm(cor_mod_eco~ pct_severe+pct_forested+ pnan +area_sqkm+n_observations ,data= df)

plot(df$cor_mod_eco,m4$fitted.values,xlab = '',
     ylab = "",pch = 20, xlim = c(0,1),ylim = c(0,1),
    )
title( main = "(f) Predicted vs. Actual\ncorrelation",cex.main = 1,line=0.25)
title(xlab = 'Actual Values',
      ylab = "Predicted Values",line=2)

abline(lm(m4$fitted.values~df$cor_mod_eco),col='red')

summary(m4)

m_interactions <-  lm(cor_mod_eco~ pct_severe+pct_forested+ pnan +
                        area_sqkm+n_observations + pct_forested*pct_severe + 
                        area_sqkm*pct_forested+ area_sqkm*pct_severe ,data= df)

step_model <- stepAIC(m_interactions,direction = "both")

a <- lm(cor_mod_eco~ pct_severe+pct_forested+ pnan +
          area_sqkm+n_observations + pct_forested*pct_severe + 
          area_sqkm*pct_forested, area_sqkm*pct_severe ,data= df)

summary(m_interactions)
train_control <- trainControl(
  method = "repeatedcv",
  number = 3,      # 3-fold (small n)
  repeats = 100    # Repeats to reduce variance
)

cv_model <- train(
  formula(step_model),
  data = df,
  method = "lm",
  trControl = train_control
)
print(cv_model)

summary(cv_model)
par(mfrow=c(1,1),mar=c(4,4,4,4))
pairs(df[,3:7],pch = 20,cex = 0.8)
a <- cor(df[,3:7])
round(a,3)
