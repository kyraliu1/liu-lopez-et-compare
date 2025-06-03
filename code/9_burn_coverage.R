
# setup -------------------------------------------------------------------


wd <- '/media/kyra/KL_REU231'
# wd <- "D:/"

setwd(wd)


# import ------------------------------------------------------------------


df = read.csv("./pct_burns.csv")

names(df)<- c('fire','watershed','subwatershed',"pburned", "punbu",'psevere',"pmoderate",'plow')
df[c("punbu",'psevere',"pmoderate",'plow')] <- df[c("punbu",'psevere',"pmoderate",'plow')]*100
df[c("punbu",'psevere',"pmoderate",'plow')] <- floor(df[c("punbu",'psevere',"pmoderate",'plow')])

df$subwatershed <- factor(df$subwatershed, levels = unique(df$subwatershed))

# plot --------------------------------------------------------------------


 png(filename = paste0("./4_results/burn_coverage_plot.png"),width = 650, height = 480,res=100)

barplot(
  t(as.matrix(df[, c("psevere", "pmoderate", "plow")])), 
  beside = F,col=NA,border=NA,ylim=c(0,101),
  main = "Percentage Burn Coverage by Burn Severity by Subwatershed",
  xlab = "",
  ylab = "Percent Coverage",yaxt = "n")

rect(0,0,8.5,100,border = NA,col = rgb(.5,.5,.5,.3))
rect(12,0,14.5,100,border = NA,col = rgb(.5,.5,.5,.3))
barplot(add=T,
  t(as.matrix(df[, c( "psevere", "pmoderate", "plow")])), 
  beside = F, 
  col = c("red","orange", "yellow" ),
  #names.arg = df$subwatershed,las = 2,cex.names = 0.8,
 las = 3,
  legend.text = c( "Severe", "Moderate", "Low"),
  args.legend = list(x = 6.5,y= 98, bty = "n"),border = NA,
)
 text(seq(.6, 14, by = 1.2), par("usr")[3]-1.9, 
      srt = 0, adj = 0.2, xpd = TRUE,
      labels = 1:12, cex = 0.8)
 

dev.off()
 
#axis(side = 1, at = 1:nrow(df), labels = df$subwatershed, las = 2)
df

#8.5 12 14