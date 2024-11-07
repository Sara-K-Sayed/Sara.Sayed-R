### the groups of functions give the same outcome
# coefficients + t-test
summary(lm(...))
summary.lm(lm(...))
summary.lm(aov(...))
# anova table + F-test
summary(aov(...))
summary.aov(aov(...))
summary.aov(lm(...))
anova(aov(...))
anova(lm(...))

### beer task (ANCOVA)
beer<-read.delim("clipboard",stringsAsFactors=T)
summary(beer)
boxplot(mass~beer,data=beer)

t.test(mass~beer,data=beer) # not significant - too high variability
anova(lm(mass~beer,data=beer)) # same p-value, and we need sums of squares (but many reviewers do not like anova for just 2 groups)

summary(lm(mass~height,data=beer)) # trivial, but useful for explaining the variability in combined model
plot(mass~height,data=beer)
abline(lm(mass~height,data=beer))

# significant partial effect of beer
anova(lm(mass~beer,data=beer)) # same p-value as in t-test, and we need sums of squares (but many reviewers do not like anova for just 2 groups)
# but p-value is actually the same, and you may need the proportion of explained variation from anova
anova(lm(mass~beer+height,data=beer)) 
anova(lm(mass~height+beer,data=beer)) # negative overlap in variation partitioning, "enhancement of R2"

# plotting
lm.beer<-lm(mass~height+beer,data=beer)
pred<-expand.grid(beer=levels(beer$beer),height=seq(from=min(beer$height),to=max(beer$height),length=100))
fit<-data.frame(predict(lm.beer,pred,interval="confidence"))

# pick manually subset for particular group and corresponding colour
plot(mass~height,data=beer,type="n")
points(mass~height,data=beer[beer$beer=="lot",],col="red")
points(mass~height,data=beer[beer$beer=="little",],col="green")
lines(pred$height[pred$beer=="lot"],fit$fit[pred$beer=="lot"],col="red")
lines(pred$height[pred$beer=="lot"],fit$lwr[pred$beer=="lot"],col="red",lty=2)
lines(pred$height[pred$beer=="lot"],fit$upr[pred$beer=="lot"],col="red",lty=2)
lines(pred$height[pred$beer=="little"],fit$fit[pred$beer=="little"],col="green")
lines(pred$height[pred$beer=="little"],fit$lwr[pred$beer=="little"],col="green",lty=2)
lines(pred$height[pred$beer=="little"],fit$upr[pred$beer=="little"],col="green",lty=2)

# automatic function
plot(mass~height,data=beer,type="n")
my.colour<-c("red","green")
for (i in 1:length(levels(beer$beer))) {
  points(mass~height,data=beer[beer$beer==levels(beer$beer)[i],],col=my.colour[i])
  lines(pred$height[pred$beer==levels(beer$beer)[i]],fit$fit[pred$beer==levels(beer$beer)[i]],col=my.colour[i])
  lines(pred$height[pred$beer==levels(beer$beer)[i]],fit$lwr[pred$beer==levels(beer$beer)[i]],col=my.colour[i],lty=2)
  lines(pred$height[pred$beer==levels(beer$beer)[i]],fit$upr[pred$beer==levels(beer$beer)[i]],col=my.colour[i],lty=2)
}
legend("topleft",legend=levels(beer$beer),col=my.colour,lty=1,title="beer")


# non-parametric rank-based tests
# library(coin)
# 
# cor.test(beer$height,beer$mass,method="spearman")
# spearman_test(mass~height,data=beer)
# independence_test(mass~height,data=beer)
# 
# wilcox.test(beer$mass[beer$beer=="lot"],beer$mass[beer$beer=="little"])
# wilcox_test(mass~beer,data=beer)
# independence_test(mass~beer,data=beer)
# 
# independence_test(mass~beer+height,data=beer)
# independence_test(mass~height|beer,data=beer)


### Seedling task (this example was used in the lecture)
# Most of the following commands just demonstrate how are the functions equivalent to each other
# and how the little modifications of functions result in a bit different outcome.
# You do not need to perform all of them, but you need to select the appropriate commands,
# depending on what kind of data you have and what do you want to test.

seedl<-read.delim("clipboard",stringsAsFactors=T)
summary(seedl)
#      plot         seedlings         management
# Min.   : 1.00   Min.   : 4.00   abandoned:10  
# 1st Qu.: 8.25   1st Qu.:10.25   grazing  :10  
# Median :15.50   Median :13.00   mowing   :10  
# Mean   :15.50   Mean   :13.43                 
# 3rd Qu.:22.75   3rd Qu.:16.00                 
# Max.   :30.00   Max.   :25.00                 
# fertilization  productivity    temperature   
# fert  :15     Min.   :350.0   Min.   :4.200  
# unfert:15     1st Qu.:451.5   1st Qu.:4.800  
#               Median :559.5   Median :6.100  
#               Mean   :547.6   Mean   :6.213  
#               3rd Qu.:649.2   3rd Qu.:7.350  
#               Max.   :789.0   Max.   :9.000  

  library(fastDummies) # dummy_cols
  library(corrplot) # corrplot, corrplot.mixed
  
  # split the table into categorical and numeric part
  seedl.f<-seedl[,sapply(seedl,class)=="factor"]
  seedl.f<-dummy_cols(seedl.f,remove_selected_columns=T,remove_first_dummy=F) # convert to dummy variables, do not keep the original variables
  seedl.n<-seedl[,sapply(seedl,class)!="factor"]
  seedl.n<-seedl.n[,3:4] # keep only numeric predictors (remove plot ID and response variable)
  
  cor.trait<-cor(cbind(seedl.n,seedl.f))
  corrplot.mixed(cor.trait,lower="number",upper="ellipse",tl.pos="lt",lower.col="black")

### two-way ANOVA
summary(aov(seedlings~management+fertilization,data=seedl)) # main effects
summary(aov(seedlings~fertilization+management,data=seedl)) # main effects
summary(aov(seedlings~fertilization*management,data=seedl)) # main effects and interaction
  anova(aov(seedlings~fertilization*management,data=seedl))  # same outcome for aov
  summary.lm(aov(seedlings~fertilization*management,data=seedl))  # coefficients (p-values are not informative, but estimates are useful for plotting)
# comment - if predictors are independent (e.g. in case of balanced design), the order of predictors in formula does not matter.

  # composed plots
  par(mfrow=c(1,3),mar=c(8,4,0.5,0.5)) #splits the space to 1 line and 3 columns, sets margins, see more parameters in help(par)
  boxplot(seedlings~fertilization,data=seedl,range=0,col=c("red","green"),las=3)
  boxplot(seedlings~management,data=seedl,range=0,col="grey",las=3)
  boxplot(seedlings~fertilization+management,data=seedl,range=0,col=c("red","green"),las=3)
  dev.off() # shut down the "plotting device" (delete the plots and plotting settings, e.g. the margins and splitting of screen)


# plotting of means and CI from ANOVA:
aov.b<-aov(seedlings~fertilization*management,data=seedl)
# using predict()
pred<-expand.grid(fertilization=levels(seedl$fertilization),management=levels(seedl$management))
fit<-predict(aov.b,newdata=pred,interval="confidence")
means<-fit[,1] 
lower<-fit[,2]
upper<-fit[,3]
# fit is matrix, columns can be selected only by indices in [], not $ (or use as.data.frame(fit), then you can use $)
# you can skip these three lines and use directly plot(fit[,1],...) etc.

# plotting
# tiff("F:/export.tif",height=10,width=10,units="cm",res=600,compression="lzw") #starts making an image file
  png("F:/export.png",height=10,width=10,units="cm",res=600) #starts making an image file
  par(mar=c(4,4,0,0)) # adjust margins (bottom, left, top, right; units are lines of text)
plot(means,ylim=c(min(lower),max(upper)),pch=21,bg=c("red","green"),xaxt="n",xlab="",xlim=c(0.5,6.5),ylab="number of seedlings")
arrows(x0=1:6,y0=lower, y1=upper, angle=90, code=3, length=0.05, col=c("red","green"))
axis(side=1,at=1:6,labels=rep(levels(seedl$fertilization),3))
mtext(side=1,at=c(1.5,3.5,5.5),text=levels(seedl$management),line=3)
  dev.off() # shuts down the current graphic device (now the tiff file)
# check functions setwd() and load() if you want to keep working on a certain project

  # package "effects" can plot the results directly, or just return the numeric results which you can plot using the functions above.
  library(effects)    
  plot(allEffects(aov.b))
  plot(allEffects(aov.b),lines=list(multiline=T))
  means<-effect("fertilization:management",aov.b)$fit
  lower<-effect("fertilization:management",aov.b)$lower
  upper<-effect("fertilization:management",aov.b)$upper


### Regression
  # simple regression
  plot(seedlings~productivity,data=seedl)
  lm.prod<-lm(seedlings~productivity,data=seedl)
  abline(lm.prod)
  summary(lm.prod)
  anova(lm.prod)

# multiple regression
summary(lm(seedlings~productivity+temperature,data=seedl))
anova(lm(seedlings~productivity+temperature,data=seedl))
anova(lm(seedlings~temperature+productivity,data=seedl))
  # the order of predictors matters as there is always at least some correlation between predictors, see the different sum of squares
  cor(seedl$productivity,seedl$temperature)

# variance decomposition
sstot<-42.80+114.46+662.10 # total SS
(42.80+114.46)/sstot # whole model
154.08/sstot # productivity main
42.80/sstot # temperature main
114.46/sstot # productivity partial
3.18/sstot # temperature partial
(154.08-114.46)/sstot # overlap

  # https://www.geeksforgeeks.org/how-to-create-a-venn-diagram-in-r/
  library(VennDiagram)
  dev.off()
  draw.pairwise.venn(area1=round(154.08/sstot,3),
                     area2=round(42.80/sstot,3),
                     cross.area=round((154.08-114.46)/sstot,3),
                     category=c("productivity","temperature"),
                     fill=c("green","red"),
                     cat.dist=-0.05,cat.fontfamily="sans",fontfamily="sans")

## contour plot
# prepare x and y axis and predict the model
lm2<-lm(seedlings~temperature+productivity,data=seedl)
pred.x<-seq(from=min(seedl$temperature),to=max(seedl$temperature),length=200)
pred.y<-seq(from=min(seedl$productivity),to=max(seedl$productivity),length=200)
fit<-matrix(predict(lm2,expand.grid(temperature=pred.x,productivity=pred.y)),nrow=200)

plot(productivity~temperature,data=seedl)
contour(pred.x,pred.y,fit,add=T,method="edge")

  filled.contour(pred.x,pred.y,fit) # looks nice, but uses strange coordinate system...
  points(productivity~temperature,data=seedl) #... so it cannot be combined with other functions

  image(pred.x,pred.y,fit) # raster image
  contour(pred.x,pred.y,fit,add=T,labcex=1) # the contours do not match the coloured areas

image(pred.x,pred.y,fit,breaks=8:18,col = hcl.colors(10, "YlOrRd", rev = TRUE)) # adjust breaks and number of colours to match contours
contour(pred.x,pred.y,fit,add=T,labcex=1)
points(productivity~temperature,data=seedl,pch=16)


# plotting 3d
library(scatterplot3d)
fitm <- with(seedl,lm(seedlings~productivity+temperature))
thdp<-with(seedl,scatterplot3d(productivity,temperature,seedlings,
    pch=16,cex.symbols=1.2,highlight.3d=T,angle=25,type="h",lty.hplot=2))
thdp$plane3d(fitm,col="darkgrey",lty=1)

  #or
  library(rms)
  library(lattice)
  lininterp<-ols(seedlings~productivity*temperature,data=seedl)
  bplot(Predict(lininterp, productivity=3:8*100, temperature=4:9), 
        lfun=wireframe,  # bplot passes extra arguments to wireframe
        screen = list(z = -40, x = -80), drape=TRUE)

### General linear models
#productivity and fertilization
anova(lm(seedlings~productivity+fertilization,data=seedl)) #first line is the main effect of the predictor, second line is partial effect (in addition to the previous predictor)
anova(lm(seedlings~fertilization+productivity,data=seedl))
summary(lm(seedlings~productivity+fertilization,data=seedl))
  t.test(productivity~fertilization,data=seedl) # predictors are related to each other, therefore the partial effects are not significant
# cor.test(seedl$productivity,as.numeric(seedl$fertilization)) # note that if fertilization is converted to binary numeric variable, the correlation test results in exactly same p-value as t-test

#productivity and management
anova(lm(seedlings~productivity+management,data=seedl))
anova(lm(seedlings~management+productivity,data=seedl)) # negative overlap of explained variability
# http://dept.stat.lsa.umich.edu/~kshedden/Courses/Regression_Notes/decomposing-variance.pdf # see slides 29-34 (esp. 29 and 33). A shortened citation from there:
# Suppose the data generating model is y = z + E, but we don't observe z. Instead, we observe a value x1 that satisfies x1 = z + x2, where x2 has mean 0 and is independent of z.
# Since x2 is independent of z, it is also independent of y, thus R22 is approximately 0 for large n. Since R22 = 0, it follows that R2 > R21 + R22 
# The reason for this is that while x2 contains no directly useful information about y, it can remove the "measurement error" in x1, making x1 a better predictor of z.

plot(seedlings~productivity,data=seedl,type="n")
points(seedlings~productivity,data=seedl[seedl$management=="abandoned",],col="red")
points(seedlings~productivity,data=seedl[seedl$management=="grazing",],col="blue")
points(seedlings~productivity,data=seedl[seedl$management=="mowing",],col="green")
# lines based on model without interaction (i.e. parallel)
summary(lm(seedlings~productivity+management,data=seedl))
# estimate of intercept is the intercept of the line for first factor level
# estimate of productivity is the slope for all factor levels
# other estimates are the differences ("contrasts") of intercept for the other factor levels from the first level
abline(22.256,-0.0234,col="red") # abandoned treatment was aphabetically chosen as a reference (intercept)
abline(22.256+9.43,-0.0234,col="blue") # modify the intercept of "abandoned" by the contrast of "grazing"
abline(22.256+2.557,-0.0234,col="green")
# or you can use predict() function (as described in the lecture for forward-selection example)
# abline(lm(seedlings~productivity,data=seedl)) # the slope of the productivity only is lower


### forward selection
anova(lm(seedlings~fertilization*management*productivity*temperature,data=seedl))
# you do not want such a complex model

  lm.0<-lm(seedlings~1, data=seedl) # 1 means "intercept" in the formula
  add1(lm.0, .~.+management*fertilization*productivity*temperature, test="F")
  lm.1<-update(lm.0, .~.+management)
  add1(lm.1, .~.+management*fertilization*productivity*temperature, test="F")
  lm.2<-update(lm.1, .~.+productivity)
  add1(lm.2, .~.+management*fertilization*productivity*temperature, test="F")
  # resulting model
  # seedlings ~ management + productivity
  
  anova(lm.0,lm.1,lm.2,test="F")
  anova(lm.0,lm.2,test="F")
  anova(lm.2)
# the same using automated approach:
lm.step<-step(lm.0, .~.+management*fertilization*productivity*temperature, test="F")
anova(lm.step)


#### LOESS smoothing (useful for plotting (but not testing) non-linear relationships)
plot(seedlings~productivity,data=seedl)
abline(lm(seedlings~productivity,data=seedl))
#simple function, you can directly plot the line
lines(loess.smooth(seedl$productivity,seedl$seedlings,span=0.6,degree=2),lty=2,col="red")
lines(loess.smooth(seedl$productivity,seedl$seedlings,span=0.5,degree=1),lty=2,col="green")
#advanced function, you must pre-compute the line yourself (using predict function), but you can get also SE (and 2*SE is confidence interval)
lo.pred<-predict(loess(seedlings~productivity,data=seedl,span=0.65,degree=2),data.frame(productivity=300:800),se=T)
lines(300:800,lo.pred$fit,lty=2,lwd=2,col="blue")
lines(300:800,lo.pred$fit+2*lo.pred$se.fit,lty=2,lwd=1,col="blue")
lines(300:800,lo.pred$fit-2*lo.pred$se.fit,lty=2,lwd=1,col="blue")
#parameters span and degree set how much curved the curve is
#span (0 to1 ): from how big part of data is the model calculated (the smaller part, the more curved)
#degree (1 or 2): if the model is line (1) or parabola (2, more curved but more smooth)

## contour plot with 3d loess model
pred.x<-seq(from=min(seedl$temperature),to=max(seedl$temperature),length=200)
pred.y<-seq(from=min(seedl$productivity),to=max(seedl$productivity),length=200)
lo2<-loess(seedlings~productivity+temperature,data=seedl,span=0.75,degree=1)
fit<-matrix(predict(lo2,expand.grid(temperature=pred.x,productivity=pred.y)),nrow=200)

plot(productivity~temperature,data=seedl,pch=16)
contour(pred.x,pred.y,fit,add=T,method="edge")

image(pred.x,pred.y,fit,
      breaks=8:18,col=hcl.colors(10,"YlOrRd",rev=TRUE),
      xlab="temperature",ylab="productivity") # adjust breaks and number of colours to match contours
contour(pred.x,pred.y,fit,add=T,labcex=1,levels=8:18)
points(productivity~temperature,data=seedl,pch=16)

