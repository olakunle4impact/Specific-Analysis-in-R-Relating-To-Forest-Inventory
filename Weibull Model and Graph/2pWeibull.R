library(fitdistrplus)
library(stats4)
library(goftest)
library(readxl)


### Set the work directory

getwd()
setwd("C:/Users/BIODUN/Desktop/SEF 723/SEF 723")
muyiwa<-read.csv("Assignment data.csv")
attach(joshua)
names(joshua)

### Split the data into plots

tree <- split(joshua$DBH, joshua$Plot) ###Split the data into plots

### Specify the distribution d and p stand for pdf and cdf

dWeibull<-function(x, ad, bd)(ad/bd)*((x/bd)^(ad-1)*exp(-(x/bd)^ad))
pWeibull<-function(x, ad, bd)1-exp(-(x/bd)^ad)

### The computation and fit indices
result<-c()

for (i in 1:15)	{
d<-tree[[i]]
fitweibull2<-mledist(d, "Weibull", start=list(ad=2,bd=5))

x<-seq(min(d),max(d),1)
pdf<-round(dWeibull(x,fitweibull2$estimate[1], fitweibull2$estimate[2]),5)

N<-as.integer(length(d)/0.0625)
Nha<-as.integer(N*pdf)

###Goodness of Fit
KS<-ks.test(d,"pWeibull",fitweibull2$estimate[1], fitweibull2$estimate[2])
CM<-cvm.test(d,"pWeibull",fitweibull2$estimate[1], fitweibull2$estimate[2])
AD<-ad.test(d,"pWeibull",fitweibull2$estimate[1], fitweibull2$estimate[2])

result<-rbind(result,
	data.frame(PLOT=rep(i,1),
	DBH=x,
	RF=pdf,
	Nha=Nha,
	Loglik=fitweibull2$loglik,
	statKS=KS$statistic,
	pKS=KS$p,
	statAD=AD$statistic,
	pAD=AD$p,
	statCM=CM$statistic,
	pCM=CM$p))
}
result
write.csv(result,"EXAMWeibull2p.csv")  ###Send the result to excel