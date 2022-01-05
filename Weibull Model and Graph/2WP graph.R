#### Plotting the graph

### Set the work directory

getwd()
setwd("C:/Users/BIODUN/Desktop/SEF 723/second class")
joshua<-read_excel("Diameter.xls", sheet="All")

### Split the data into plots

tree <- split(joshua$DBH, joshua$Plot) ###Split the data into plots

### Specify the distribution d and p stand for pdf and cdf

dWeibull<-function(x, ad, bd)(ad/bd)*((x/bd)^(ad-1)*exp(-(x/bd)^ad))
pWeibull<-function(x, ad, bd)1-exp(-(x/bd)^ad)


### Plot the graph and save as pdf document
pdf("Weibull2P.pdf",width=7,height=7)
par(mfcol=c(2,2),mai=c(0.7,0.7,0.1,0.1),cex=0.7)

result<-c()

for (i in 1:5)  {
      d<- tree[[i]]
fitweibull2<-mledist(d, "Weibull", start=list(ad=2,bd=5))

hist(d, prob=T, xlab="DBH class (cm)",ylab="Relative frequency",main="")
	x<-seq(min(d),max(d),1)
	lines(x,dWeibull(x, fitweibull2$estimate[1], fitweibull2$estimate[2],lty=1, lwd=2))
	legend("topright", "Weibull 2p",lty=1,lwd=2)
}

dev.off()
