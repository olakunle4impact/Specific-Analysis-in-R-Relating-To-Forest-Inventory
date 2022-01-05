getwd()
setwd(dir="C:/Users/BIODUN/Desktop/SEF 743")

muyiwa<-read.csv("natural.csv")
attach(joshua)
names(joshua)

###split the data into Dm and Plot
natural<-split(joshua$Dm, joshua$Plot)

SV<-c()
### the loop statement(makes the result come out in batches)

for(i in 1:6){
	d<-natural[[i]]

P25<-quantile(d,0.25)
P50<-quantile(d,0.50)
P75<-quantile(d,0.75)
P95<-quantile(d,0.95)
Dmin<-min(d)
Dmax<-max(d)
mD<-mean(d)
NHA<-length(d)/0.25
GHA<-sum((pi*(d/100)^2)/4)/0.25
QM<-sqrt(mean(d^2))

SV<-rbind(SV,
	data.frame(Plot=i,
	Dmin=Dmin,
	Dmax=Dmax,
	Dmean=mD,
	Dg=QM,
	P25=P25,
	P50=P50,
	P75=P75,
	P95=P95,
	Nha=NHA,
	Gha=GHA))
}

SV

########################################################################
################ Dominant Height ###########################

joshua<- joshua[order(joshua$Plot, joshua$Hm),]

###Dominant Height

Hdom <- round(aggregate(cbind(meanHm = joshua$Hm)~joshua$Plot,FUN = function(x) mean(tail(x,6))),1)
names(Hdom) <-c("Plot", "Hd")

###Merge the data 
Stand <-merge(SV,Hdom, by="Plot")
Stand
Hdom

###write to Excel
write.csv(Stand,"Stand.csv")

attach(Stand)

