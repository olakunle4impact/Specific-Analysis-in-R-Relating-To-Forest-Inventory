setwd()
getwd()
setwd(dir="C:/Users/BIODUN/Desktop/Assignment")
muyiwa<-read.csv("Data2.csv")
names(joshua)
attach(joshua)
plot(Dbh,THt)

## Naslund:  H=1.3+d^2/(a+bd)^2
HDnaslund<-function(d,a,b){
			1.3+d^2/(a+b*d)^2
			}

fmnaslund<-nls(THt~HDnaslund(Dbh,a,b),
			data=joshua,
			start=list(a=1.4,b=0.15))

##Fit Indices
sqrt(mean(resid(fmnaslund)^2))

mean(abs(THt-predict(fmnaslund)))
AIC(fmnaslund)
BIC(fmnaslund)

plot(predict(fmnaslund),resid(fmnaslund))
abline(0,0)

shapiro.test(resid(fmnaslund))$p.value

## Power: H=aD^b
HDpower<-function(d,a,b){
		1.3+a*Dbh^b
		}
fmPower<-nls(THt~HDpower(Dbh,a,b),
		data=joshua,
		start=list(a=3,b=1))
## Fit Indices
sqrt(mean(resid(fmPower)^2))
mean(abs(THt-predict(fmPower)))
AIC(fmPower)
BIC(fmPower)


# wykoff: H=1.3+exp(a+b/(D+1))
HDwykoff<-function(d,a,b){
		1.3+exp(a+b/d+1)
		}
fmwykoff<-nls(THt~HDwykoff(Dbh,a,b),
		data=joshua,
		start=list(a=3.6,b=-5))

d<-seq(0,50)
plot(Dbh,THt, xlab="Diameter, cm", ylab="Height, m")
lines(d,HDnaslund(d,coef(fmnaslund)[1],coef(fmnaslund)[2]), lwd=2, lty=1)
lines(d,HDpower(d,coef(fmPower)[1],coef(fmPower)[2]), lwd=2, lty=2)
lines(d,HDwykoff(d,coef(fmwykoff)[1],coef(fmwykoff)[2],lwd=2,lty=2)
legend("bottomright", c("Naslund","Power"),lty=c(1,2), lwd=c(2,2))



