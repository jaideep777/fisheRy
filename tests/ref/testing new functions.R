root<-"O:/"
setwd(paste(root,"fish/Dorothy/Dolly SH paper/",sep=""))
source("functions-2018-10-29.R")

# load data and keep only the relevant columns
load("CAP_it_ms_hl_yr_statusquo.Rdata",v=TRUE)
a<-data.frame(it=x$j,ms=x$m,hl=x$h,yr=x$year,s=x$SSB,y=x$Y,e=x$D.tot,p=x$P.tot)

load("COD_it_ms_hl_yr_statusquo.Rdata",v=TRUE)
a<-x[,c("it","ms","hl","yr","s","y","e","p")]

# downsize for speed
# a<-a[a$it<=3,]



x<-unique(a$hl)
y<-unique(a$ms)

u.std<-normalize_max(a)

# remove cases with negative longterm profit - within each replicate
u.std$id<-paste(u.std$it,u.std$hl,u.std$ms) # unique "case" identity
mean.p<-aggregate(u.std$p, list(id=u.std$id),mean,na.rm=T)
u.std<-merge(u.std,mean.p,by="id")
u.std<-u.std[u.std$x>0,!names(u.std) %in% c("id","x")]

# remove cases with negative longterm profit - on average, over replicates
u.std$id<-paste(u.std$hl,u.std$ms) # unique "case" identity
mean.p<-aggregate(u.std$p, list(id=u.std$id),mean,na.rm=T)
u.std<-merge(u.std,mean.p,by="id")
u.std<-u.std[u.std$x>0,!names(u.std) %in% c("id","x")]

profit<-with(u.std,tapply(p,list(hl,ms),mean))
image(profit)
# the "anomalies" appear real - low harvest prop gives higher SSB but lower catch
with(a[a$it==1 & a$ms>155 & round(a$hl,2) %in% c(0.1,0.34,0.99),],plot(yr,s,cex=hl*2))
with(a[a$it==1 & a$ms>155 & round(a$hl,2) %in% c(0.1,0.34,0.99),],plot(yr,y,cex=hl*2))
with(a[a$it==1 & a$ms>155 & round(a$hl,2) %in% c(0.1,0.34,0.99),],plot(yr,p,cex=hl*2))

with(a[a$it==1 & round(a$ms) %in% c(98,111,134,157) &a$hl==0.99,],plot(yr,s,cex=ms/100))
with(a[a$it==1 & round(a$ms) %in% c(98,111,134,157) &a$hl==0.99,],plot(yr,y,cex=ms/100))
with(a[a$it==1 & round(a$ms) %in% c(98,111,134,157) &a$hl==0.99,],plot(yr,p,cex=ms/100))
with(a[a$it==1 & round(a$ms) %in% c(98,111,134,157) &a$hl==0.99,],plot(yr,e,cex=ms/100))

with(a[a$it==1 & a$hl==0.99,],plot(tapply(p,ms,mean),type="l")); abline(h=0)

# calculations without discounting/baseline shifting
su<-SHutility.EAF(u.std,m)
ss<-SHsatisfaction.EAB.exp(su)
jss<-JSS(ss)
jss.max<-tapply(jss$SHUmin,jss$it,max)
jss[jss$SHUmin %in% jss.max,]

jss<-aggregate(jss$SHUmin,list(hl=jss$hl,ms=jss$ms),mean)

xy<-matrix(nrow=length(x), ncol=length(y), dimnames=list(x,y), data=0)
for(i in 1:nrow(jss)) xy[(1:length(x))[x==jss$hl[i]],(1:length(y))[y==jss$ms[i]]]<-jss$x[i]
image(xy,col=gray((32:0)/32)); box()

# calculations with discounting (Td) and baseline shifting (Ts and q)

# Figure 4a - average first, then max
su<-SHutility.EAF(u.std,m)
res.s<-NULL
for(Td in c(1,2.5,5,10,20,40,Inf)){
	for(Ts in c(5,10,20,40,80,160,Inf)){
		ss<-SHsatisfaction.EAB.exp(su,T50=Ts,T50.discounting=Td,q=1)
		jss<-JSS(ss)
		jss<-aggregate(jss$SHUmin,list(hl=jss$hl,ms=jss$ms),mean)
		res.s<-rbind(res.s,c(Td,Ts,max(jss$x)))
	}
}
res.s<-data.frame(res.s)
names(res.s)<-c("Td","Ts","JSS")
filled.contour(matrix(res.s$JSS,nrow=7),xlab="Time scale of baseline shifting",ylab="Time scale of discounting",plot.axes={
	axis(1,0:6/6,labels=c(5,10,20,40,80,160,Inf))
	axis(2,0:6/6,labels=c(1,2.5,5,10,20,40,Inf)) })


# Figure 4a - max first, then average
su<-SHutility.EAF(u.std,m)
res.s<-NULL
for(Td in c(1,2.5,5,10,20,40,Inf)){
	for(Ts in c(5,10,20,40,80,160,Inf)){
		ss<-SHsatisfaction.EAB.exp(su,T50=Ts,T50.discounting=Td,q=1)
		jss<-JSS(ss)
		jss.max<-tapply(jss$SHUmin,jss$it,max)
		res.s<-rbind(res.s,c(Td,Ts,mean(jss.max)))
	}
}
res.s<-data.frame(res.s)
names(res.s)<-c("Td","Ts","JSS")
filled.contour(matrix(res.s$JSS,nrow=7),xlab="Time scale of baseline shifting",ylab="Time scale of discounting",plot.axes={
	axis(1,0:6/6,labels=c(5,10,20,40,80,160,Inf))
	axis(2,0:6/6,labels=c(1,2.5,5,10,20,40,Inf)) })
