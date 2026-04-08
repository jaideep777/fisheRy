### help function
# image that uses gray scale as the default
Image<-function(...){image(...,col=gray((32:0)/32)); box()}

# capelin data from the assessment
root<-"O:/"
a2<-read.csv(paste(root,"fish/Dorothy/Dolly SH paper/","capelin assessment2.csv",sep="")) # AFWG2014 Table 9.6, with 2013 data from Table 9.2 and 9.5

### parameters
# implementation parameters and age ranges
ages<-6			# maximum age - AFWG2014 Table 9.1
years<-5100		# simulation years
skip<-100		# transient years to be skipped
N.ini<-1e8		# initial numbers at age 1

# biological parameters
Linf<-24.7		# VB: asymptotic length - estimated from IMR data (autumn)
a0<- -0.6387	# accounts for autumn timing of the survey
k<-0.248		# 
theta<-0.001178	# weight-length: condition - estimated from IMR data
g<-3.515		# weight-length: allometric exponent - estimated from IMR data

# fishing parameters
LF50<-14.5		# length at 50% selectivity - from Bjarte Bogstad pers. comm. L50=14.5cm, L99=16cm 
SF<-log(99)/(16-14.5)	# selection factor  - from Bjarte Bogstad pers. comm. L50=14.5cm, L99=16cm
b<-0.25			# concentration profile parameter

# employment parameters for CAPELIN
d.sea<-0.045	# FTE/vessel day
d.max<-30000	# maximum available FTE at sea
d.shr<-8.23e-7	# FTE/kg

# economic parameters for CAPELIN
price.sea<-1.81 # landing price NOK/kg
price.shr<-9.52 # selling price NOK/kg

s.sea<-1353000			# employment cost sea NOK/FTE
s.shr<-367000			# employment cost shore NOK/FTE, , data avg(2001-2011)
fixedC.sea<-294443000	# fixed costs sea NOK (=average per unit * #units)
fixedC.shr<-12690757000	# fixed costs shore NOK (=average per unit * #units), data avg(2001-2011)
varC.sea<-61000			# variable costs NOK/vessel day
scale.catch.all<-0.128	# percentage of total pelagic catch that is capelin - all years
scale.catch.cap<-0.192	# percentage of total pelagic catch that is capelin - capelin years

### initialisation
# initialisation of age-specific vectors
L<-Linf*(1-exp(-k*(1:ages-a0)))		# length-at-age (cm)
W<-theta*L^g/1000					# weight-at-age (kg)

# maturity-at-age from IMR survey data (calculation done in SD script)
o1<-read.csv(paste(root,"ICES/WGEVO/SD/BScapelin/","ogive.csv",sep=""))
o1$age<-o1$age+1 # autumn age in the file - increase age to get start-of-year age
mat<-c(o1$x,1) # assume all age fish are mature

# monthly/seasonal natural mortality from AFWG2008 - by year and maturity status
m<-read.csv(paste(root,"fish/Dorothy/Dolly SH paper/","natural mortality.csv",sep="")) # AFWG2008 Table 9.9
m<-aggregate(m[,c("Mi","Mm")],list(age=m$age),mean)
Mi<-c(m$Mi,rep(m$Mi[length(m$Mi)],times=ages-length(m$Mi))) 	# natural mortality at age - immature - monthly
Mm<-c(m$Mm,rep(m$Mm[length(m$Mm)],times=ages-length(m$Mm))) 	# natural mortality at age - mature - for Jan-Mar
names(Mi)<-1:ages
names(Mm)<-1:ages

# stock-recruitment - using everything because so noisy
# recruitment is given at age 1.5 years
# SSB is the model-predicted SSB for April ~spawning season

r<-data.frame(year=a2$year,
ssb=a2$SSB.Apr, # SSB in in 1000 tonnes (from AFWG 2014 Table 9.6)
R=c(a2$recr[2:nrow(a2)],NA)*1e3*exp(4*Mi[1]) ) # recruitment at age 1 in millions (10^9 in the file, from AFWG 2014 Table 9.6)
r<-r[!is.na(r$ssb*r$R),]

# from Mikkelsen & Pedersen (2004) Table 1, model 1-1-1, for relatively low herring abundance
r0<-1e3*762/(71.4+2.33*100)	# age 1 recruits per kg SSB
Bhalf<-(71.4+2.33*100)*1e6 # first estimate of SSB where recruit survival is halved

# illustration of Mikkelsen & Pedersen
plot(r$ssb,r$R,pch=16)
x<-seq(min(r$ssb,na.rm=T),max(r$ssb,na.rm=T),length=100)
lines(x,r0*x/(1+x/(Bhalf/1e6)))
abline(a=0,b=r0,lty="dotted")

# try estimating
m0<-lm(R~ssb-1,r[r$ssb<median(r$ssb,na.rm=T),])
summary(m1<-nls(R~s0*ssb/(1+ssb/Bhalf),start=list(s0=coef(m0),Bhalf=mean(r$ssb[r$ssb>median(r$ssb,na.rm=T)],na.rm=T)),data=r))
lines(x,coef(m1)[1]*x/(1+x/coef(m1)[2]))
abline(a=0,b=coef(m1)[1],lty="dotted")

# not satisfactory, use slope at origin from Mikkelsen & Pedersen (2004)
s0<-1e3*762/(71.4+2.33*100)	# age 1 recruits per kg SSB
summary(m1<-nls(R~s0*ssb/(1+ssb/Bhalf),start=list(Bhalf=mean(r$ssb[r$ssb>median(r$ssb,na.rm=T)],na.rm=T)),data=r))
Bhalf<-coef(m1)[1]*1e6
lines(x,r0*x/(1+x/coef(m1)[1]),col="blue")

# lognormal errors preferable - try out
r$lnR<-log(r$R)
summary(m2<-nls(lnR~log(s0)+log(ssb)-log(1+ssb/Bhalf),start=list(Bhalf=100),data=r))
lines(x,r0*x/(1+x/coef(m2)[1]),col="blue")


# recruitment variability - age 1
sf<-sd(resid(m2)) # 1.362229

# year range to be used for fishing parameters
SSB.years<-2000:2013 # all years in the period where winter fishery has dominated

# mean F - SSB adjusted for natural mortality over 3 months
# both catch and SSB are in 1000 tonnes
# have to be solved iteratively using the Baranov catch equation over fishing season of 3 months
stock<-a2$SSB[a2$year %in% (SSB.years-1)]*exp(-3*mean(Mi[3:6]))
catch<-a2$catch[a2$year %in% SSB.years]

stock<-stock[catch>10] # exclude the years when fishery was closed
catch<-catch[catch>10]

M<-mean(Mm[3:6])
simul<-function(F.,parms){ # parms=stock,catch
	stock<-parms[,1]; catch<-parms[,2]
	Y<-F./(F.+M)*(1-exp(-F.-M))*stock
	sum((Y-catch)^2)
}
F.max<-optimize(f=simul,interval=c(0,3),parms=data.frame(stock,catch))$minimum # 0.2998017

# initialisation of other data structures
N<-matrix(0,nrow=years+1,ncol=ages)	# numbers at age
N[1,1]<-N.ini
for(i in 2:ages) N[1,i]<-N.ini*exp(-sum(12*Mi[1:(i-1)])) 	# numbers at age, year 1
Res<-data.frame(year=1:years,N=NA,SSB=NA,Y=NA,R=NA,Eff=NA,Ereal=NA,Fratio=NA,
	D.sea=NA,D.shr=NA,D.tot=NA,P.sea=NA,P.shr=NA,P.tot=NA) # results

### simplified simulation function for estimating Bhalf
simulate.SSB<-function(Bhalf,display=TRUE){
	F<-F.max/(1+exp(-SF*(L-LF50)))		# fishing mortality at age
	for(year in 1:years){
		if(sum(N[year,]*W*mat*exp(-Mm))>200e6){	# fishing is possible
			if(sum(N[year,]*W*mat*exp(-Mm-F))>200e6) F.<-F		# normal F
			else{												# reduced F when full F would bring SSB<200kt
				F.max.<- max(0,-mean(Mm[3:6]) -log(200e6/sum(N[year,]*W*mat)))
				F.<-F.max./(1+exp(-SF*(L-LF50)))
			}
			Y<-sum(F./(F.+Mm)*(1-exp(-F.-Mm))*N[year,]*W*mat)	
			SSB<-sum(N[year,]*W*mat*exp(-F.-Mm))
		}else{													# no fishing
			SSB<-sum(N[year,]*W*mat*exp(-Mm))
			Y<-0
		}
		N[year+1,1]<-r0*SSB/(1+SSB/Bhalf)*exp(rnorm(n=1,mean=-sf^2/2,sd=sf))
		N[year+1,2:ages]<-N[year,1:(ages-1)]*exp(-12*Mi[1:(ages-1)])*(1-mat[1:(ages-1)])
		Res[year,1:5]<-c(year,sum(N[year,]),SSB,Y,N[year+1,1])
	}
	if(display==TRUE){
		op<-par(mfrow=c(2,2),pty="s",mar=c(4,4,.5,.5))
		plot(Res$R/1e9,type="l",xlab="",ylab="Recruitment (billions)")
		plot(Res$SSB/1e6,type="l",xlab="",ylab="SSB (1000t)")
		plot(Res$Y/1e6,type="l",xlab="",ylab="Total catch weight (1000t)")
		Image(x=1:years,y=1:ages,z=log(N[1:years,]),xlab="",ylab=""); legend("top","Age structure",bty="n") # non-normalized
		par(op)
	}
	Res
}

# estimate Bhalf that gives the best fit with SSB
SSB.ref<-mean(a2$SSB[a2$year %in% SSB.years]) *exp(-3*mean(m$Mi[3:5]))
simul.SSB<-function(parms){
	res<-simulate.SSB(parms,display=FALSE)[(skip+1):years,]
	(mean(res$SSB/1e6)-SSB.ref)^2
}
(Bhalf<-optimize(f=simul.SSB,interval=c(1e6,1e10))$minimum) # mean of 10: 662109644 

Bhalf<-662*1e6

# check the results
res<-simulate.SSB(Bhalf)

### biological calibration done!


# find out carrying capacity (Z=M) - needed for effort calculations
for(year in 1:(nrow(N)-1)){				
	SSB<-sum(N[year,]*W*mat*exp(-Mm))
	N[year+1,1]<-r0*SSB/(1+SSB/Bhalf)*exp(rnorm(n=1,mean=-sf^2/2,sd=sf))
	N[year+1,2:ages]<-N[year,1:(ages-1)]*exp(-12*Mi[1:(ages-1)])*(1-mat[1:(ages-1)])
}
K<-apply(N[(skip+1):years,],2,mean) # skip first 50 years as the transient

# function needed to find fishing mortality corresponding to a certain effort
simul<-function(F.max,parms){ # parms=N.rel,E.real
	E<-parms[1]^(1-b)*F.max*(exp(-(F.max+M[ages])*(1-b))-1)/(q*(F.max+M[ages])*(b-1))
	(E-parms[2])^2
}

M<-Mm # M is at same time scale as F (3 months)

### simplified simulation function for estimating q
simulate.E<-function(q,display=FALSE){
	age.fish<-min((1:ages)[1/(1+exp(-SF*(L-LF50)))>0.5])
	F2E.factor<-ifelse(b==1,F.max/q,F.max*(exp(-(F.max+M[ages])*(1-b))-1)/(q*(F.max+M[ages])*(b-1))) # Liu & Heino 2014
	sel.fact<-1/(1+exp(-SF*(L-LF50)))
	for(year in 1:years){ # 
		if(sum(N[year,]*W*mat*exp(-M))>200e6){	# fishing is possible
			# determine the effective fishing mortality
			N.rel<-mean(N[year,age.fish:ages]/K[age.fish:ages])
			if(sum(N[year,]*W*mat*exp(-Mm-F))>200e6){
				F.max.<-F.max	# normal F
				E<-N.rel^(1-b)*F2E.factor			# effort corresponding to F (F2E.factor contains only terms that are year-independent)
			}else{				# reduced F when full F would bring SSB<200kt
				F.max.<- max(0,-mean(M[3:6]) -log(200e6/sum(N[year,3:6]*W[3:6]*mat[3:6])))
				E<-ifelse(F.max.==0,0,N.rel^(1-b)*ifelse(b==1,F.max./q,F.max.*(exp(-(F.max.+M[ages])*(1-b))-1)/(q*(F.max.+M[ages])*(b-1))))
			}
			D.sea.demand<-d.sea*E				# corresponding employment demand
			D.eff<-d.max*D.sea.demand/(d.max+D.sea.demand)	# effective man hours
			E.real<-D.eff/d.sea					# realized effort (~vessel days)
			F.real<-ifelse(F.max.==0,0,ifelse(b==1,q*E.real,optimize(f=simul,interval=c(0,F.max.),parms=c(N.rel,E.real))$minimum))

			# population dynamics
			F<-F.real*sel.fact					# fishing mortality at age
			Z<-F+M								# total mortality for mature
			Y<-sum(F/Z*(1-exp(-Z))*N[year,]*W*mat)	# mature catch
			SSB<-sum(N[year,]*W*mat*exp(-Z))
		}else{
			Y<-0; D.sea.demand<-0; E<-0; E.real<-0; F.real<-0
			SSB<-sum(N[year,]*W*mat*exp(-M))			
		}
		N[year+1,1]<-r0*SSB/(1+SSB/Bhalf)*exp(rnorm(n=1,mean=-sf^2/2,sd=sf))
		N[year+1,2:ages]<-N[year,1:(ages-1)]*exp(-12*Mi[1:(ages-1)])*(1-mat[1:(ages-1)])

		# economic calculations
		profit.sea<-Y*price.sea - scale.catch.cap*(D.sea.demand*s.sea + E*varC.sea) - scale.catch.all*fixedC.sea
		profit.shr<-Y*(price.shr-price.sea) - Y*d.shr*s.shr - scale.catch.all*fixedC.shr
		Res[year,]<-c(year,sum(N[year,]),SSB,Y,N[year+1,1],E,E.real,F.real/F.max,
			D.sea.demand,Y*d.shr,D.sea.demand+Y*d.shr,profit.sea,profit.shr,profit.sea+profit.shr)
	}
	if(display==TRUE){
		op<-par(mfrow=c(2,2),pty="s",mar=c(4,4,.5,.5))
		plot(Res$R/1e9,type="l",xlab="",ylab="Recruitment (billions)")
		plot(Res$SSB/1e6,type="l",xlab="",ylab="SSB (1000t)")
		plot(Res$Y/1e6,type="l",xlab="",ylab="Total catch weight (1000t)")
		Image(x=1:years,y=1:ages,z=log(N[1:years,]),xlab="",ylab=""); legend("top","Age structure",bty="n") # non-normalized
		par(op)
	}
	Res
}

d1<-read.csv("./fish/Dorothy/Dolly SH paper/capelin effort and catch.csv")
d1<-d1[d1$Total.Eff.Cap>500,] # only years with directed capelin fishery

# estimate q that gives the best fit with E
E.ref<-mean(d1$Total.Eff.Cap)
simul.E<-function(parms){
	res<-simulate.E(parms)
	mean(res$Eff[(skip+1):years])-E.ref
}
q<-1e-6
(q<-uniroot(f=simul.E,interval=c(1e-6,1e-8),tol = 1e-13)$root) # mean of 5: 4.652236e-05
q<-4.65e-05

# check the behaviour
res<-simulate.E(q,display=TRUE)[(skip+1):years,]; summary(res)

window<-1:(years-skip)
window<-101:200
window<-1001:1500
op<-par(mfrow=c(2,2),pty="s",mar=c(4,4,.5,.5))
plot(window,res$Eff[res$year %in% window],ylim=range(c(res$Eff,res$Ereal)),type="l",xlab="",ylab="Effort"); lines(window,res$Ereal[res$year %in% window])
plot(window,ifelse(res$Fratio[res$year %in% window]==0,NA,res$Fratio[res$year %in% window]),type="l",xlab="",ylab="F ratio"); abline(v=res$year[res$Fratio==0],col="grey")
plot(window,res$D.sea[res$year %in% window],ylim=range(c(res$D.sea,res$D.tot)),type="l",xlab="",ylab="Employment",col="grey"); lines(window,res$D.tot[res$year %in% window])
plot(window,res$P.sea[res$year %in% window],ylim=range(c(res$P.sea,res$P.tot)),type="l",xlab="",ylab="Profit",col="grey"); lines(window,res$P.tot[res$year %in% window]); abline(h=0,col="white",lty="dotted")
par(op)

op<-par(mfrow=c(2,2),pty="s",mar=c(4,4,.5,.5))
plot(window,res$R[res$year %in% window]/1e9,type="l",xlab="",ylab="Recruitment (billions)")
plot(window,res$SSB[res$year %in% window]/1e6,type="l",xlab="",ylab="SSB (1000t)")
plot(window,ifelse(res$Fratio[res$year %in% window]==0,NA,res$Fratio[res$year %in% window]),type="l",xlab="",ylab="F ratio"); abline(v=res$year[res$Fratio==0],col="grey")
plot(window,res$Y[res$year %in% window]/1e6,type="l",xlab="",ylab="Total catch weight (1000t)")
# Image(x=window,y=1:ages,z=log(N[window,]),xlab="",ylab=""); legend("top","Age structure",bty="n") # non-normalized
par(op)
