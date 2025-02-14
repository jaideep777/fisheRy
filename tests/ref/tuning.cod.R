### help function
# image that uses gray scale as the default
Image<-function(...){image(...,col=gray((32:0)/32)); box()}

# cod data from the assessment
root<-"O:/"
setwd(root)
e1<-read.csv(paste(root,"fish/NEAcod/","environmental.csv",sep=""),header=T)
o1<-read.csv(paste(root,"fish/NEAcod/","ogive.wg14.csv",sep=""),header=T)
F1<-read.csv(paste(root,"fish/NEAcod/","FatAge.csv",sep=""),header=T) # 1946-2013
M2<-read.csv(paste(root,"fish/NEAcod/","M2.csv",sep=""),header=T)
r1<-read.csv(paste(root,"fish/NEAcod/","recr1.csv",sep=""),header=T)

### parameters
# implementation parameters and age ranges
ages<-30		# maximum age
years<-2000		# simulation years
skip<-1000		# transient years to be skipped
N.ini<-1e8		# initial numbers at age 1

# biological parameters
Linf<-196		# VB: asymptotic length
a0<- -0.937		# VB: age at length zero
k<-0.0555		# VB: steepness
theta<-8.10e-6	# weight-length: condition
g<-3.01			# weight-length: allometric exponent

# fishing parameters
b<-0.75			# concentration profile parameter
q<-1e-06		# scaling parameter relating to catchability and density

# employment parameters
d.sea<-0.054		# FTE/vessel day
d.max<-30000		# maximum available offshore FTE
d.shr<-0.000004		# FTE/kg

# economic parameters
price.sea<-13.13	# landing price NOK/kg
price.shr<-17.0		# selling price NOK/kg

s.sea<-1078000			# employment cost sea NOK/FTE
s.shr<-348000			# employment cost shore NOK/FTE
fixedC.sea<-351123000	# fixed costs sea NOK (=average per unit * #units)
fixedC.shr<-103246800	# fixed costs shore NOK
varC.sea<-65000 		# variable costs NOK/vessel day
scale.catch<-0.53 		# percentage of total codfish catch that is cod

### initialisation
# initialisation of age-specific vectors
L<-Linf*(1-exp(-k*(1:ages-a0)))		# length-at-age
W<-theta*L^g						# weight-at-age

# extract some age-specific parameters from assessment input and results

# stock-recruitment
# year range to be used for recruitment - need more data here because of noise
SSB.years<-1984:2013

r1$recr<-c(r1$a1[2:nrow(r1)],NA)*1000 # recruitment at age 1 (1000s in the file, from AFWG 2014 Table 3.16)
r1$SSB<-r1$SSB*1000 # SSB in kg (in tonnes in the file, from AFWG 2014 Table 3.24a)
plot(r1$SSB,r1$recr)
points(r1$SSB[r1$year %in% SSB.years],r1$recr[r1$year %in% SSB.years],pch=16)
m0<-lm(recr~SSB-1,r1[r1$SSB<median(r1$SSB),])
summary(m1<-nls(recr~s0*SSB/(1+SSB/Bhalf),start=list(s0=coef(m0),Bhalf=mean(r1$recr[r1$SSB>median(r1$SSB)],na.rm=T)/coef(m0)),data=r1[r1$year %in% SSB.years,]))
x<-seq(min(r1$SSB),max(r1$SSB),length=100)
lines(x,coef(m1)[1]*x/(1+x/coef(m1)[2]))
abline(a=0,b=coef(m1)[1],lty="dotted")

r0<-coef(m1)[1]	# age 1 recruits per kg SSB
Bhalf<-coef(m1)[2] # first estimate of SSB where recruit survival is halved

# recruitment variability - age 1 gives very high value, use age 3 instead
# recruitment noise from the last 30 years, age 1
sf<-sf.<-sd(log(r1$recr[r1$year %in% SSB.years]),na.rm=T)
# recruitment noise from the last 30 years, age 3
sf<-sf.<-sd(log(e1$recr[e1$year %in% SSB.years]),na.rm=T)

# year range to be used for other fishing/biological parameters
SSB.years<-2004:2013

# natural mortality (including cannibalism) from AFWG2014
m<-M2[M2$Year %in% SSB.years,]
m<-0.2+c(apply(m[,2:ncol(m)],2,mean),0)
M<-c(m,rep(m[length(m)],times=ages-length(m))) 	# natural mortality at age
names(M)<-1:ages

# maturity-at-age from AFWG2014
o1<-o1[o1$YEAR %in% SSB.years,]
mat<-c(0,0,apply(o1[,2:ncol(o1)],2,mean))
mat<-c(mat,rep(1,times=ages-length(mat))) # maturity-at-age
names(mat)<-1:ages

# selectivity profile from AFWG2014
F1<-F1[1:(nrow(F1)-1),1:(ncol(F1)-1)]
Ft<-reshape(F1,timevar="age",varying=list(names(F1)[2:ncol(F1)]),direction="long")
Ft<-Ft[Ft$Year_Age  %in% SSB.years,]
F.at.age<-aggregate(Ft$X3,list(age=Ft$age),mean)
sel.fact<-c(0,0,F.at.age$x/max(F.at.age$x))
sel.fact<-c(sel.fact,rep(1,times=ages-length(sel.fact)))

# estimate LF50 from AF50
(m1<-nls(sel.fact~1/(1+exp(((1:ages)-FA50)/a)),start=list(FA50=5,a=-1)))
LF50<-Linf*(1-exp(-k*(coef(m1)[1]-a0))) # 61.72841

# estimate LF50 and SF from length-at-age and sel.fact
plot(L,sel.fact,type="o")
(m2<-nls(sel.fact~1/(1+exp(-SF*(L-LF50))),start=list(LF50=LF50,SF=.1)))
LF50<-coef(m2)[1] # 61.4806
SF<-coef(m2)[2] # 0.1222
x<-0:200
lines(x,1/(1+exp(-SF*(x-LF50))))

# mean asymptotic F
F.max<-max(F.at.age$x) # 0.56934

# for reference only - F5-10
mean(F.at.age$x[F.at.age$age %in% 5:10]) # 0.5038633

# initialisation of other data structures
N<-matrix(0,nrow=years+1,ncol=ages)	# numbers at age
N[1,1]<-N.ini
for(i in 2:ages) N[1,i]<-N.ini*exp(-sum(M[1:(i-1)])) 	# numbers at age, year 1
Res<-data.frame(year=1:years,N=NA,SSB=NA,Y=NA,R=NA,Eff=NA,Ereal=NA,Fratio=NA,
	D.sea=NA,D.shr=NA,D.tot=NA,P.sea=NA,P.shr=NA,P.tot=NA) # results

### simplified simulation function for estimating Bhalf
simulate.SSB<-function(Bhalf,display=TRUE){
	F<-F.max*sel.fact		# fishing mortality at age
	Z<-M+F								# total mortality
	s<-exp(-Z)		
	for(year in 1:years){
		Y<-sum(F/Z*(1-s)*N[year,]*W)
		SSB<-sum(N[year,]*W*mat)
		N[year+1,1]<-r0*SSB/(1+SSB/Bhalf)*exp(rnorm(n=1,mean=-sf^2/2,sd=sf))
		N[year+1,2:ages]<-N[year,1:(ages-1)]*s[1:(ages-1)]
		Res[year,1:5]<-c(year,sum(N[year,]),SSB,Y,N[year+1,1])
	}
	if(display==TRUE){
		op<-par(mfrow=c(2,2),pty="s",mar=c(4,4,.5,.5))
		plot(Res$R,type="l",xlab="",ylab="Recruitment")
		plot(Res$SSB,type="l",xlab="",ylab="SSB (kg)")
		plot(Res$Y,type="l",xlab="",ylab="Total catch weight (kg)")
		Image(x=1:years,y=1:ages,z=log(N[1:years,]),xlab="",ylab=""); legend("top","Age structure",bty="n") # non-normalized
		par(op)
	}
	Res
}

# estimate Bhalf that gives the best fit with SSB
sf<-0 # optimisation without recruitment noise
SSB.ref<-mean(e1$ssb[e1$year %in% SSB.years])/1e6
simul.SSB<-function(parms){
	res<-simulate.SSB(parms,display=FALSE)[(skip+1):years,]
	(mean(res$SSB/1e9)-SSB.ref)^2
}
Bhalf<-optimize(f=simul.SSB,interval=c(1e8,1e12))$minimum

# check the results
sf<-sf.
res<-simulate.SSB(Bhalf)

### biological calibration done!


# find out carrying capacity (Z=M) - needed for effort calculations
N.<-matrix(0,nrow=1051,ncol=ages)	# this calculation should not be influenced by the length of the main simulation
N.[1,]<-N[1,]
s<-exp(-M)		# survival
for(year in 1:(nrow(N.)-1)){				
	N.[year+1,2:ages]<-N.[year,1:(ages-1)]*s[1:(ages-1)]
	SSB<-sum(N.[year,]*W*mat)
	N.[year+1,1]<-r0*SSB/(1+SSB/Bhalf)
}
K<-apply(N.[51:1050,],2,mean) # skip first 50 years as the transient
rm(N.)

# function needed to find fishing mortality corresponding to a certain effort
simul<-function(F.max,parms){ # parms=N.rel,E.real
	E<-parms[1]^(1-b)*F.max*(exp(-(F.max+M[ages])*(1-b))-1)/(q*(F.max+M[ages])*(b-1))
	(E-parms[2])^2
}

d1<-read.csv("./fish/Dorothy/Dolly SH paper/effort and catch.csv")
d1<-merge(d1,e1[,c("year","catch")],by.x="Year",by.y="year")
d1$effort<-d1$catch/d1$Cod.Catch.tonn*d1$Total.Effort # hypothetical total effort in vessel days

### simplified simulation function for estimating q
simulate.E<-function(q,display=FALSE){
	age.fish<-min((1:ages)[1/(1+exp(-SF*(L-LF50)))>0.5])
	F2E.factor<-ifelse(b==1,F.max/q,F.max*(exp(-(F.max+M[ages])*(1-b))-1)/(q*(F.max+M[ages])*(b-1))) # Liu & Heino 2014
	for(year in 1:years){ # 
		# determine the effective fishing mortality
		N.rel<-mean(N[year,age.fish:ages]/K[age.fish:ages])
		E<-N.rel^(1-b)*F2E.factor			# effort corresponding to F (F2E.factor contains only terms that are year-independent)
		D.sea.demand<-d.sea*E				# corresponding employment demand
		D.eff<-d.max*D.sea.demand/(d.max+D.sea.demand)	# effective man hours
		E.real<-D.eff/d.sea					# realized effort (~vessel days)
		F.real<-ifelse(b==1,q*E.real,optimize(f=simul,interval=c(0,F.max),parms=c(N.rel,E.real),tol=0.0001)$minimum)
		# population dynamics
		F<-F.real*sel.fact					# fishing mortality at age
		Z<-M+F								# total mortality
		s<-exp(-Z)							# survival
		Y<-sum(F/Z*(1-s)*N[year,]*W)
		SSB<-sum(N[year,]*W*mat)
		N[year+1,1]<-r0*SSB/(1+SSB/Bhalf)*exp(rnorm(n=1,mean=-sf^2/2,sd=sf))
		N[year+1,2:ages]<-N[year,1:(ages-1)]*s[1:(ages-1)]
		# economic calculations
		profit.sea<-Y*price.sea - scale.catch*(D.sea.demand*s.sea + E*varC.sea + fixedC.sea)
		profit.shr<-Y*(price.shr-price.sea) - Y*d.shr*s.shr - scale.catch*fixedC.shr
		Res[year,]<-c(year,sum(N[year,]),SSB,Y,N[year+1,1],E,E.real,F.real/F.max,
			D.sea.demand,Y*d.shr,D.sea.demand+Y*d.shr,profit.sea,profit.shr,profit.sea+profit.shr)
	}
	if(display==TRUE){
		op<-par(mfrow=c(2,2),pty="s",mar=c(4,4,.5,.5))
		plot(apply(N,1,sum)/sum(K),type="l",xlab="",ylab="Total numbers (relative to K)")
		plot(Res$SSB,type="l",xlab="",ylab="SSB (kg)")
		plot(Res$Y,type="l",xlab="",ylab="Total catch weight")
		Image(x=1:years,y=1:ages,z=log(N[1:years,]),xlab="",ylab=""); legend("top","Age structure",bty="n") # non-normalized
		par(op)
	}
	Res
}

# estimate q that gives the best fit with E
sf<-0 # optimisation without recruitment noise
E.ref<-mean(d1$effort)
simul.E<-function(parms){
	res<-simulate.E(parms)
	mean(res$Eff[(skip+1):years])-E.ref
}
q<-uniroot(f=simul.E,interval=c(1e-5,1e-8),tol = 1e-12)$root

# check the behaviour
res<-simulate.E(q,display=TRUE)[(skip+1):years,]; summary(res)
sf<-sf.
res<-simulate.E(q,display=TRUE)[(skip+1):years,]; summary(res)

res<-simulate.E(q,display=TRUE)
op<-par(mfrow=c(2,2),pty="s",mar=c(4,4,.5,.5))
plot(1:years,res$Eff,ylim=range(c(res$Eff,res$Ereal)),type="l",xlab="",ylab="Effort"); lines(1:years,res$Ereal)
plot(1:years,res$Fratio,type="l",xlab="",ylab="F ratio")
plot(1:years,res$D.sea,ylim=range(c(res$D.sea,res$D.tot)),type="l",xlab="",ylab="Employment",col="grey"); lines(1:years,res$D.tot)
plot(1:years,res$P.sea,ylim=range(c(res$P.sea,res$P.tot)),type="l",xlab="",ylab="Profit",col="grey"); lines(1:years,res$P.tot)
par(op)
