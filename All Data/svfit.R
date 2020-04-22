"svfit" <- function(rtn,burnins,iter,pphim=NULL,pphiv=NULL,prilam=NULL,xt=NULL){
## fits univariate stochastic volatility models without the leverage effect.
##
### Model used: r(t) = x(t)'beta+exp(s_{t-1}/2)e(t).
#### s_t = phi_0 + phi_1*s(t-1)+ w_t
#### y_t = s_{t-1} + log(e_t^2), where y_t = log[(r_t-x_t'beta)^2]
#### log(e_t^2) is approximated by mixture of 7 normals. That is, y_t = s_{t-1} + v(t).
#### Since v(t) is a component of the mixture, it may have non-zero mean. 
#### Therefore, the observation equation is y_t = c(t) + s(t-1)+ v(t).
####
##### Currently, x_t = 1 to represent the mean of the returns.
##### variance = exp(s_t) so that volatility = sqrt(exp(s_t)).
#####
###library(mnormt); library(fGarch)
require(mvtnorm); require(fGarch)
if(is.matrix(rtn))rtn=rtn[,1]
nT=length(rtn)
if(burnins < 1)burnins=100
if(iter < 1)iter = 1
if(length(pphim) < 1)pphim=rep(0,2)
if(length(pphiv) < 1)pphiv=rep(1000,2)
if(length(prilam) < 1)prilam=c(10,0.1)
### Initial conditional variance estimates via a GARCH(1,1) model.
m1=garchFit(~garch(1,1),data=rtn,trace=FALSE)
##
st=2*log(m1@sigma.t)
m1a=arima(st,order=c(1,0,0))
ini=c(mean(st),var(st))
phi0=m1a$coef[2]
phi=m1a$coef[1]
varw=m1a$sigma2
#
yt=rtn-mean(rtn)
#### 0.00001 is added to avoid taking log of zero.
yt=log(yt^2+0.000001)
## Obtain the variance of v(t) for each t. Using the mixture of 7 normals.
m2=approx(yt,st)
ct=m2$mean; varv=m2$var
### initialization
sst=NULL
phiHat=NULL
VarW=NULL
for (it in 1:(burnins+iter)){
m3=ffbs.sv(yt,ct,phi0,phi,varw,varv,ini)
#par=c(phi0,phi,varw)
#cat("par",par,"\n")
st=m3$st
ph=drphi.sv(st,varw,pphim,pphiv)
phi0=ph[1]
phi=ph[2]
varw=drsig.sv(st,phi0,phi,prilam)
m4=approx(yt,st)
ct=m4$mean; varv=m4$var
if(it > burnins){
sst=cbind(sst,st)
phiHat=rbind(phiHat,ph)
VarW=c(VarW,varw)
}
#
}
### Compute summary statistics
### take exponential of each draw to obtain variance, then taking square-root
### to obtain volatility of each draw. Finally, compute the posterior mean and 
### variance of the volatility.
sst = exp(sst)
sst = sqrt(sst)
sv=apply(sst,1,mean)
svVar=apply(sst,1,var)
state=cbind(c(sv),c(svVar))
vol=c(sv)
### quantiles
QQ=NULL
for (i in 1:nrow(sst)){
qq=quantile(sst[i,],c(0.05,0.5,0.95))
QQ=rbind(QQ,qq)
}
##est=apply(phiHat,2,mean)
## end of the program
### state: gives the posterior mean and variance of the draws of states.
###
svfit <- list(volatility= vol, phiHat=phiHat, varw=VarW, States=state,quantiles=QQ,draws=sst)
}

###
"approx" <- function(ystar,vt){
### 7-component normal approximation
## ystar: obervation
## vt: volatility
## ct: explanatory variables, if any.
##
##
pb=c(.0073,0.10556,0.00002,0.04395,0.34001,0.24566,0.2575)
apxm=c(-11.40039,-5.24321,-9.83726,1.50746,-0.65098,0.52478,-2.35859)
apxv=c(5.79596,2.61369,5.1795,0.16735,0.64009,0.34023,1.26261)
apxsd=sqrt(apxv)
nT=length(ystar)
## intialization
ct=rep(0,nT); v=rep(1,nT)
pden=NULL
for (j in 1:7){
xm=vt+apxm[j]
d1=dnorm(ystar,mean=xm,sd=rep(apxsd[j],nT))
pden=cbind(pden,d1)
}
post=NULL
for (j in 1:7){
p1=pb[j]*pden[,j]
post=cbind(post,p1)
}
tot=apply(post,1,sum)
idx=c(1:nT)[tot < 1.0/10^(7)]
#cat("length: ",c(length(idx)),"\n")
if(length(idx) > 0){
post[idx,]=pb
post[-idx,]=post[-idx,]/tot[-idx]
}
else{
post=post/tot
}
###
post1=t(apply(post,1,cumsum))
ru=runif(nT)
for (i in 1:nT){
kdx=1
totp=post1[i,1]
while(ru[i] > totp){
kdx=kdx+1
totp=post1[i,kdx]
}
if(kdx > 7)kdx=7
ct[i]=apxm[kdx]
v[i]=apxv[kdx]
}
###
approx <- list(mean=ct,var=v)
}


###
"ffbs.sv" <- function(y,ct,dt,phi,varw,varv,ini){
## Forward filering and backward sampling
### model:
### st(t) = dt(t) + phi*st(t-1) + w(t)  [state-equation]
### y(t) = c(t) + st(t-1) + v(t) [observation equation]
###
### varw: variance of w(t)
### varv: variance of v(t)
###
### initialization
nT=length(y)
stm=rep(0,nT); stv=rep(1,nT); stpm=rep(0,nT); stpv=rep(1,nT)
ypm=rep(0,nT); ypv=rep(1,nT); st=rep(1,nT)
#
if(phi>=1)phi=0.99999
stm[1]=ini[1]
stv[1]=ini[2]
if(length(dt)==1)dt=rep(dt,nT)
phi2=phi*phi
### forward filtering
for (i in 2:nT){
### compute the prediction of state given F(t-1).
stpm[i]=phi*stm[i-1]+dt[i]
stpv[i]=phi2*stv[i-1]+varw
### compute the prediction of y(t) given F(t-1).
ypm[i]=stpm[i]+ct[i]
ypv[i]=stpv[i]+varv[i]
### compute the inverse of Q(t)
wk=stpv[i]/ypv[i]
stm[i]=stpm[i]+wk*(y[i]-ypm[i])
stv[i]=stpv[i]-wk*wk*ypv[i]
}
##cat("ffbs",c(stm[nT],stv[nT]),"\n")
#### Backward sampling
st[nT]=rnorm(1,stm[nT],sqrt(stv[nT]))
for (i in 1:(nT-1)){
j=nT-i
wk=stv[j]*phi
tmp=1/stpv[j+1]
tm=stm[j]+wk*tmp*(st[j+1]-stpm[j+1])
v=stv[j]-wk*tmp*wk
st[j]=rnorm(1,tm,sqrt(v))
}
##
ffbs.sv <- list(st=st,stm=stm,stv=stv,stpm=stpm,stpv=stpv,ypm=ypm,ypv=ypv)
}

####
"drphi.sv" <- function(st,varw,prim,priv){
### draw coefficients of an AR(1) model, including the constant
### This is for stochastic volatility model only.
###
### prim: prior mean of (phi_0,phi_1)
### priv: vector of prior variances phi_0 and phi_1
###
nT = length(st)
priV=diag(1/priv)
x=cbind(rep(1,nT-1),st[1:(nT-1)])
x=as.matrix(x)
xpx=crossprod(x,x)
y=matrix(st[2:nT],(nT-1),1)
xpy=crossprod(x,y)
xpy[1]=xpy[1]/varw+prim[1]*priV[1,1]
xpy[2]=xpy[2]/varw+prim[2]*priV[2,2]
xpxi = xpx/varw + priV
xpx=solve(xpxi)
postm=xpx%*%xpy
###phi=rmnorm(1,mean=postm,varcov=xpx)
phi=rmvnorm(1,mean=postm,sigma=xpx)
###
return(phi)
}

###
"drsig.sv" <- function(st,phi0,phi,lambda){
### draw sigm^2 for stochastic volatility model
nT=length(st)
resi=st[2:nT]-phi0-phi*st[1:(nT-1)]
wk=sum(resi^2)
df1=nT-1+lambda[1]
c1=rchisq(1,df1)
varw=(lambda[1]*lambda[2]+wk)/c1
return(varw)
}