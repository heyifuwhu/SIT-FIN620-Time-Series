"Egarch" <- function(rtn,include.mean=TRUE){
# Estimation of an EGARCH(1,1) model. Assume normal innovations
# rtn: return series 
#
Egarchdata <<- rtn
# obtain initial estimates
if(include.mean){
  rmean=mean(rtn)
  par=c(mu=rmean,alpha0=-0.1,alpha1=0.1,gamma=-0.1,beta=0.8)
 }
else{
 par=c(alpha0=-0.1,alpha1=0.1,gamma=-0.1,beta=0.8)
 }

#### log Likelihood function
glk <- function(par){
rtn <- Egarchdata
ht=c(var(rtn))
nT=length(rtn)
at = rtn
ist = 0
if(length(par) > 4){
 at = at-par[1]
 ist=1
}
for (i in 2:nT){
eptm1=at[i-1]/sqrt(ht[i-1])
lnht=par[ist+1]+par[ist+2]*(abs(eptm1)+par[ist+3]*eptm1)+par[ist+4]*log(ht[i-1])
sig2t=exp(lnht)
ht=c(ht,sig2t)
}
hh=sqrt(ht)
glk = -sum(log(dnorm(x=at/hh)/hh))
}
#
#mm=optim(par,glk,method="Nelder-Mead",hessian=T)
S=0.000001
if(include.mean){
 low=c(mu=-10*abs(rmean),alpha0=-6,alpha1=S, gamma = S-1, beta=0.5)
 upp=c(mu=10*abs(rmean),alpha0=5,alpha1=1-S,gamma=-S,beta=1-S)
 }
 else{
 low=c(alpha0=-6,alpha1=S, gamma = S-1, beta=0.5)
 upp=c(alpha0=5,alpha1=1-S,gamma=-S,beta=1-S)
 }
##mm=optim(par,glk,method="L-BFGS-B",hessian=T,lower=low,upper=upp)
fit=nlminb(start=par,objective=glk,lower=low,upper=upp) ##,control=list(trace=3,rel.tol=1e-5))
epsilon = 0.0001 * fit$par
npar=length(par)
Hessian = matrix(0, ncol = npar, nrow = npar)
for (i in 1:npar) {
for (j in 1:npar) {
x1 = x2 = x3 = x4  = fit$par
x1[i] = x1[i] + epsilon[i]; x1[j] = x1[j] + epsilon[j]
x2[i] = x2[i] + epsilon[i]; x2[j] = x2[j] - epsilon[j]
x3[i] = x3[i] - epsilon[i]; x3[j] = x3[j] + epsilon[j]
x4[i] = x4[i] - epsilon[i]; x4[j] = x4[j] - epsilon[j]
Hessian[i, j] = (glk(x1)-glk(x2)-glk(x3)+glk(x4))/
(4*epsilon[i]*epsilon[j])
}
}
cat("Maximized log-likehood: ",-glk(fit$par),"\n")
# Step 6: Create and Print Summary Report:
se.coef = sqrt(diag(solve(Hessian)))
tval = fit$par/se.coef
matcoef = cbind(fit$par, se.coef, tval, 2*(1-pnorm(abs(tval))))
dimnames(matcoef) = list(names(tval), c(" Estimate",
" Std. Error", " t value", "Pr(>|t|)"))
cat("\nCoefficient(s):\n")
printCoefmat(matcoef, digits = 6, signif.stars = TRUE)

# compute the volatility series and residuals
par=fit$par
ht=var(rtn)
nT=length(rtn)
ist=0
at=rtn
if(include.mean){
  at=rtn-par[1]
  ist=1
}
for (i in 2:nT){
eptm1=at[i-1]/sqrt(ht[i-1])
lnht=par[ist+1]+par[ist+2]*(abs(eptm1)+par[ist+3]*eptm1)+par[ist+4]*log(ht[i-1])
sig2t=exp(lnht)
ht=c(ht,sig2t)
}
sigma.t=sqrt(ht)
Egarch <- list(residuals=at,volatility=sigma.t)
}

