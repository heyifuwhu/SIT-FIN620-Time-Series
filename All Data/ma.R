"ma" <- function(pri,n,plot=TRUE){
# pri: price series of an asset (univariate)
# n: window size
#
if(!is.matrix(pri))pri=as.matrix(pri)
#if(is.xts(pri))pri=data.frame(pri)
nob=dim(pri)[1]
Range=max(pri[,1])-min(pri[,1])
R1=max(pri[,1])-mean(pri[,1])
loc=max(pri)-Range/6
if(R1 < Range/2)loc=min(pri)+Range/6
#
if(n < 2) n=1
if(nob > n){
psum=sum(pri[1:n,1])
ma1=rep(psum/n,n)
#
for (i in (n+1):nob){
psum=psum+pri[i,1]
psum=psum-pri[i-n,1]
ma1=c(ma1,psum/n)
}
}
if(plot){
par(mfcol=c(1,1))
plot(pri[,1],type='l')
lines(ma1,lty=2)
###loc=max(pri)-Range/6
legend(n/2,loc,c(paste("n = ",c(n))),lty=2)
title(main='Moving average plot')
}
ma <- list(ma=ma1)
}
