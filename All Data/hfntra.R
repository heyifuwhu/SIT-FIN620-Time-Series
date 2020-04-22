"hfntra" <- function(da,int){
# Compute number of trades in a given interval (intraday)
#
# int: time intervals in minutes
# da: data in the format: date, hour, minute, second, price, volume
#
if(!is.matrix(da))da=as.matrix(da)
intsec=int*60
istart=9*60*60+30*60
iend=16*60*60
# compute the number of time intervals within a trading day
tradetime=6.5*60*60
nintval=floor(tradetime/intsec)

T=dim(da)[1]

# compute time in seconds from midnight.
caltime=da[,2]*60*60+da[,3]*60+da[,4]

ntrade=NULL
date=da[1,1]
iday=1
#
icnt=0
cnt=rep(0,nintval)
for (i in 1:T) {
#
if(da[i,1] > date){
date=da[i,1]
ntrade=c(ntrade,cnt)
cnt=rep(0,nintval)
}
if(caltime[i]==istart)cnt[1]=cnt[1]+1
if(caltime[i]==iend)cnt[nintval]=cnt[nintval]+1
if((caltime[i] > istart) && (caltime[i] < iend)){
ii=caltime[i]-istart
ij=floor(ii/intsec)
cnt[ij+1]=cnt[ij+1]+1
}
# end of i-loop
}
ntrade=c(ntrade,cnt)

par(mfcol=c(2,1))
plot(ntrade,type='l')
title(main="Time plot of number of transactions")
acf(ntrade,lag=3*nintval)

hfntra <- list(ntrad=ntrade)
}