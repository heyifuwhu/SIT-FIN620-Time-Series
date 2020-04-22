"mvwindow" <- function(rt,size=63){
# rt: return series
# size: window size.
if(is.matrix(rt))rt=as.numeric(rt[,1])
nT=length(rt)
win=size
vol=rep(0,nT)
if (win <= nT){
for (i in win:nT){
ist=i-win+1
x=rt[ist:i]
v=var(x)
vol[i]=sqrt(v)
}
}
vol[1:(win-1)]=vol[win]
mu=mean(rt,na.rm=T)
lb=mu-2*vol
up=mu+2*vol
y1=max(up,rt,na.rm=T)
y2=min(lb,rt,na.rm=T)
par(mfcol=c(1,1))
plot(rt,xlab='time',ylab='rtn',ylim=c(y2,y1)*1.1,type='l')
lines(lb,col='red')
lines(up,col='red')
title(main='Volatility plot')

mvwindow <- list(sigma.t=vol)
}