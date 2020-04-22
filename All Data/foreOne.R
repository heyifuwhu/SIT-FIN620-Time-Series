"foreOne" <- function(m1,rt,orig,xre=NULL){
# m1: is a model object
# orig: is the starting forecast origin
# rt: the time series
# xre: the independent variables
#
regor=c(m1$arma[1],m1$arma[6],m1$arma[2])
seaor=list(order=c(m1$arma[3],m1$arma[7],m1$arma[4]),period=m1$arma[5])
nT=length(rt)
if(orig > nT)orig=nT
if(length(xre) > 0){
 if(!is.matrix(xre))xre=as.matrix(xre)
 }
pred=NULL; se=NULL
for (it in orig:nT){
 x=rt[1:it]
 xr=xre[1:it,]
 mm=arima(x,order=regor,seasonal=seaor,xreg=xr)
 nxreg=NULL
 if(length(xre) > 0)nreg=xre[it,]
 p1=predict(mm,1,newxreg=nxreg)
pred=c(pred, p1$pred)
se=c(se, p1$se)
}
print(pred)
print(se)
foreOne <- list(origin=orig,pred=pred,se=se)
}