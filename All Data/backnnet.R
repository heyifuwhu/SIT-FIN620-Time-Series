"backnnet" <- function(x,y,nsize,orig,nlin=TRUE,nsk=TRUE,miter=10000){
# x: input (the number of columns is the number of input nodes.
# y: output variable
# orig: is the forecast origin
# nsize: the number of nodes in the hidden layer.
# nlin: linout switch
# nsk: skip layer switch
# It requires the "nnet" library
T=length(y)
if(orig > T)orig=T
nori=T-orig
err=rep(0,nori)
jlast=T-1
#
for (n in orig:jlast){
xx=x[1:n,]
yy=y[1:n]
mm=nnet(xx,yy,size=nsize,linout=nlin,skip=nsk,maxit=miter,trace=FALSE)
xp=x[n,]
fore=predict(mm,xp)
jcnt=n-orig+1
err[jcnt]=y[(n+1)]-fore
}
rmse=sqrt(sum(err^2)/(T-orig-1))
print("RMSE of out-of-sample forecasts")
print(rmse)
backnnet<-list(origin=orig,error=err,rmse=rmse)
}
