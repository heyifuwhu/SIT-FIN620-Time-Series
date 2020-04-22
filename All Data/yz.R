"yz" <- function(op,hi,lo,cl,window=63,nsplit=0,split=0){
# Compute the volatility based on Yang and Zhang (2000) method.
n=length(op)
nm1=n-1
ot=log(op[2:n])-log(cl[1:nm1])
ut=log(hi[2:n])-log(op[2:n])
dt=log(lo[2:n])-log(op[2:n])
ct=log(cl[2:n])-log(op[2:n])
if (nsplit > 0){
 if(nsplit==1){
  split=matrix(split,1,2)
}
 for (i in 1:nsplit){
 idx=split[i,1]
 ratio=split[i,2]
 ot[idx]=log(op[idx+1])-log(cl[idx]*ratio)
}
}
syz2=rep(0,nm1)
k=0.34/(1.34+(window+1)/(window-1))
for (i in window:nm1){
ist=i-window+1
s02=var(ot[ist:i])
sc2=var(ct[ist:i])
srs2=mean(ut[ist:i]*(ut[ist:i]-ct[ist:i])+dt[ist:i]*(dt[ist:i]-ct[ist:i]))
syz2[i]=s02+k*sc2+(1-k)*srs2
}
yz <-list(yzsq = syz2)
}