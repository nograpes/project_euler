add.one<-function(x) c(x[1]+x[2],x[2])
add.two<-function(x) c(x[1]+(2*x[2]),x[2])
invert<-function(x) rev(x)
digits<-function(x) floor(log(x,base=10))+1

library(gmp)
n<-1000
seed<-as.bigz(c(1,2))
both<-list()
for(i in 1:n){
  both[[i]]<-add.one(seed)
  seed<-invert(add.two(seed))
}
length(which(apply(sapply(both,digits),2,function(x)x[1]>x[2])))

# Wow. Recurrence (I didn't come up with this):
f<-function(x) (2*floor((x-1)/13))+ 1
f(1e6)
