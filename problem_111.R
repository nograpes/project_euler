# Problem 111
bad.prime.check<-function(x){
  i<-2
  while(i<=ceiling(sqrt(x))){
    if(x%%i==0)return(FALSE)
    i<-i+1  
  }
  return(TRUE)
}

gen.all.primes<-function(num,d,digits=10){
  x<-(0:9)[-(num+1)]
  subs<-  t(do.call(expand.grid,rep(list(x),d)))
  template<-rep(num,digits)
  pos<-combn(1:digits,d)
  mat<-do.call(cbind,lapply(1:ncol(subs), function(y) apply(pos,2,function(x) replace(template,x,subs[,y]))))
  x<-as.numeric(apply(mat,2,paste0,collapse=''))
  x[nchar(x)==digits]
}

f<-function(num,d,digits=10){
  x<-gen.all.primes(num,d,digits=digits)
  for(i in x) if (bad.prime.check(i)) return(TRUE)
  return(FALSE)
}

digits=10
final<-c()
highest<-c()
pool<-list()
for (i in 0:9){
  j<-1
  while(!f(i,j,digits=digits)) j<-j+1
  highest[i+1]<-j
  x<-unique(gen.all.primes(i,j,digits=digits))
  pool[[i+1]]<-x[sapply(x,bad.prime.check)]
  final[i+1]<-sum(pool[[i+1]])
}
sum(final)
