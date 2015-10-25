prime.check.one<-function(x) {
  if(x==2) return(TRUE)
  y<-ceiling(sqrt(x))
  !any(x%%(2:y)==0)
}

prime.check<-function(x) sapply(x,prime.check.one)

left<-c(2,3,5,7)
right<-c(2,3,5,7)

expand.right<-function(x){
  x<-expand.grid(x,1:9)
  as.numeric(paste0(x[,1],x[,2]))
}

expand.left<-function(x){
  x<-expand.grid(1:9,x)
  as.numeric(paste0(x[,1],x[,2]))
}

truncatable.primes<-c()
while(length(truncatable.primes)<11){
   left<-expand.left(left)    
   right<-expand.right(right)
   left<-left[prime.check(left)]
   right<-right[prime.check(right)]
   new.primes<-intersect(left,right)
   if(length(new.primes)>0) truncatable.primes<-c(truncatable.primes,new.primes)
}

sum(truncatable.primes)