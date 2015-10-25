# Super slow.
# I should make use of the fact that all primes must be either:
# 6k-1
# 6k+1
check.primality<-function(x){
  if(x==1) return (FALSE)
  if (x%in%c(2,3)) return (TRUE)
  if (any(x%%c(2,3)==0)) return(FALSE)
  for (i in seq(3,ceiling(sqrt(x)),by=2)) if(x%%i==0) return(FALSE)
  return(TRUE)
}


sapply(19:23,check.primality)

system.time()

system.time(x<-sapply(round(runif(1e5,1,1e7)),check.primality))

check.primality(4e6+1)


which(sapply(1:20,check.primality))


n<-1e5/2
a<-cumsum(c(1,(8*1:n)-6))[-1]
b<-cumsum(c(1,(8*1:n)-4))[-1]
c<-cumsum(c(1,(8*1:n)-2))[-1]
d<-cumsum(c(1,(8*1:n)-0))[-1]
# primes<-sieve(max(d))
# rle(cumsum(a%in%primes))$lengths

# num.primes<-rowSums(sapply(list(a,b,c,d),function(x) cumsum(x%in%primes)))
num.primes<-rowSums(sapply(list(a,b,c,d),function(x) cumsum(sapply(x,check.primality))))

num.diag<-(1:n*4)+1

min(num.primes/num.diag)

(13120*2)+1

num.primes[13120]/num.diag[13120]