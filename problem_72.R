# Sieve of Erastothenes
sieve<-function(max){
  prime<-rep(TRUE,max)
  prime[1]<-FALSE
  for (i in 1:max) if (prime[i]) prime[seq(i,max,by=i)[-1]]<-FALSE
  which(prime)
}

primes<-sieve(1e6)
nums<-seq.int(1e6)
totient<-nums

for (prime in primes){
  totient[prime]<-prime-1
  if((prime*2)<=1e6){
    x<-seq(prime*2,1e6,by=prime)
    totient[x]<-totient[x] * (1-(1/prime))
  }
}
sum(totient)-1


# Test Zone.
primes<-sieve(8)
nums<-seq.int(8)
totient<-nums

for (prime in primes){
  totient[prime]<-prime-1
  if((prime*2)<=8){
    x<-seq(prime*2,8,by=prime)
    totient[x]<-totient[x] * (1-(1/prime))
  }
}
sum(totient)-1