# Sieve of Erastothenes
sieve<-function(max){
  prime<-rep(TRUE,max)
  prime[1]<-FALSE
  for (i in 1:max) if (prime[i]) prime[seq(i,max,by=i)[-1]]<-FALSE
  which(prime)
}

primes<-sieve(1e6)

# Resilience
totient<-function(x){
  factors<-primes[x%%primes==0]
  x*prod(1-(1/factors))
}

# Not quite resilience.
ratio<-function(x) x/totient(x)

# Just guessed till I found which one was less than 1e6
ratio(prod(primes[1:7]))

# This one is much easier once you understand problem 243.

