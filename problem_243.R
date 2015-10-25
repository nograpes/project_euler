# Sieve of Erastothenes
sieve<-function(max){
  prime<-rep(TRUE,max)
  prime[1]<-FALSE
  for (i in 1:max) if (prime[i]) prime[seq(i,max,by=i)[-1]]<-FALSE
  which(prime)
}

primes<-sieve(1e7)

# Resilience
totient<-function(x){
  factors<-primes[x%%primes==0]
  x*prod(1-(1/factors))
}

resilience<-function(x) totient(x)/(x-1)

# Guessing. 
# Numbers with very low resilience will be numbers that 
# are divisible by a large number of primes.
# The totient function, or Euler's phi function, gives the 
# exact number of coprimes.
# Interestingly, the totient function indicates that for any
# set of primes, no matter what exponent you take each of the 
# primes to, the number of coprimes will be exactly proportional.
# So 2*3 = 6. 
totient(6) # 2 coprimes.
# And 2^2*3=12
totient(12) # 4 coprimes
# So, at first I thought the resilience was constant. But, then I 
# realized that it isn't, because it isn't 2/6 and 4/12, but 2/5 and 4/11
# So the resilience actually *decreases* for any set of primes as you increase
# the exponent.
# But eventually, it will cross the next set of primes, like
# 2*3*5
# So, I just kind of guessed, for all the products of primes, found that 
# The product of the first nine primes was close but not quite right,
# and then guessed up from there. 

resilience(12)
4/11

15499 / 94744
resilience(prod(primes[1:9])) >  (15499 / 94744)
resilience(prod(primes[1:9])*2*3*5*7) <  (15499 / 94744)
resilience(prod(primes[1:9])*2*3) <  (15499 / 94744)

resilience(prod(primes[1:9])*7) <  (15499 / 94744)
resilience(prod(primes[1:9])*5) <  (15499 / 94744)
resilience(prod(primes[1:9])*2*2) <  (15499 / 94744)
resilience(prod(primes[1:9])*3) <  (15499 / 94744)
resilience(prod(primes[1:9])*2) <  (15499 / 94744)


resilience(prod(primes[1:8]))


resilience(prod(primes[1:8])*11*2*5) <  (15499 / 94744)

prod(primes[1:9])*5 # 1115464350
prod(primes[1:9])*3 


# The product of the first nine primes is greater than:
# 15499 / 94744
prod(primes[1:9]) # 223092870

resilience(prod(primes[1:9])*2) # 446185740

totient(prod(primes[1:9]))

resilience(prod(primes[1:9]))>resilience(prod(primes[1:9])*2)

resilience(prod(primes[1:3])*6)
resilience(prod(primes[1:4]))


primes[9]
resilience(prod(primes[1:8]))



resilience(prod(primes[1:9])) # 223092870

resilience(prod(c(primes[c(1:8,10)])))

resilience(prod(primes[1:10])) # 6469693230

resilience(prod(primes[1:3])) # 30
resilience(prod(primes[1:4])) # 210

which(sapply(30:210,resilience) < resilience(30))

resilience(60)
resilience(30)


resilience(primes[1]*primes[2]*primes[6])