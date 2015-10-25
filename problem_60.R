c(3,7,109,673)

# Generate all primes to 1e6.
# Sieve of Erastothenes
sieve<-function(max){
  prime<-rep(TRUE,max)
  prime[1]<-FALSE
  for (i in 1:max) if (prime[i]) prime[seq(i,max,by=i)[-1]]<-FALSE
  which(prime)
}
primes<-sieve(1e6)

prefix=541
# For each prime, find all suffixes?
get.suffixes <- function(prefix) {
  x<-gsub(paste0('^',prefix),'',primes[grep(paste0('^',prefix,'.+'),primes)])
  x=  as.numeric(x[substr(x,1,1)!='0'])
  x=x[x>prefix]
  flip=as.numeric(paste0(x,prefix)) 
  x[flip %in% primes]
}

get.suffixes(3)

??char

get.suffixes(541)

length(primes)^2



# Would simple brute force even work?
# Generate primes as necessary?
# Each primes checked against *all* primes lower?

