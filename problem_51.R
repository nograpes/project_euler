# Sieve of Erastothenes
sieve<-function(max){
  prime<-rep(TRUE,max)
  prime[1]<-FALSE
  for (i in 1:max) if (prime[i]) prime[seq(i,max,by=i)[-1]]<-FALSE
  which(prime)
}

# Generate families
f<-function(val,digits,num.stars){
  pos<-which(digits==val)
  if(length(pos)>1)combos<-combn(pos,num.stars)
  else combos<-matrix(pos)
  apply(combos,2,function(x) {
    digits[x]<-'*'
    paste0(digits,collapse='')}
  )
}

# Actually, any family with two or four stars can never work.
# At least three of the numbers will not be prime, because
# at least three of them will make the whole number divisible
# by 3. 
generate.families<-function(x){
  digits<-strsplit(as.character(x),'')[[1]]
  tab<-table(digits)
  biggest.run<-max(tab)
  families<-c()
  for (num.stars in 1:biggest.run){
    vals<-names(tab[tab>=num.stars])
    families<-c(families,unlist(sapply(vals,f,digits,num.stars)))
  }
  families
}

test<-function(max){
  primes<-sprintf('%05d',sieve(max))
  l<-lapply(primes,generate.families)
  t<-table(unlist(l))
  t[t>=8]
}

# Manual, but pointless to write generalized code.
test(1e6)
# *2*3*3
primes<-sprintf('%05d',sieve(1e6))
020303 %in% primes # FALSE
121313 %in% primes # FALSE

