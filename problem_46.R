primes<-2
i<-1
brk<-FALSE
while(!brk){
  i<-i+2
  if(!any((i%%primes)==0)) {
    primes<-c(primes,i)
  } else {
    residue<-sqrt((i-primes)/2)
    if (!any(floor(residue)==residue)) brk<-TRUE
  }
}
i

#sqrt((5777-primes)/2)


#25 - primes