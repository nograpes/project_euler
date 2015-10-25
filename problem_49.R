# Generate all primes.
primes<-2
i<-1
while(i<1e4){
  i<-i+2
  if(!any((i%%primes)==0)) primes<-c(primes,i)
}

require(gregmisc)
all.perms<-permutations(n = 4, r = 4)

shift<-function(x){
  split<-strsplit(as.character(x),'')
  lapply(lapply(split,function(x) apply(all.perms,1,function(y) x[y])),function(x) apply(x,2,paste0,collapse=''))
}


all.primes.shift<-shift(primes)
just.primes<-unique(lapply(all.primes.shift,function(x)sort(unique(as.numeric(x[x%in%primes])))))
just.primes<-just.primes[sapply(just.primes,length)>2]


any.progression<-function(x){
  any(sapply(x,function(y) sapply(x[x!=y],function(z) (z-(y-z)) %in% x)))
}

filtered<-just.primes[(sapply(just.primes,any.progression))]
lapply(filtered,diff)
diff(c(2969,6299,9629))
# 296962999629


