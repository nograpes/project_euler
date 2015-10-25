# Generate all primes up to 2001.
primes<-c(2,3)
for (i in seq(5,2001,by=2)){
  if(!any(i %% primes == 0)) primes<-c(primes,i)
}

primality.test<-function(x){
  if (x<1) return(FALSE)
  if (x %in% primes) return(TRUE)
  for (i in primes) if (x %% i == 0) return(FALSE) # Speed filter
  max.poss<-floor(sqrt(x))
  if (max.poss>max(primes)) {
    check<-seq(max(primes),floor(sqrt(x)),by=2)
    for (i in check) if (x %% i == 0) return(FALSE)
  }
  TRUE
}

bs<-primes[primes<=1000]
a.pair<-function(b,prime) prime-1-b


max.prime.gen<-function(a,b){
  i<-0
  while(TRUE) {
    if (!primality.test(func(a,b,i))) return (i)
    i<-i+1
  }
}

pairs<-do.call(rbind,lapply(bs,function(b) merge(b,a.pair(b,primes))))
colnames(pairs)<-c('b','a')
pairs<-pairs[rev(order(pairs$b)),]
pairs<-pairs[abs(pairs$a)<=1000 & abs(pairs$b)<=1000,]

pairs$max<-sapply(1:nrow(pairs),function(x) max.prime.gen(pairs$a[x],pairs$b[x]))

head(pairs[rev(order(pairs$max)),])

func(-61,971,0:70)

971*61

max.prime.gen(1,41)

for (b in rev(bs)) {
  b<-997
  a.pairs<-a.pair(b,primes)
  
}

func<-function(a,b,n) n^2 + a*n + b

func(a.pair(bs[length(bs)],3),bs[length(bs)],2)
  
  
  
# Generate all primes up to 1e6.
primes<-c(2,3)
for (i in seq(5,1e6,by=2)){
  if (i %% 1001 == 0) print(i)
  if(!any(i %% primes == 0)) primes<-c(primes,i)
}

shift<-function(x) {
  bits<-strsplit(as.character(x),'')[[1]]
  x<-c(bits[length(bits)],bits[-length(bits)])
  as.numeric(paste0(x,collapse=''))
}

shift.recursively<-function(x,i){
  if(i==1) return(x)
  return(c(x,shift.recursively(shift(x),i-1)))
}

all.shift<-function(x) {
  shift.recursively(x,nchar(as.character(x)))
}


all.rotations<-function(x) {
  all.shift(bits)
}

s<-mclapply(primes,function(x) all.shift(x) %in% primes)


s<-mclapply(primes,function(x) all(all.shift(x) %in% primes))
table(unlist(s))



