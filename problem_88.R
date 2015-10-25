# Problem 88
# This was a super tough one. Confusing to think about.
# To recreate the mindset you were in when you did this, consider a few things:

# 1) If you take any composite, you can generate an integer set that sums and adds to it, just by filling in ones. Like 16, it splits into 8*2, but since 8+2==10, just add in six more 1s. So the sequence for that pair is (8,2,1,1,1,1,1,1). So for 16, you can make at least k=8.
# 2) The same is true for all of a composites possible factorizations. They can always be used to match up with one k.
# 3) Given the sum of the factorization, and the number of factors, you can calculate k. So, as above, the number of factors =2 and the sum was 10, so k=2+(16-10). This is true in general.
# 4) Finally, note that if you calculate all the possible sums and lengths for factorization sets for some number, you can use it to calculate lengths and sums for its multiples. So, once you have done 16, you can easily calculate some sums and lengths for 32, just by adding one more factor to the length, and 2 to the sum. If you do this cleverly, you only need to generate factor pairs for each n.


# Find all primes up to 24K
# max<-18
max<-24000 

primes<-2
i<-1
while(i<=max){
  i<-i+2
  if(!any((i%%primes)==0)) primes<-c(primes,i)
}

non.primes<-setdiff(2:max,primes)

fac.sum<-list()
fac.len<-list()
k<-rep(NA,12000)

all.divisor.pairs<-function(n){
  d<-1:floor(sqrt(n))
  small<-d[n%%d==0]
  cbind(small,n/small)[-1,,drop=FALSE]
}

for (n in non.primes){
  divisor.pairs<-all.divisor.pairs(n)
  sums<-rowSums(divisor.pairs)
  lens<-rep(2,length(sums))
  for (j in 1:nrow(divisor.pairs)){
    m<-divisor.pairs[j,]
    for (i in m) {
      if (!(i %in% primes)){
        # Add it in.
        i.sums<-fac.sum[[i]]+(n/i)
        i.lens<-fac.len[[i]]+1
        sums<-c(sums,i.sums)
        lens<-c(lens,i.lens)
      }
    }
  }
  all.k<-unique(n-sums+lens)
  k[all.k[is.na(k[all.k])]]<-n
  fac.sum[[n]]<-sums
  fac.len[[n]]<-lens
}
sum(unique(k[2:12000]))
