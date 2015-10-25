# Cheap and really slow.
primes<-2
max<-1e6
potential<-seq(3,max,by=2)
while(length(potential)>0){
  cur<-potential[1]
  primes<-c(primes,cur)
  multiples<-seq(cur,max,by=cur)
  potential<-potential[-c(1,multiples)]
}

current<-1
for (i in 1:length(primes)){
  for (j in i:length(primes)){
    if(((j-i+1)>current) && sum(primes[i:j]) < 1e6 && sum(primes[i:j]) %in% primes) {
      current<-j-i+1
      print(current)
      print(sum(primes[i:j]))
    }
  }
}
