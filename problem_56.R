library(gmp)
power<-function(a,b) as.character(as.bigz(a)^as.bigz(b))
digit.sum<-function(x) sapply(lapply(strsplit(x,''),as.numeric),sum)
both<-expand.grid(1:100,1:100)
s<-unname(sapply(mapply(power,both[,1],both[,2]),digit.sum))
max(s)

