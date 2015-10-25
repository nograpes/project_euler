even.palindromes<-function(length){
  side<-length/2
  bottom<-10^(side-1)
  top<-(10^(side))-1
  half<-bottom:top
  rev.half<-sapply(lapply(strsplit(as.character(half),''),rev),paste0,collapse='')
  evens<-as.numeric(paste0(as.character(half),rev.half))
  f<-function(x)as.numeric(paste0(as.character(half),x,rev.half))
  sapply(c('',as.character(1:9)),f)
}

rev.string<-function(x) sapply(lapply(strsplit(x,''),rev),paste0,collapse='')
binary<-function(x)sub('^[0]+','',as.character(intToBin(x)))

palindromes<-c(1:9,unlist(sapply(c(2,4,6),even.palindromes)))
palindromes<-palindromes[palindromes<1e6]
stripped.binary<-binary(palindromes)

double.base.palindromes<-palindromes[which(rev.string(stripped.binary)==stripped.binary)]
sum(double.base.palindromes)



