options(scipen=22) # Utterly critical
chop<-function(x){
  x<-as.character(x)
  if(nchar(x)<=10) return(as.numeric(x))
  split<-strsplit(x,'')[[1]][(nchar(x)-9):nchar(x)]
  as.numeric(paste0(split,collapse=''))
}

self.power<-function(x){
  cur<-1
  for (y in 1:x) cur<-chop(cur*x)
  cur
}
s<-sapply(1:1000,self.power)
chop(sum(s))
