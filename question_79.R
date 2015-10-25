# Problem 79.
# Don't try and maintain the structure as you go along. Dragons lie there.

x<-unname(unlist(read.table('http://projecteuler.net/project/keylog.txt')))
m<-strsplit(as.character(x),'')

first<-function(m) sapply(m,"[",1)
not.first<-function(m) c(sapply(m,"[",-1))
strip<-function(m,char) lapply(m,function(y) if(y[1]==char) y[-1] else y)

str<-''
while(any(lapply(m,length)!=0)) {
  char<-setdiff(first(m),not.first(m))
  m<-strip(m,char)
  m<-m[lapply(m,length)!=0]
  str<-c(str,char)
}
paste0(str,collapse='')

