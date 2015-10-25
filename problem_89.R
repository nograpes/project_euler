# Problem 89
x<-as.character(unname(unlist(read.table('http://projecteuler.net/project/roman.txt'))))
x<-strsplit(x,'')
convert<-function(x) lapply(lapply(x,match,table=c('I','V','X','L','C','D','M')),function(x)c(1,5,10,50,100,500,1000)[x])
nums<-convert(x)
f<-function(s) {
  u<-c(diff(s)>0,FALSE)
  sum(s[!u]) - sum(s[u])
}
digits<-sapply(nums,f)



f<-function(t,x) 1-((1-x)^(1:t))

(0.97^5)

library(reshape2)
library(ggplot2)
df=data.frame(time=1:36,treatment=f(36,0.08),no.treatment=f(36,0.03))
ggplot(data=melt(df,id.vars='time'),aes(x=time,y=value,colour=variable)) + geom_line(size=2)

digits
