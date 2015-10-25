factorials<-c(1,cumprod(1:9))
names(factorials)<-as.character(0:9)
digit.sum<-function(x) sapply(strsplit(as.character(x),''),function(x)sum(factorials[x]))
which(digit.sum(1:1e6)==1:1e6)