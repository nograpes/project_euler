data<-as.numeric((read.csv('http://projecteuler.net/project/cipher1.txt',header=FALSE))[1,])
codes<-apply(expand.grid(letters,letters,letters),1,paste0,collapse='')
keys<-lapply(codes,function(x)rep(charToRaw(x),length.out=length(data)))

decrypts<-sapply(keys,function(x)rawToChar(xor(as.raw(data),x)))
l<-sapply(strsplit(decrypts,' '),length)
decrypts[l==max(l)] # Check.
sum(as.numeric(charToRaw(decrypts[l==max(l)])))
