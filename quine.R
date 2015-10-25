l<-c()
l[1]<-'l<-c()'
l[2]<-'cat(paste(l[1],rawToChar(as.raw(10))))'
l[3]<-'for (i in 1:4) cat(paste0("l[",i,"]<-",rawToChar(as.raw(39)),l[i],rawToChar(as.raw(39)),rawToChar(as.raw(10))))'
l[4]<-'for (i in 2:4) cat(paste0(l[i],rawToChar(as.raw(10))))'
cat(paste(l[1],rawToChar(as.raw(10))))
for (i in 1:4) cat(paste0("l[",i,"]<-",rawToChar(as.raw(39)),l[i],rawToChar(as.raw(39)),rawToChar(as.raw(10))))
for (i in 2:4) cat(paste0(l[i],rawToChar(as.raw(10))))
