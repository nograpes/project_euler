# Problem 43
divisible.by<-c(2,3,5,7,11,13,17)
no.repeats<-function(x) x[sapply(sapply(strsplit(x,''),unique),length)==sapply(x,nchar)]
divisor<-function(x)no.repeats(sprintf((1:999)[1:999 %% x == 0],fmt='%03.0f'))

overlap<-function(x,y) {
  x.tail<-paste0(strsplit(x,'')[[1]][-1],collapse='')
  y.head<-sapply(strsplit(y,''),function(x)paste0(x[1:2],collapse=''))
  x.head<-paste0(strsplit(x,'')[[1]][1],collapse='')
  if (any(x.tail==y.head)) return(paste0(x.head,y[x.tail == y.head]))
  return(c())
}

all.overlap<-function(x,y) no.repeats(unlist(sapply(x,overlap,y=y)))

listo<-rev(sapply(divisible.by,divisor))
cur<-listo[[1]]
for(l in listo[-1]){
  cur<-all.overlap(l,cur)
}
sum(as.numeric(paste0(sapply(strsplit(cur,''),setdiff,x=as.character(1:9)),cur)))