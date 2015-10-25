# Lychrel numbers.
is.palindrome<-function(x) {
  x<-strsplit(x,'')
  mapply(function(x,y) all(x==y),x,lapply(x,rev))
}

last.char<-function(x){
  x<-strsplit(x,'')[[1]]
  x[length(x)]
}
rest.char<-function(x){
  x<-strsplit(x,'')[[1]]
  if (length(x)==1) return('0')
  paste0(x[-length(x)],collapse='')
}

`%plus%`<-function(x,y){
  x<-strsplit(x,'')[[1]]
  y<-strsplit(y,'')[[1]]
  if (length(x)>length(y)){
    long<-x
    short<-y
  } else {
    long<-y
    short<-x
  }
  short<-c(rep(0,length(long)-length(short)),short)
  carry<-'0'
  current<-c()
  for (i in length(long):1){
    chars<-as.character((as.numeric(long[i])+as.numeric(short[i])+as.numeric(carry)))
    chr<-last.char(chars)
    carry<-rest.char(chars)
    current<-c(chr,current)
  }
  if(as.numeric(carry)==0) carry=''
  paste0(c(carry,current),collapse='')
}

reverse<-function(x) sapply(lapply(strsplit(x,''),rev),paste0,collapse='')
reverse.and.add<-function(x) x %plus% reverse(x)

ends.in.zero<-function(x)
  sapply(strsplit(x,''),function(x) x[length(x)]=='0')

reverse.not.end.in.zero<-function(x) 
  reverse(x[!ends.in.zero(x)])

reverse.not.end.in.zero(c('1230','3212'))

not.lychrel<-c()
lychrel<-c()
for (i in as.character(seq.int(1e4))) {
# for (i in as.character(seq.int(789))) {
  # i<- as.character(seq.int(1e4))[790]
  if (is.list(lychrel)) {
    print(i)
    break
  }
  current<-i
  if(!(current %in% c(lychrel,not.lychrel))) {
    while(TRUE){
      nex<-reverse.and.add(current[length(current)])
      if(nex %in% lychrel){ 
        lychrel<-unique(c(lychrel,current,unlist(reverse.not.end.in.zero(current))))
        break
      }
      if(is.palindrome(nex) || (nex %in% not.lychrel)) {
        not.lychrel<-unique(c(not.lychrel,current,unlist(reverse.not.end.in.zero(current))))
        break
      }
      current<-c(current,nex)
      if (length(current)>=51){
        lychrel<-unique(c(lychrel,current,unlist(reverse.not.end.in.zero(current))))
        break
      }
    }
  }
}
length(lychrel[as.numeric(lychrel)<=1e4])
reverse.and.add(9998)
