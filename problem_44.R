# Pentagon numbers
pentagonal<-function(n) (n*((3*n)-1))/2

i<-0
current<-c()
cs<-c()
bs<-c()
while(!any((cs - bs) %in% current)){
  i<-i+1
  test<-pentagonal(i)
  cs<-(current)[(test-current) %in% current]
  bs<-(test-current)[(test-current) %in% current]
  current<-c(current,test)
}

ind<-which((cs-bs) %in% current)
min(cs[ind]-bs[ind])



# Garbage.
is.integer.and.real<-function(x) sapply(Im(x),all.equal,0) & mapply(function(x,y)is.logical(all.equal(x,y)),Re(x) %% 1,0) & Re(x)>0
# is.pentagonal<-function(n) any(sapply(polyroot(rev(c(1.5,-0.5,-n))),is.integer.and.real))
check<-function(n1,n2) is.pentagonal(sum(pentagonal(c(n1,n2)))) && is.pentagonal(abs(pentagonal(n1)-pentagonal(n2)))
current.max

x<-merge(pentagonal(1:10000),pentagonal(1:10000),all=TRUE)
x<-x[x$x!=x$y,]
x$diff<-abs(x[,2]-x[,1])
x$sum<-abs(x[,2]+x[,1])

big<-ceiling(Re(polyroot(rev(c(1.5,-0.5,-max(x$sum))))))[1]
all.pent<-pentagonal(1:big)
y<-(x$diff %in% all.pent) && (x$sum %in% all.pent)

test<-x[sapply(x$diff,is.pentagonal) & sapply(x$sum,is.pentagonal),]
head(test[order(test$diff),])


n<-2
upper.limit<-NA
best<-c()
while (is.na(upper.limit) | ((pentagonal(n)-pentagonal(n-1)) <= upper.limit)){
  found<-((n-1):1)[mapply(check,((n-1):1),n)]
  if(length(found)>0){
    max.found<-max(found)
    best<-n-max.found
    if(best<upper.limit | is.na(upper.limit)) {
      upper.limit<-max.found
      best<-c(n,max.found)
    }
  }
  n<-n+1
}

best
