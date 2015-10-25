# Goofball
dat = c(9, 2, 7)
k = seq(0, 1, length = 10)
x =list(a = 1, b = 8, c = 4)
y = list(a = .5, b = 5, c = 5)
matrix = cbind(unlist(x), unlist(y)) %*% rbind(1-k, k)

colSums(matrix(rep(dat,nrow(matrix)),ncol=nrow(matrix)) %*% (matrix*c(1,-1,1)))

# z = apply(matrix, 2, as.list) # unecessary.


# fun = function(dat, vec) sum(vec$a * dat - vec$b * dat + vec$c * dat)

# res = rep(0, length(k))
# for (i in 1:(length(k))){
#   res[i] = fun2(dat, matrix[,i])
# }


res


logCounter <- function(number) {
  k <- 0
  if(k>=0){
    k = k+1
  }
  result <- log(number)
  if (result > 1) {
    logCounter(result)
  } else {
    return(k)
  }
}


logCounter <- function(number) {
  result <- log(number)
  if (result > 1) return(logCounter(result)+1)
  return(1)
}

debug(logCounter)
logCounter(123)




set.seed(1)
x1 = 1:5
x2 = 5:1
x3 = seq(2, 10, 2)
xl <- list(x1, x2, x3)
y1 = rnorm(5)
y2 = runif(5)
y3 = seq(20, 12, -2)
yl <- list(y1, y2, y3)
z1 = rnorm(5)
z2 = runif(5)
z3 = seq(20, 12, -2) %% 3
zl <- list(z1, z2, z3)
LL <- list(xl, yl, zl)

new.LL<-lapply(seq_along(LL),function(f)do.call(cbind,LL[[f]]))

new.LL[[1]] - new.LL[[2]]

lapply(combn(seq_along(new.LL),2),2,function(x) new.LL[[x[1]]]-new.LL[[x[2]]])

i<-1

unlist(
lapply(1:(length(new.LL)-1), f<-function(i)
  mapply(`-`,new.LL[i],new.LL[setdiff(1:length(new.LL),1:i)],SIMPLIFY=FALSE)
),recursive=FALSE)

# Convert to matrices.
new.LL<-lapply(seq_along(LL),function(f)do.call(cbind,LL[[f]]))
# Loop over each combo
lapply(apply(combn(1:3,2),2,function(x) c(new.LL[x])),function(x) x[[1]]-x[[2]])

# Convert to matrices.
new.LL<-lapply(seq_along(LL),function(f)do.call(cbind,LL[[f]]))
# Loop over each combo
lapply(apply(combn(1:length(new.LL),2),2,
             function(x) new.LL[x]),function(x) x[[1]]-x[[2]])

lapply(apply(combn(1:length(new.LL),2),2,
             `[`,x=new.LL),function(x) x[[1]]-x[[2]])

combos<-combn(1:length(new.LL),2)
mapply(`-`, new.LL[combos[1,]],new.LL[combos[2,]],SIMPLIFY=FALSE)

class(new.LL[combos])
## More sweeping:
A <- array(1:24, dim = 4:2)

## no warnings in normal use
sweep(A, 1, 5)
lapply(split(new.LL[combn(1:length(new.LL),2)],rep(seq_along(new.LL),each=2)),function(x)x[[1]]-x[[2]])

sweep(combos,2,STATS=I)

?sweep
`[`(i=1,x=new.LL)

`-`(list(1,2))
`-`(c(1,2))

apply(combn(1:3,2),2,`[`,new.LL)

?`[`

mapply("-", LL[[1]], LL[[2]])
mapply("-", LL[[1]], LL[[3]])
mapply("-", LL[[2]], LL[[3]])

zl

unlist(LL)




# Find all squares 
# If the perimeter is 1000, then the max hypo is 334
# Question 43?
max.root.hypo<-500
x<-(1:max.root.hypo)^2
p<-rep(0,1000)
for (i in seq_along(x)){
  test<-which((x[i]+x) %in% x)
  test<-test[test>i]
  if (length(test)>0) {
    a<-rep(x[i],length(test))
    b<-x[test]
    c<-a+b
    indices<-colSums(sqrt(rbind(a,b,c)))
    p[indices]<-p[indices]+1
  }
}
which(p==max(p,na.rm=TRUE))

# Question 44
p<-function(n)(n*((3*n)-1))/2

check.pentagonal<-function(p) {
  roots<-polyroot(rev(c(1.5,-0.5,-p)))
  real.roots<-Re(roots[mapply(all.equal,Im(roots),rep(0,length(roots)))])
  pos.roots<-real.roots[real.roots>0]
  integer.roots<-pos.roots[pos.roots==floor(pos.roots)]
  length(integer.roots)>0
}

which.pentagonal<-function(p) {
  roots<-polyroot(rev(c(1.5,-0.5,-p)))
  real.roots<-Re(roots[mapply(all.equal,Im(roots),rep(0,length(roots)))])
  pos.roots<-real.roots[real.roots>0]
  integer.roots<-pos.roots[pos.roots==floor(pos.roots)]
  integer.roots
}

i<-1
test<-p(i)
#test<-p(1:2166)
#i<-2167
goof<-c()
minest.diff<-Inf
minest.pair<-c()
diffs<-c()
while(length(goof)==0) {
  i<-i+1
  if(i %% 10000 == 0 ) print(i)
  last<-p(i)
  if(last - test[length(test)] > minest.diff) break
  k<-which((p(i)-test) %in% test)  
  if(length(k)>0) {
    x<-sapply(p(i)+p(k),check.pentagonal)
    if (length(x)>0){
      confirmed.k<-k[x]
      min.diff<-min(abs(p(i)-p(confirmed.k)))
      if (min.diff<minest.diff) {
        minest.diff<-min.diff
        minest.k<-confirmed.k[which((abs(p(i)-p(confirmed.k)))==min.diff)]
        minest.pair<-c(i,minest.k)
      }
    }
  }
  # Check neg side.
  k<--which((p(i)-test) %in% (test+seq_along(test)))  
  if(length(k)>0) {
    x<-sapply(p(i)+p(k),check.pentagonal)
    if (length(x)>0){
      confirmed.k<-k[x]
      min.diff<-min(abs(p(i)-p(confirmed.k)))
      if (min.diff<minest.diff) {
        minest.diff<-min.diff
        minest.k<-confirmed.k[which((abs(p(i)-p(confirmed.k)))==min.diff)]
        minest.pair<-c(i,minest.k)
      }
    }
  }
  test<-c(test,last)
}

warnings()

p(i) - p(i-1)
p(i-1) - p(i-2)


check.pentagonal(sum(p(minest.pair)))
check.pentagonal(diff(p(minest.pair)))

check.pentagonal(p(29001) - p(29000))
check.pentagonal(p(2167) - p(1020))
minest.pair

p(28077) - p(28076)

sapply(diffs,which.pentagonal)
# Defintely lower than 1147


df<-data.frame(a=sample(c(TRUE,FALSE),1000,replace=TRUE),b=sample(c(TRUE,FALSE),1000,replace=TRUE))
df[apply(df,1,any),]

goof

check.pentagonal(p(49) + p(47))



p(48)

goof


test<-p(1:m)
sums<-rowSums(expand.grid(test,test)) %in% test
diff<-abs(rowSums(expand.grid(test,-test))) %in% test

length(which(diff))

expand.grid(test,test)[3007,]

which(sums)[1]

which(sums&diff) 


