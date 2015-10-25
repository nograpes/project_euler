colA= c("A","A","A","B","B","B","C","C","C")
colB= c(1,2,3,3,0,1,2,1,0)
colC= c(0,0,0,1,0,0,0,0,1)
df= data.frame (site=colA, sp1=colB, sp2=colC)

library(reshape2)
melt(df)



f<-function(n) 1/(1-n^(1:n))


f<-function(x) x^(1:x)

f(2)


sapply(1:100,P)

Q(5,give=TRUE)

i<-1
res<-P(i)
while ((res %% 1e6) !=0){
  if ((i%%100)==0)print(i)
  i<-i+1
  res<-P(i)
}

is.double(P(i))

library(Rmpfr)

?mpfr

strip<-function(x) as.integer(paste0(strsplit(as.character(x),'')[[1]][-1],collapse=''))

strsplit

partition<-function (n,k){
  #current<-solved[[n]][[k]]
   current<-solved[n,k]
  if(!is.na(current)) return(current)
  sum = 0
  if(k > n) return (0)
  if(k == n) return (1)
  sum <- sum + partition(n,k+1) + partition(n-k,k)
  if (sum>1e8) sum <-strip(sum)
   solved[n,k]<<-sum
  #solved[[n]][[k]]<<-sum
  return (sum)
}

strip<-function(x) as.integer(paste0(strsplit(as.character(x),'')[[1]][-1],collapse=''))
n<-5e4
solved<-Matrix(0,nrow=n,ncol=n)
for (i in 1:n) {
  if (i %% 1000 ==0) print(i)
  for (k in i:1) {
    if (k==i) { solved[i,k]<-1
    } else {
      if(k>(i-k)) { second<-0
      } else second<-solved[i-k,k]
      sum<-solved[i,k+1] + second
      if (sum>1e8) sum<-strip(sum)
      solved[i,k]<-sum
    }
  }
  if(solved[i,1]%%1e6 == 0) {
    print(i)
    break
  }
}

library(inline)
help(package=inline)


f<-function(n) {
  n<-n+1
  pp[0:1 +1] <- as.integer(1)
  for(i in 3:n){
    # /* first do r = m(3m+1)/2  */
      s = 1 #  /* "s" for "sign" */
      f = 5 #  /* f is first difference  */
      r = 2 #  /* initial value (viz m(3m+1)/2 for m=1) */
      pp[i] = as.integer(0)
    while(i-r >= 1){
      pp[i] = pp[i]+ s*pp[i-r]
      r = r+ f
      f = f+3 #  /* 3 is the second difference */
        # /* change sign of s */
        s = s*-1
    }
    # /* now do r = m(3m-1)/2 */
    s = 1
    f = 4 # /*first difference now 4 */
    r = 1 # /* initial value (viz m(3m-1)/2 for m=1) */
      while(i-r >= 1){
        pp[i] = pp[i] + (s*pp[i-r])
        r = r+f
        f = f+3
        s = s*-1
      }
      if (pp[i] %% 1e6 == 0) {
        print(i)
        return(i)
      }
      if (pp[i]>1e8 | pp[i]<1e8) pp[i]<-strip(pp[i])
  }
  pp[-1]
}

strip<-function(x){
  ch<-as.character(x)
  splitted<-strsplit(ch,'')[[1]]
  sign<-''
  if(splitted[1]=='-'){
    sign<-'-'
    splitted<-splitted[-1]
  }
  if(length(splitted)<=8) return(x)
  size<-length(splitted)
  to.strip<-size-8
  as.integer(paste0(sign,paste0(splitted[-(1:to.strip)],collapse=''),collapse=''))
}

warnings()

S<-f(500000)
which((S %% 1e6) == 0)

as.numeric(paste(strsplit(as.character(15065878135),'')[[1]][-1],collapse=''))

warnings()

all(f(50)==P(50,give=TRUE))

library(partitions)
P(50,give=TRUE)
f(50)

code

n<-301
P(301) %% 1e8 == solved[301,1]


P(300,give=TRUE)[300] %% 1e8 == solved[300,1]


(P(n,give=TRUE) %% 1e8 != solved[1:n,1])

warnings()

tail(saved)
tail(solved[,1])

saved<-solved[,1]

which(solved[,1]%%1e6==0)


partition(1,449)

solved[448,1]

options(expressions=1e5)



solved[359,1] == P(359,give=TRUE)

solved[359,1] %% 1e6

all(solved[1:159,1]==P(159,give=TRUE))

P(6,give=TRUE)
s<-sapply(359,partition,k=1)

P(i) > 2147483647

P(i) %% 1e6

P(5,give=TRUE)

i

P(i,give=TRUE) %% 1e6



Q(i,give=TRUE) %% 1e6
solved[n,k]<<-sum