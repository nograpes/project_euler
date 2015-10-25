# set.seed(1)
# n<-80
# m<-matrix(round(rnorm(n^2)*100)^2,nrow=n)

m<-as.matrix(read.csv('http://projecteuler.net/project/matrix.txt',header=FALSE))

path<-matrix(NA,nrow=nrow(m),ncol=ncol(m))
path[1,1]<-m[1,1]

incoming<-function(i,j){
  i.s<-c(i-1,i,i,i+1)
  j.s<-c(j,j+1,j-1,j)
  valid<-i.s>0 & i.s<=ncol(m) & j.s>0 & j.s <=nrow(m)
  select<-path[cbind(i.s[valid],j.s[valid])]
  select[!is.na(select)]
}

change<-TRUE
while(change) {
  change<-FALSE
  for(i in 1:nrow(m)){
    for (j in 1:ncol(m)){
      least<-incoming(i,j)
      if(length(least)!=0) {
        test<-min(least) + m[i,j]
        p<-path[i,j]
        if(is.na(p) || test<p) {
          path[i,j]<-test
          change<-TRUE
        }
      }  
    }
  }
}
path[80,80]

