get.ancestors<-function(x){
  y<-rbind(x-c(1,0),x-c(0,1))
  y[y[,1]>0 & y[,2]>0,,drop=FALSE]
}

mat<-as.matrix(read.csv('http://projecteuler.net/project/matrix.txt',header=FALSE))
m<-matrix(nrow=80,ncol=80)

for (row in 1:80){
  for (col in 1:80) {
    last<-get.ancestors(c(row,col))
    least.ancestor<-ifelse(nrow(last)==0,0,min(m[last]))
    m[row,col]<-mat[row,col]+least.ancestor
  }
}
m[80,80]

