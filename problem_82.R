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

set.seed(1)
n<-300
m1 <- matrix(rnorm(n^2),ncol=n)
m2 <- matrix(rnorm(n^2),ncol=n)
m <- cbind(m1,m2)
dim(m)

png('test_heatmap.png')
system.time(heatmap(m,useRaster=TRUE))
dev.off()

png('test_heatmap.png')
system.time(heatmap(m,useRaster=FALSE))
dev.off()



library(Cairo)
CairoPNG('test_heatmap.png')
system.time(heatmap(m,useRaster=TRUE))
dev.off()

help(package='Cairo')




