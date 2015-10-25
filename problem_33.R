set<-1:9
accum<-data.frame(Var1=c(),Var2=c())
for (i in set) {
  for (j in set[-i]){
    target<-i/j
    for (k in set[-c(i,j)]){
      tops<-c(k*10 + i,i*10 + k)
      bottoms<-c(k*10 + j,j*10 + k)
      df<-expand.grid(tops,bottoms)
      accum<-rbind(accum,df[df$Var1/df$Var2==target,])
    }
  }
}

res<-accum[accum$Var1<accum$Var2,]

a<-prod(res[,1])
b<-prod(res[,2])

# a is obviously 1 / 100th of b