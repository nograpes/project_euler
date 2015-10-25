scores<-c(sapply(1:20,`*`,1:3),25,50)
all.scores<-do.call(rbind,lapply(seq_along(scores),function(x) 
  expand.grid(scores[x],scores[x:length(scores)])))
all.scores$sum<-all.scores[,1]+all.scores[,2]
ways<-table(all.scores$sum)
t<-table(scores)
ways[as.character(1)]<-0
ways[names(t)]<-ways[names(t)]+t
ways['0']<-1

all.scores()


as.character(100-c(1:20,25)*2)

sum(ways[])

x<-(6- (c(1:20,25)*2))
x<-x[x>=0]
sum(ways[as.character(x)])


ways[!(names(t) %in% names(ways))]

f<-function(x) {
  x<-(x- (c(1:20,25)*2))
  x<-x[x>=0]
  sum(ways[as.character(x)])
}  

sum(sapply(2:99,f))

  sum(ways[as.character(x-c(1:20,25)*2)],na.rm=TRUE)