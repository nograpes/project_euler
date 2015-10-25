neighbour.list<-function(x,y,grid.size) {
  slides<-c(1,-1,0,0)
  moves<-data.frame(x=slides,y=rev(slides))
  new.pos<-data.frame(x=x+moves$x,y=y+moves$y)
  with(new.pos,new.pos[x>0 & y>0 & y<=grid.size & x<=grid.size,])
}

i.and.j<-function(i,j,grid.size) ((i-1) * grid.size)+ j

# Init prob mat.
grid.size<-30
prob.list<-list()
for (i in 1:grid.size){
  for(j in 1:grid.size){
    neighs<-neighbour.list(i,j,grid.size)
    prob<-1/nrow(neighs)
    trans.probs<-rep(0,grid.size^2)
    positions<-mapply(i.and.j,neighs$x,neighs$y,grid.size)
    trans.probs[positions]<-prob
    prob.list[[length(prob.list)+1]]<-trans.probs
  }
}
trans.mat<-do.call(rbind,prob.list)
mat.pow(trans.mat,50)


t(c(1,rep(0,(grid.size^2)-1))) %*% mat.pow(trans.mat,50)

mat.pow<-function(trans.mat,pow)Reduce(`%*%`,lapply(1:pow,function(x)trans.mat))


# Old.
calc.dist<-function(x,y){
  prob.table<-data[[x]][[y]]
  neighbours<-neighbour.list(x,y,grid.size)  
  num.neigh<-nrow(neighbours)  
  
  f<-function(x) {
    row<-prob.table[x,]  
    flies<-row$flies
    exploded.table<-table.per.flies(flies,num.neigh)
    exploded.table$probs<-exploded.table$probs*row$probs
    exploded.table
  }
  
  tab.reduce(do.call(rbind,lapply(1:nrow(prob.table),f)))
}

table.per.flies<-function(flies,neighbours) {
  left<-(1/neighbours)^(0:flies)
  right<-((neighbours-1)/neighbours)^(flies:0)
  ways<-sapply(0:flies,choose,n=flies)
  probs<-left*right*ways
  
  d<-data.frame(probs=probs,flies=0:flies)
  tab.reduce(d[d$probs>0,])
}

combine.prob.table.list<-function(list.of.tables) Reduce(merge.two.prob.tables,list.of.tables)

merge.two.prob.tables<-function(table.a,table.b){
  red.a<-tab.reduce(table.a)
  red.b<-tab.reduce(table.b)
  x<-merge(unname(table.a),unname(table.b),all=TRUE)
  flies<-rowSums(x[,c(2,4)])
  probs<-x[,1]*x[,3]
  combined<-data.frame(probs,flies)
  tab.reduce(combined)
}

tab.reduce<-function(tab) aggregate(probs~flies,data=tab,sum)[c('probs','flies')]

zero.if.empty<-function(x) ifelse(length(x)==0,0,x)

expected.empty<-function(data)
  sum(sapply(data,function(x)sapply(x,function(y) zero.if.empty(y$probs[y$flies==0]))))


start.prob.table<-data.frame(probs=1,flies=1)
grid.size=3
data<-lapply(1:grid.size,function(y)lapply(1:grid.size,function(x)start.prob.table))

new.data<-lapply(1:grid.size,function(y)lapply(1:grid.size,function(x)list()))
expected<-c()

for (jumps in 1:10){
  expected[jumps]<-expected.empty(data)
  for(i in 1:grid.size){
    for (j in 1:grid.size){
      neighbours<-neighbour.list(i,j,grid.size)
      table.to.dist<-calc.dist(i,j)
      for (k in 1:nrow(neighbours)){
        row<-neighbours[k,]
        cur.list<-new.data[[row$x]][[row$y]] 
        new.data[[row$x]][[row$y]][[length(cur.list)+1]]<-table.to.dist
      }
    }
  }
  
  for(i in 1:grid.size) 
    for (j in 1:grid.size) 
      new.data[[i]][[j]]<-combine.prob.table.list(new.data[[i]][[j]])
  
  data<-new.data
  new.data<-lapply(1:grid.size,function(y)lapply(1:grid.size,function(x)list()))
}


# Create a sim for verification.
sim.one.step<-function(data){
  new.data<-matrix(0,ncol=grid.size,nrow=grid.size)
  for(i in 1:grid.size){
    for (j in 1:grid.size){
      neighbours<-neighbour.list(i,j,grid.size)
      num.neigh<-nrow(neighbours)
      flies<-data[i,j]
      tab<-table(sample(1:num.neigh,flies,replace=TRUE))[as.character(1:num.neigh)]
      tab<-ifelse(is.na(tab),0,tab)
      new.data[as.matrix(neighbours)]<-new.data[as.matrix(neighbours)]+tab
    }
  }
  new.data
}

sim.one.run<-function(jumps){
  data<-matrix(1,ncol=grid.size,nrow=grid.size)
  for (i in 1:jumps)  data<-sim.one.step(data)
  length(which(data==0))
}

grid.size<-4
system.time(sim.one.run(5))

system.time(s<-mclapply(1:100000,function(x)sim.one.run(5),mc.cores=12))
sprintf('%.6f',round(mean(unlist(s)),6))
# 5.595810
barplot(table(unlist(s)))
36/16

grid.size<-6
jumps<-50
fly<-list()
a<-Sys.time()
for(i in 1:(grid.size/2)) { # Makes it only valid for even grid sizes.
  for(j in 1:(grid.size/2)) {
    data<-matrix(0,ncol=grid.size,nrow=grid.size)
    data[i,j]<-1 # One fly test.
    fly[[length(fly)+1]]<-one.run(data,jumps)
  }
}
b<-Sys.time()
b-a
all.rotations<-unlist(lapply(fly,rotations),recursive=FALSE)
r<-Reduce(`*`,lapply(all.rotations,function(x) 1-x))
sprintf('%.6f',round(sum(r),6))


grid.size<-8
data<-matrix(0,ncol=grid.size,nrow=grid.size)
i<-1
j<-1
data[i,j]<-1 # One fly test.
system.time(test<-one.run(data,jumps=50))
test
