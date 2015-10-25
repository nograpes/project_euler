# The cleverest!
neighbour.list<-function(x,y,grid.size) {
  slides<-c(1,-1,0,0)
  moves<-data.frame(x=slides,y=rev(slides))
  new.pos<-data.frame(x=x+moves$x,y=y+moves$y)
  with(new.pos,new.pos[x>0 & y>0 & y<=grid.size & x<=grid.size,])
}

i.and.j<-function(i,j,grid.size) ((i-1) * grid.size)+ j

# mat.pow<-function(trans.mat,pow)Reduce(`%*%`,lapply(1:pow,function(x)trans.mat))

# Init prob mat.
grid.size<-30
jumps<-50
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
library(expm)
system.time(after.jumps<-trans.mat %^% jumps)
# system.time(after.jumps2<-mat.pow(trans.mat,jumps))
# all.equal(after.jumps2==after.jumps)
all<-diag(grid.size^2)
all.probs<-all %*% after.jumps
not.probs<-1-all.probs
sprintf('%.6f',sum(apply(not.probs,2,prod)))
# End cleverest.

library(parallel)
neighbour.list<-function(x,y,grid.size) {
  slides<-c(1,-1,0,0)
  moves<-data.frame(x=slides,y=rev(slides))
  new.pos<-data.frame(x=x+moves$x,y=y+moves$y)
  with(new.pos,new.pos[x>0 & y>0 & y<=grid.size & x<=grid.size,])
}

# Since each fly has it's own trajectory, independent of all others, model each one on it's own!
one.step<-function(data){
  new.data<-matrix(0,ncol=grid.size,nrow=grid.size)
  for(i in 1:grid.size){
    for (j in 1:grid.size){
      current<-data[i,j]
      if(current>0){
        neighbours<-neighbour.list(i,j,grid.size)
        num.neigh<-nrow(neighbours)
        update.prob<-current/num.neigh
        new.data[as.matrix(neighbours)]<-new.data[as.matrix(neighbours)]+update.prob
      }
    }
  }
  new.data
}

one.run<-function(data,jumps){
  for (i in 1:jumps)  data<-one.step(data)
  data
}

rotate <- function(x) t(apply(x, 2, rev))
rotations<-function(x){
  l<-list()
  l[[1]]<-x
  l[[2]]<-rotate(x)
  l[[3]]<-rotate(l[[2]])
  l[[4]]<-rotate(l[[3]])
  l
}

# Inititalize single fly list
grid.size<-30
jumps<-50
fly<-list()
for(i in 1:(grid.size/2)) { # Makes it only valid for even grid sizes.
  for(j in 1:(grid.size/2)) {
    data<-matrix(0,ncol=grid.size,nrow=grid.size)
    data[i,j]<-1 # One fly test.
    fly[[length(fly)+1]]<-data
  }
}
system.time(after.jumps<-mclapply(fly,one.run,jumps,mc.cores=12))
all.rotations<-unlist(lapply(after.jumps,rotations),recursive=FALSE)
r<-Reduce(`*`,lapply(all.rotations,function(x) 1-x))
sprintf('%.6f',round(sum(r),6))