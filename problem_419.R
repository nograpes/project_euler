next.val<-function(x){
  y<-rle(strsplit(x,'')[[1]])[c('lengths','values')]
  y$lengths<-as.character(y$lengths)
  paste0(apply(t(data.frame(y)),2,paste0,collapse=''),collapse='')
}

all.val<-function(x,val)
  unname(table(strsplit(x,'')[[1]])[val])

A<-function(x)all.val(x,'1')
B<-function(x)all.val(x,'2')
C<-function(x)all.val(x,'3')

answer.vec<-function(x) {
  x<-c(A(x),B(x),C(x))
  ifelse(is.na(x),0,x)
}
answer<-function(x) paste(A(x),B(x),C(x),sep=',')

get.elem<-function(x,val='1'){
  for(i in seq_len(x-1)) val<-next.val(val)
  val
}

num.threes<-function(x) length(which(strsplit(x,'')[[1]]=='3'))

tokenize<-function(x) {
  tokens<-strsplit(x,'3')[[1]]
  paste0(tokens,c(rep(3,num.threes(x)),rep('',length(tokens)-num.threes(x))))
}

num.double.threes<-function(x) sapply(gregexpr('33',x),function(x) length(which(x!=-1)))
tokenize.double.threes<-function(x) {
  tokens<-strsplit(x,'33')[[1]]
  paste0(tokens,c(rep(33,num.double.threes(x)),rep('',length(tokens)-num.double.threes(x))))
}


first<-'1'
transitions<-list()
result<-list()
add<-function(x){
    if(!(x %in% names(transitions))){
      threes<-num.double.threes(x)
      new.x<-x
      i<-0
      while(num.double.threes(new.x)==threes) {
        i<-i+1
        new.x<-next.val(new.x)
      }
      transitions[[x]]<<-i
      result[[x]]<<-tokenize.double.threes(new.x)
      for(i in result[[x]]) add(i)
  }
}
for (i in tokenize.double.threes(first)) add(i)

threes<-num.double.threes(x1)


add(x1)
result


# For each, make a named vector
expand.result<-function(x){
  x1<-x
  l<-list()
  for (i in seq_len(transitions[[x1]]-1)) {
    l[[x1]]<-next.val(x1)
    x1<-l[[x1]]
  }
  l[[x1]]<-result[[x]]
  l
}



expanded.results<-unlist(lapply(names(result),expand.result),recursive=FALSE)
all.states<-sort(names(expanded.results))
expanded.results<-expanded.results[all.states]
big.m<-do.call(rbind,lapply(lapply(expanded.results, `%in%`, x=all.states),as.numeric))

rownames(big.m)<-all.states
colnames(big.m)<-all.states

init<-t(c(1,rep(0,length(all.states)-1)))

all.three<-function(x) c(`1`=A(x),`2`=B(x),`3`=C(x))

vals<-t(sapply(all.states,all.three))
vals<-ifelse(is.na(vals),0,vals)

# Answer
n<-18
init %*% Reduce(`%*%`,replicate(n-1,list(big.m))) %*% vals
all.three(get.elem(n))



x<-tokenize.double.threes(get.elem(18))
x1=x[[1]]
num.double.threes(expanded.results[x[[1]]])

next.val(x[[1]])

result[x[[1]]]
transitions[x[[1]]]

tokenize.double.threes(get.elem(19))




