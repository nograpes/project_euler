# Problem 52
f2<-function(length){
  max<-10^length
  start<-1:floor(max/6)
  end<-start*6
  f<-function(start,end){
    start<-sprintf(paste0('%0',length,'d'),start)
    end<-sprintf(paste0('%0',length,'d'),end)
    start.dig<-lapply(strsplit(start,''),sort)
    end.dig<-lapply(strsplit(end,''),sort)
    mapply(function(x,y)all(x==y),start.dig,end.dig)
  }
  filtered<-which(f(start,end))
  for (i in 2:5)
    if (length(filtered)>0) filtered<-filtered[mapply(f,filtered,filtered*i)]
  filtered
}
unlist(sapply(1:6,f2)) # After a couple of guesses I found 6.
# 142857*(1:6)

