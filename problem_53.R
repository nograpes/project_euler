
# For even n, n choose k is symmetric, with a one hanging at the end.
# So n/2 is always the highest k for any even n
# So if i is the first k over 1e6, then total above 1e6 is n-((i*2)+1)
# choose(10,1:10)
# For odd ns it is always the middle doubled. to plus the hanging one.
# So, same: n-((i*2)+1)
# choose(1e2,4)

first.k.above<-function(n,max=1e6){
  for(i in 1:floor(n/2)) if(choose(n,i)>=max) return(n-(((i-1)*2)+1))
  0
}
sum(sapply(1:100,first.k.above))