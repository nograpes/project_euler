l<-log(1e6/7,base=2)
base<-2^floor(l)

denom<-base*7 # 917,504
num<-base*3 # 393,216

f<-function(x) floor((x*num)/denom)

f(rem[3])

f(rem[7])

rem<-(denom+1):1e6
stuff<-f(rem)/rem

rem[which(stuff==max(stuff[stuff<(num/denom)]))]

999997 # denom
f(999997)

stuff[min(which())]




identical(stuff,max(stuff))
sapply(stuff,identical,max(stuff))
