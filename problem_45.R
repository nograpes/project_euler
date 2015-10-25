pentagonal<-function(x) pos.roots(3,-1,-2*x)

pos.roots<-function(a,b,c) {
  disc<-b^2 - (4*a*c)
  if (disc<0) return(FALSE) # Only imaginary roots
  all.roots<-(-b + (c(-1,1)*sqrt(disc)))/(2*a)
  any(all.roots %% 1 ==0 & (all.roots>0))
}

sapply(c(1,4,5,12),pentagonal)

hex<-function(n) n*(2*n -1) 

i<-144
while(!pentagonal(hex(i))) i<-i+1 # All hexagonal numbers are triangle numbers.
hex(i)





factorials