# Problem 301

# Really insanely hard, but simple code
f<-function(x){
  if (x<0) return(NA)
  if (x==1) return(2)
  if (x==2) return(3)
  f(x-1)+f(x-2)
}
f(30)
# 2178309

# Yep, that's it. The Fibonacci sequence.
# Why?

# Read up on Nim. 
# It turns out that if you convert the pile sizes (a,b,c) to binary
# then if a xor b xor c == 0
# It is an automatic win.
library(bitops)
X<-function(n) bitXor(bitXor(n,2*n),3*n)

# This would be the function for x bits.
quick.check<-function(x) length(which(X(1:(2^x))==0))
quick.check(6)
# But, even checking that up to 2^30 would be long.

# If a XOR b XOR c XOR = 0,
# a XOR b == c
# We know that
# a+b == c
# We also know that since b is just 2*a, then
# b is the left-shift of bits of a.
# We also know that the + operator and XOR operator
# are really close, they are only different in the case
# of both bits 1.
# You will only add two bits one when a has 2 bits in a row that are one!
# That was my insight.

# So the only time when a XOR b is going to equal C
# is when there aren't two 1's in a row.

# So how many binary numbers from 1:2^30 have two 1's in a row?
# Think of it this way:
# You can construct the number as chunks of either:
# 10 and 0. (Unless you have one bit left)
# So, if you start with four bits:
# you can have either:
# 10 0
# 100 1010 00 010
# 1001 1000 1010 0010 000 0100 0101
# 1001 1000 1010 0010 0000 0001 0100 0101
# So it is really a function of f described above.

# I didn't figure out that it was the Fibonacci sequence until I read the comments.
# And I didn't get the XOR stuff either until I read it on Wikipedia.
# But the rest is mine.


# So, by definition, a+b=c.
# So, the XOR will equal 


