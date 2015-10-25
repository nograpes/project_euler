# This one was really hard, but only because I tried to solve it in a stupid way.
# I went all over hte place, but eventually I came up with this simple equation 
# describing the system.
# b = total a= blue discs
(a)*(a-1) / (b)*(b-1) = 1/2
b^2 - b - 2a^2 + 2a = 0
# But of course I only want the integer roots. 
# Which makes this a Diophantine equation. 
# What I didn't know was that there is an algorithm to solve these,
# so that you end up with a generating function.
# I still haven't solved how it works, but there is a calculator online:
# http://www.alpertron.com.ar/QUAD.HTM

# The generating equation looks like this:
gen.next <- function(pair) {
  P = 3
  Q = 4
  K = -3
  R = 2
  S = 3
  L = -2
  x = pair['x']
  y = pair['y']
  pair = c((P*x) + (Q*y) + K,(R*x) + (S*y) + L)
  names(pair) = c('x','y')
  pair
}

options(scipen=9999)
pair = c(x=0,y=1)
while(pair['x']<1e12) {
  pair = gen.next(pair)
  print(pair)
}

# It feels cheap, but it isn't. I proved many "other" results about this:
# My major first shot was:
# when 8a^2 + 8a + 1 is a perfect square, then (a+1, (((((8*a*(a+1))+1)**0.5)-1)/2)+1)
# is a solution.
# Which is true, but of no real help. I eventually tried to solve when exactly 
# the equation would yield a perfect square, and I went down that road for a while,
# and finally actually did solve it (bv using the same Diophantine solver) just for a 
# different equation.
