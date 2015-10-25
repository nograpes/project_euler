# Question 28
sides=seq(3,1001,by=2)
square.length<-sides*4-4
top.right<-cumsum(c(1,square.length))[-1]
top.left<-top.right-(sides-1)
bottom.left<-top.left-(sides-1)
bottom.right<-bottom.left-(sides-1)

sum(top.right,top.left,bottom.right,bottom.left,1)

