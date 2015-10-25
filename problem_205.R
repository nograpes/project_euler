# Problem 205
dice.probs<-function(num,sides)
  table(rowSums(do.call(expand.grid,lapply(1:num,function(x)1:sides)))) / (sides^num)

# Cubic Colin
colin<-dice.probs(6,6)
# Pyramidal Pete
pete<-dice.probs(9,4)

a<-rev(cumsum(rev(pete[-1])))
b<-colin[as.character(9:35)]

sum(a*b)+sum(colin[as.character(6:8)])

7*2^17


set.seed(1)
x1=rnorm(100,0,1)
x2=rnorm(100,1,1)
# Find points where x1 is above x2.
above<-x1>x2
# Points always intersect when above=TRUE, then FALSE or reverse
intersect.points<-which(diff(above)!=0)
# Find the slopes for each line segment.
x1.slopes<-x1[intersect.points+1]-x1[intersect.points]
x2.slopes<-x2[intersect.points+1]-x2[intersect.points]
# Find the intersection for each segment.
x.points<-intersect.points + ((x2[intersect.points] - x1[intersect.points]) / (x1.slopes-x2.slopes))
y.points<-x1[intersect.points] + (x1.slopes*(x.points-intersect.points))
# Plot.
plot(x1,type='l')
lines(x2,type='l',col='red')
points(x.points,y.points,col='blue')

x1.slopes[1] +x1[intersect.points[1]]


x1.slopes[1](x)-x2.slopes[1](x)=x2[intersect.points[1]]-x1[intersect.points[1]]



(x1.slopes[1]*goof) + x1[intersect.points[1]]

goof<-x2[2]-x1[2] / (x1.slopes[1]-x2.slopes[1])

x2.slopes


plot(x1[1:5],type='l')
lines(x2[1:5],type='l',col='red')


ggplot(data.frame(x1,x2),aes(colour=))