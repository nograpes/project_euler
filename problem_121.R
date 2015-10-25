# Problem 121
library(iterpc)
n <- 15
wins <- ceiling(n/2)
floor(factorial(n+1) / 
sum(sapply(seq(wins-1), function(x) sum(apply(getall(iterpc(n, x)),1,prod))),1))

a=5000
b=c(1000, 2000, 4000)
diff=a-b


m <- matrix(c(8,3,4,1,5,9,6,7,2,1,2,3), nrow=4, ncol=3)
df <- expand.grid(x=1:ncol(m),y=1:nrow(m))
df$val <- m[as.matrix(df[c('y','x')])]
df


corpus <- c("This is the first 2.2 cm sentence 2. This is the second.",
            "document 2 only has one sentence")
corpus.sentences <- strsplit(corpus, "\\. ")
num.sentences <- sapply(corpus.sentences, length)



strsplit


?strsplit

library(ggplot2)
library(scales)

ggplot(df, aes(x=x, y=y, label=val)) + 
  geom_tile(fill='transparent', colour = 'black') + geom_text(size = 14) + scale_y_reverse() +
  theme_classic() + 
  theme(axis.text  = element_blank(),
        panel.grid = element_blank(),
        axis.line  = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

m
plot(x=NULL,xlim=c(0,2),ylim=c(0,2))
mtext()

diff/sum(diff)

# If any cakes with zero size and positive cost, return infinity.
# Remove any cakes with zero size and not positive cost.
# Sort cakes into density

# Initialize n+1 boxes, each with one cake.

# The loop.
#  Evaluate maximum density in leftover space for each box.
#  Remove any box where maximum density + current value < max(current_value) over all boxes.

# Find the box with the highest potential.
 # Within those boxes find the one with the lowest space.

# Replace the box with the n copies, where n is the # cakes.
 # Each new box is the old box plus one cake.
 # To reduce box explosion, you can only add cakes of lesser or 
 # equal density to the lowest density cake.

# box {lowest, current_value, leftover_space}

?

l = list(meuse, meuse)

slot(l[[1]], 'data')

lapply(l, slot, 'data')

lapply(l, "@", 'data')

slot

m = lapply(l, function(x) x@data)

attributes(as.data.frame(l[[1]]))

sapply(lapply(l, as.data.frame), class)

`@`(meuse,data)

data(meuse)

coordinates(meuse) <- c('x','y')

coordinates(meuse)
meuse@coords

str(meuse)

data(meuse)
xy = meuse[c("x", "y")] # retrieve coordinates as data.frame
class(meuse)
data(meuse) # reload data.frame
coordinates(meuse) = c("x", "y") # specify column names


?SpatialPointsDataFrame
class(meuse)
meuse@data

plot(meuse)
class(meuse)
xy = meuse[c("x", "y")] # retrieve coordinates as data.frame


# Maximum density function.


