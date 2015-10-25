# Just as easy as it looks. Just remember you log identity.
df <- read.csv('http://projecteuler.net/project/resources/p099_base_exp.txt',header=FALSE)
names(df) <- c('base','exp')
x = log(df$base)*df$exp
which(max(x)==x)