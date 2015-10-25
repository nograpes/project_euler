# Problem 42
# Trivial?
map<-c()
map[LETTERS]<-1:26
word.value<-function(x) sapply(lapply(strsplit(x,''),function(y)map[y]),sum)

words<-scan('http://projecteuler.net/project/words.txt',what='character',sep=',',quote='"')
sequence<-function(n)(n*(n+1))/2
matches<-sequence(1:100)

length(which(word.value(words) %in% matches))


