# Weird, and hard.
# A right angle triangle with integer sides is called a Pythagorean triple.
# For any Pythagorean triple, you can get another triple by multiplying all sides by any integer.
# Any Pythagorean triple which is the "root", that is, where all its sides cannot be divided evenly by another triple is called a "primitive triple"
# So, given all "primitive triples" below length n, you can generate all triples below length n.
# A set (a,b,c) is a primitive triple iff there is an integer pair m and n such that
# m>n
# m and n are coprime
# m and n are opposite parity (oddness, eveness)
# a = m^2 - n^2, b=2mn, c=m^2+n^2

# So now the problem is how to generate all possible coprime pairs.
# Magically, all coprime numbers can be arranged in a pair of two disjoint ternary trees, amazingly, one when the pairs are opposite parity, and the other for odd-odd pairs! So, I just followed out the tree:
# Root: c(m=2,n=1)
# Branch 1: (2m-n,m)
# Branch 2: (2m+n,m)
# Branch 3: (m+2n,n)

# Bizarrely, I think that the result above is derived from the triangles, not the other way around.
# I suspect that Euler's totient function, which calculates all the *number* of coprimes less than n, is useful here, but then how would I check for parity and length?

expand.leaf<-function(x) {
  m<-x[1]
  n<-x[2]
  list( c((2*m) - n,m),c((2*m) + n, m),c(m + (2*n),n))
}

expand.tree<-function(tree) 
  unlist(lapply(tree,expand.leaf),recursive=FALSE)

size<-function(x){
  m<-x[1]
  n<-x[2]
  2*m*(m+n)
}

f<-function(x){
  m<-x[1]
  n<-x[2]
  c(m^2-n^2,2*m*n,m^2+n^2)
}

all.triples<-function(primitive) floor(max.size / sum(primitive))

all.sizes<-function(primitive) sum(primitive) * (1:floor(max.size / sum(primitive)))

tree<-list(c(2,1))
max.size<-1.5e6
stored.sizes<-rep(0,max.size)
while(length(tree)>0){
  sizes<-unlist(lapply(lapply(tree,f),all.sizes))
  table.sizes<-table(sizes)
  idx<-as.integer(names(table.sizes))
  stored.sizes[idx]<-stored.sizes[idx]+table.sizes
  tree<-expand.tree(tree)
  tree<-tree[sapply(tree,size) <= max.size]
}
unname(table(stored.sizes)['1'])
