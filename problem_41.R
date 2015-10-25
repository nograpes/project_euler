
gen.combos<-function(x) {
  if (length(x)==1) return(x)
  c(sapply(x,function(y) paste0(y,gen.combos(x[x!=y]))))
}

# If the digits of a number are divisible by 3
# Then the number is divisible by 3
# All 9- and 8-pandigital numbers will thus be divisible by 3
# Because sum(1:9) and sum(1:8) are divisible by 3.
all.pandigital<-gen.combos(as.character(7:1))
all.pandigital<-as.integer(all.pandigital)

primality.test<-function(x) all(x%% (2:ceiling(sqrt(x)))!=0)

# Apply a little filter for speed.
test<-which((all.pandigital %% 2) == 0)
all.pandigital<-all.pandigital[-test]
test<-which((all.pandigital %% 3) == 0)
all.pandigital<-all.pandigital[-test]

for(i in all.pandigital) {
  if(primality.test(i)) {
    print(i)
    break
  }
}

