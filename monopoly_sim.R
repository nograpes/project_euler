# Monopoly sim.
library(gdata)
pos.names<-tolower(c('go','a1','cc1','a2','t1','R1','B1','CH1','B2','B3','JAIL',
'c1','u1','c2','c3','r2','d1','cc2','d2','d3','fp','e1','ch2','e2','e3',
'r3','f1','f2','u2','f3','g2j','g1','g2','cc3','g3','r4','ch3','h1','t2','h2'))

die.roll<-function(sides=4) sample(1:sides,2,replace=TRUE)

advance<-function(current.pos,dice){
  (current.pos + sum(dice)) %% 40
}

advance.to.next<-function(start.char,current.pos) {
  positions.of.char<-which(startsWith(pos.names,start.char))-1
  pos<-findInterval(current.pos,positions.of.char)+1
  if(pos>length(positions.of.char)) return(positions.of.char[1])
  positions.of.char[pos]
}

get.pos.by.name<-function(name) which(name==pos.names)-1
get.name.by.pos<-function(pos) pos.names[pos+1]
is.chance<-function(pos) startsWith(get.name.by.pos(pos),'ch')
is.chest<-function(pos) startsWith(get.name.by.pos(pos),'cc')
is.doubles<-function(dice) dice[1]==dice[2]

cc.orig.deck<-c('go','jail',rep('nothing',14))
ch.orig.deck<-c('go','jail','c1','e3','h2','r1',rep('next_r',2),'next_u','back_3',rep('nothing',5))

shuffle.deck<-function(deck) deck[order(rnorm(length(deck)))]

# returns position after draw.
draw.cc<-function(current.pos) {
  draw<-cc.deck[1]
  cc.deck<<-c(cc.deck[-1],cc.deck[1])
  if(draw=='nothing') return(current.pos)
  get.pos.by.name(draw)
}

draw.ch<-function(current.pos){
  draw<-ch.deck[1]
  ch.deck<<-c(ch.deck[-1],ch.deck[1])
  if(draw=='nothing') return(current.pos) 
  if(draw=='back_3') return(current.pos-3) # back before 0 not possible.
  if(startsWith(draw,'next_')) return(advance.to.next(strsplit(draw,'_')[[1]][2],current.pos))
  get.pos.by.name(draw)
}

# Check for chance or cc
check.for.ch.or.cc<-function(current.pos){
  if(is.chest(current.pos)) {
    return(draw.cc(current.pos))
  } else {
    if(is.chance(current.pos)) {
      new.pos<-draw.ch(current.pos)
      if(new.pos!=current.pos) new.pos<-return(check.for.ch.or.cc(new.pos)) # Chance to by 'back 3' CC scenario.
      return(new.pos)
    }
  }
  current.pos
}

pos.tracker<-c()  
pos.tracker[pos.names]<-0
current.doubles<-0
current.pos<-0
set.seed(1)
cc.deck<-shuffle.deck(cc.orig.deck)
ch.deck<-shuffle.deck(ch.orig.deck)
for (i in 1:100000) {
  # print(current.pos)
  dice<-die.roll()
  # print(dice)
  # get.name.by.pos(current.pos)
  # Doubles go to jail.
  if(is.doubles(dice) & current.doubles==2) {
    current.doubles<-0
    current.pos<-get.pos.by.name('jail')
  } else { # Normal move.
    if (is.doubles(dice)) {
      current.doubles<-current.doubles+1
    } else {
      current.doubles<-0
    }
    current.pos<-advance(current.pos,dice)
    current.pos<-check.for.ch.or.cc(current.pos)
    if(get.name.by.pos(current.pos)=='g2j') current.pos<-get.pos.by.name('jail')
  }
  x<-get.name.by.pos(current.pos)
  # print(x)
  pos.tracker[[x]]<-pos.tracker[[x]]+1
}

test<-pos.tracker/sum(pos.tracker)

head(rev(sort(test)),3)

dice<-c(1,1)
advance(37,dice)


# Number chain.
next.num<-function(x) sum(as.numeric(strsplit(as.character(x),'')[[1]])^2)

record<-rep(NA,1e7)
record[1]<-1
record[89]<-89

f<-function(num){
  if(record[num] %in% c(1,89)) return(c(num,record[num]))
  return(c(num,f(next.num(num))))
}

for (i in seq_along(record)) {
  if (i %% 10000 == 0) print(i)
  if (is.na(record[i])) {
    set<-f(i)
    record[set[-length(set)]]<-set[length(set)]
  }
}
table(record)

f(3299999)

