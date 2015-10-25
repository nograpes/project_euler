hands<-read.table('http://projecteuler.net/project/poker.txt',header=FALSE,stringsAsFactors=FALSE)

# High Card: Highest value card.
# One Pair: Two cards of the same value.
# Two Pairs: Two different pairs.
# Three of a Kind: Three cards of the same value.
# Straight: All cards are consecutive values.
# Flush: All cards of the same suit.
# Full House: Three of a kind and a pair.
# Four of a Kind: Four cards of the same value.
# Straight Flush: All cards are consecutive values of same suit.
# Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.

# 2, 3, 4, 5, 6, 7, 8, 9, T, J, Q, K, A.
val.map<-sprintf('%02.0f',1:13)
names(val.map)<-c(as.character(2:9),'T','J','Q','K','A')
# table(values(unlist(hands)))

suits<-function(hand) sapply(strsplit(hand,''),`[`,2)
values<-function(hand) sapply(strsplit(hand,''),`[`,1)
values.mapped<-function(hand) val.map[values(hand)]
is.straight<-function(hand) all( diff(as.numeric(sort(values.mapped(hand))))==1 )
is.flush<-function(hand) length(unique(suits(hand)))==1

col<-function(x)as.numeric(paste0(x,collapse=''))

hand.val<-function(hand) {
  hand<-unlist(hand)
  d<-data.frame(table(values.mapped(hand)))
  d2<-d[rev(order(d[,2],d[,1])),]
  tab<-d2$Freq
  names(tab)<-d2[,1]
  if(is.straight(hand) & is.flush(hand)) return(col(c('9',names(tab)[1],rep('00',4)))) # Straight Flush
  if(tab[1]==4) return(col(c('8',names(tab)[1:2],rep('00',3)))) # Four of a kind.
  if(tab[1]==3 & tab[2]==2) return(col(c('7',names(tab)[1],rep('00',4))))  # Full house.
  if(!is.straight(hand) & is.flush(hand)) return(col(c('6',names(tab)[1:5]))) # Flush
  if(is.straight(hand) & !is.flush(hand)) return(col(c('5',names(tab)[1:5]))) # Straight
  if(tab[1]==3 & tab[2]==1) return(col(c('4',names(tab)[1:3],'00','00')))# Three of a kind.
  if(tab[1]==2 & tab[2]==2) return(col(c('3',names(tab)[1:3],rep('00',2)))) # Two pair
  if(tab[1]==2 & tab[2]==1) return(col(c('2',names(tab)[1:4],'00'))) # Two of a kind.
  if(tab[1]==1) return(col(c('1',names(tab))))# High Card.
}

player.one<-hands[1:5]
player.two<-hands[-1:-5]

player.one.scores<-apply(player.one,1,hand.val)
player.two.scores<-apply(player.two,1,hand.val)
length(which(player.one.scores>player.two.scores))

