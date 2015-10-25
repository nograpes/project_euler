# Letter frequencies in English.
frequency<-c(e=12,a=9,i=9,o=8,n=6,r=6,t=6,l=4,s=4,u=4,d=4,g=3,b=2,c=2,m=2,p=2,f=2,h=2,v=2,w=2,y=2,k=1,j=1,x=1,q=1,z=1,blank=2)
# Points in English.
points<-c(rep(1,10),rep(2,2),rep(3,4),rep(4,5),rep(5,1),rep(8,2),rep(10,2),rep(0,2))
names(points)<-names(frequency)

# Create sack.
sack<-rep(names(frequency),times=frequency)
refill.sack<-function() 
  sack<<-rep(names(frequency),times=frequency)

draw<-function(n) {
  i<-sample(seq.int(sack),n,replace=FALSE)
  hand<-sack[i]
  sack<<-sack[-i] # A nasty side-effect.
  hand
}

# Scrape some letters.
urls<-paste('http://webclipart.about.com/od/hobbiesoriginal/ss/',c('Blank',LETTERS),'-Scrabble-Tile.htm',sep='')
f<-function(x) paste(scan(x,what='character',quiet=TRUE),collapse='')
l<-sapply(urls,f)
pic.urls<-gsub('.*"(http://0.tqn.com/d/webclipart/[^"]*.png)".*','\\1',l)
library(RCurl)
library(png)
tiles<-lapply(pic.urls,function(x)readPNG(getURLContent(x)))
names(tiles)<-c('blank',letters)

# Make a board
hand<-draw(9)
m<-matrix(hand,nrow=3)
library(reshape2)
library(ggplot2)
ggplot(melt(m,value.name='letter'),aes(x=Var1,y=Var2,fill=letter)) + 
  geom_tile(col='white',size=2) + element_blank()

library(ggmap)
??ggimage

data(hadley)
ggimage(hadley)
ggimage(hadley, coord_equal = FALSE)
?ggimage

x <- seq(1, 438, 15); n <- length(x)
df <- data.frame(x = x, y = -(120*(scale((x - 219)^3 - 25000*x) + rnorm(n)/2 - 3)))
qplot(x, y, data = df, geom = c('smooth','point'))
ggimage(hadley, fullpage = FALSE) +
  geom_smooth(aes(x = x, y = y), fill = I('gray60'), data = df,
              colour = I('green'), size = I(1)) +
  geom_point(aes(x = x, y = y), data = df,
             colour = I('green'), size = I(3), fill = NA)

## End(Not run)

dimnames(m)<-list('x','y')


my_image <-  readPNG(getURLContent(pic.urls[2]))

plot(1:2,type='n')
img<-my_image
transparent <- img[,,4] == 0
img <- as.raster(img[,,1:3])
img[transparent] <- NA
rasterImage(img, 1.2, 1.27, 1.8, 1.73, interpolate=FALSE)





