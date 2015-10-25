indices<-cumsum((1:6) * (9*10^(0:5)))

x<-190

f<-function(x){
  chars<-findInterval(x,indices)+1
  if(x%in%indices) chars<-chars-1
  base<-indices[chars-1]
  number<-ceiling((x-base) / chars) -1
  offset<-((x-base-1) %% chars)+1
 
  if (offset==0) offset<-chars
  first.num<-10^(chars-1)
  strsplit(as.character(first.num+number),'')[[1]][offset]
}


prod(as.integer(sapply(10^(1:6),f)))




nchar(paste(1:99,collapse=''))


