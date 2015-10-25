compare <- function(i, j) {
  print(compose(i)[[1]])
  print(compose(i)[[1]] + compose(i)[[2]])
  print(compose(j)[[1]])
  print(compose(j)[[1]] + compose(j)[[2]] )
}

# Makes a little matrix of the two cuboids.
starts.and.ends <- function(cuboid1, cuboid2)
  rbind(cuboid1[[1]], cuboid1[[1]] + cuboid1[[2]],
        cuboid2[[1]], cuboid2[[1]] + cuboid2[[2]])

# Takes exactly four integers, calculates volume difference.
dim.overlap.volume <- function(points) 
  diff(sort(points)[2:3])

# Doesn't verify if cubes overlap.
overlap.volume <- function(cuboid1, cuboid2) 
  prod(apply(starts.and.ends(cuboid1, cuboid2), 2, dim.overlap.volume))

# When the two cuboids "touch" (but don't overlap) the behaviour
# is ill-defined. It doesn't matter, because the overlap volume will 
# be exactly zero.
dim.overlaps <- function(points) 
  as.logical(diff(c(1,1,2,2) [order(points)] [1:2]))

overlaps <- function(cuboid1, cuboid2) 
  all(apply(starts.and.ends(cuboid1, cuboid2), 2, dim.overlaps))
  

# set.overlaps <- function(cuboid, set) 
#   which(sapply(set, overlaps, cuboid))
# 
# overlaps <- function(cuboid1, cuboid2) 
#   contains(cuboid1, cuboid2[[1]]) |
#   contains(cuboid1, cuboid2[[1]] + cuboid2[[2]])
# 
# contains <- function(cuboid, point) 
#   all(point >= cuboid[[1]]) & 
#   all(point <= cuboid[[1]] + cuboid[[2]])

new.S <- integer(8077)
S2 <- function(k) {
  if(new.S[k]!=0) return(new.S[k])
  if   (k<= 55) new.S[k]<<-(100003 - (200003*k) + (300007*(k^3))) %% 1000000
  else new.S[k]<<-(S2(k-24) + S2(k-55)) %% 1000000
  new.S[k]
}

S <- function(k) {
  if(new.S[k]!=0) return(new.S[k])
  if   (k<= 55) (100003 - (200003*k) + (300007*(k^3))) %% 1000000
  else (S(k-24) + S(k-55)) %% 1000000
  }

C <- function(n) {
  list  (sapply(6*n-(5:3), S) %% 10000,
    1 + (sapply(6*n-(2:0), S) %% 399))
}

compose <- function(i) list(x.y.z[,i],delta[,i])

# The third dimension of 44 must cross 1000

# x-order
crosses <- function(dim) {
  ord <- order(x.y.z[dim,])
  x.y.z.dim <- x.y.z[dim, ord]
  delta.dim <- delta[dim, ord]
  cross <- findInterval(x.y.z.dim + delta.dim, x.y.z.dim)
  f <- function(x) ord[x:cross[x]][-1]
  s <- sapply(1:n, f)
  s <- s[match(seq.int(n), ord)]
  rev.s <- split(rep(1:n,sapply(s, length)), unlist(s)) [as.character(1:n)]
  lapply(mapply(c, s, rev.s), unique)
}

intersect.all <- function(...) Reduce(intersect, list(...))
overlap.vol <- function(i, j) overlap.volume(compose(i), compose(j))
total.vol <- function(i, set) sum(unlist(lapply(set, overlap.vol, i)))

# Generate all 300000 S
n <- 1500
all.S <- integer(n * 6)
all.S[1:55] <- sapply(1:55, S)

for (k in 56:(n*6)) 
  all.S[k] <- (all.S[k-24] + all.S[k-55]) %% 1000000

x.y.z <- matrix(all.S[rep(6 * (0:(n-1)), each=3) + rep(1:3,n) ] %% 10000, nrow=3)
delta <- matrix(1+(all.S[rep(6 * (0:(n-1)), each=3) + rep(4:6,n) ] %% 399), nrow=3)


# Faster way
crossed <- lapply(seq(nrow(x.y.z)), crosses)
intersections <- do.call(mapply, c(FUN = intersect.all, crossed))
one.way.intersections <- 
  lapply(seq_along(intersections), 
       function(x) intersections[[x]] [intersections[[x]] > x])

overlap.total <- sum(mapply(total.vol, seq.int(n), one.way.intersections))
sum(apply(delta, 2, prod)) - overlap.total


# Slow way
overlap.total <- 0
track <- list()
for (i in seq(n-1)) {
  print(i)
  cuboid1 <- compose(i)
  for (j in (i+1):n) {
    cuboid2 <- compose(j)
    if (overlaps(cuboid1, cuboid2)) {
      overlap.total <- overlap.total + 
        overlap.volume(cuboid1, cuboid2)
      track <- c(track, list(c(i,j)))
    }
  }
}
old.overlap.total <- overlap.total
sum(apply(delta, 2, prod)) - old.overlap.total

track2 = track
track2 = lapply(track2, sort)
track2 = lapply(track2, rev)
track2 = track2 [order(sapply(track2,'[',1))]

compare(1073, 705)

# 1000
# 723581599
# 50000
# 319911885737

# 328968937309

Sum of cuboids: 726218359
# Answer: 723581599
726218359 - 723581599
# overlap 2636760