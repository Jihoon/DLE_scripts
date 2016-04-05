# Approach A
a <- vector()
b <- vector()

for (i in 1:10000) {
  lim <- runif(1)
  a[i] <- runif(1, 0, lim)
  b[i] <- lim - a[i]
}

hist(a, 100)
hist(b, 100)

# Approach B
a <- vector()
b <- vector()
c <- vector()

for (i in 1:5000) {
  a[i] <- runif(1)
  b[i] <- runif(1)
  c[i] <- runif(1)
  s <- (a[i]+b[i]+c[i])
  a[i] <- a[i]/s
  b[i] <- b[i]/s
  c[i] <- c[i]/s
}

hist(a, 100)
hist(b, 100)
hist(c, 100)

d <- a+b+c
plot3d(a, b, c, size=5)
rgl.snapshot("H:/MyDocuments/IO work/Uncertainty/Draw test/draw_test.png", "png")


# Approach C
a <- vector()
b <- vector()
c <- vector()

for (i in 1:50000) {
  a[i] <- runif(1)
  b[i] <- runif(1)
  s <- a[i]+b[i]
  if (s>1) next
  c[i] <- 1-s
}

ind <- !is.na(c)

hist(a[ind], 100)
hist(b[ind], 100)
hist(c[ind], 100)
plot3d(a[ind], b[ind], c[ind], size=5)
rgl.snapshot("H:/MyDocuments/IO work/Uncertainty/Draw test/draw_test2.png", "png")


# Approach D with restriction
a <- vector()
b <- vector()
c <- vector()

for (i in 1:5000) {
  a[i] <- runif(1, 0, 0.3)
  b[i] <- runif(1, 0, 0.5)
  s <- a[i]+b[i]
  if (s>1) next
  c[i] <- 1-s
}

ind <- !is.na(c)

hist(a[ind], 100)
hist(b[ind], 100)
hist(c[ind], 100)
plot3d(a[ind], b[ind], c[ind], size=5)
rgl.snapshot("H:/MyDocuments/IO work/Uncertainty/Draw test/draw_test2.png", "png")

# Using Dirichlet
a <- rdirichlet(50000, rep(1, 3))
b <- vector()
for (i in 1:50000) {
  b <- rbind(b, rdirichlet(1, rep(1, 3)))
}
hist(a[,1], 100)
hist(a[,2], 100)
hist(a[,3], 100)
plot3d(a[,1], a[,2], a[,3], size=5)
hist(b[,1], 100)
hist(b[,2], 100)
hist(b[,3], 100)
plot3d(b[,1], b[,2], b[,3], size=5)


library(gtools)

# Approach #0.
# Use RandVec
# Still too slow for small ranges

get_draws = function(n_draw, n_dim=3, min, max) {
  # alpha = 1
  if(missing(min)) {min = rep(0, n_dim)}
  if(missing(max)) {max = rep(1, n_dim)}
  
  # draws <- rdirichlet(n_draw,rep(1,n_dim))
  dr <- RandVec(0, 1, 1, n_dim, n_draw*100)    # Uniform draw
  draws <- dr$RandVecOutput
  #   if(check_draws(n_dim=3, min, max)) 
  #     return(draws)
  
  min_ok <- apply(draws, 2, function(x) {all(x >= min)})
  max_ok <- apply(draws, 2, function(x) {all(x <= max)})
  oks <- (min_ok & max_ok)
  if (sum(oks)>=n_draw) return(draws[,oks][,1:n_draw])
  draws <- draws[,oks]
  
  for (i in 1:(n_draw-sum(oks))) {
    dr <- RandVec(0, 1, 1, n_dim, 1)
    draw <- dr$RandVecOutput
    while(!all(draw >= min) | !all(draw <= max)) {
      dr <- RandVec(0, 1, 1, n_dim, 1)
      draw <- dr$RandVecOutput
    }
    draws <- cbind(draws, draw)
  }
  
  return(t(draws))
}

# Approach #1.
# Draw a lot from no-constraint simplex (every coordinate between [0,1])
# And reject draws not meeting given contraints
# Issue: It takes too long if the contraint ranges get very small.

get_draws = function(n_draw, n_dim=3, min, max) {
  # alpha = 1
  if(missing(min)) {min = rep(0, n_dim)}
  if(missing(max)) {max = rep(1, n_dim)}
  
  draws <- rdirichlet(n_draw,rep(1,n_dim))
  
#   if(check_draws(n_dim=3, min, max)) 
#     return(draws)
  
  min_ok <- apply(draws, 1, function(x) {all(x >= min)})
  max_ok <- apply(draws, 1, function(x) {all(x <= max)})
  oks <- (min_ok & max_ok)
  if (sum(oks)==n_draw) return(draws)
  draws <- draws[oks,]
  
  for (i in 1:(n_draw-sum(oks))) {
    draw <- rdirichlet(1, rep(1,n_dim))
    while(!all(draw >= min) | !all(draw <= max))
      draw <- rdirichlet(1, rep(1,n_dim))
    draws <- rbind(draws, draw)
  }

  return(draws)
}

# Approach #2
# Get intersections first and do Dirichlet directly with those points.
# This is not quite correct since Dirichlet is for three axis.

get_draws = function(n_draw, n_dim=3, min, max) {
  # alpha = 1
  if(missing(min)) {min = rep(0, n_dim)}
  if(missing(max)) {max = rep(1, n_dim)}

  range <- rbind(min, max)
  inters <- vector()
  
  for (i in 1:n_dim) {
    intersection<- matrix(nrow=2^(n_dim-1), ncol=n_dim)
    
    axis <- range[,-i]   
    b <- apply(axis, 2, as.list)
    planes <- expand.grid(b)
    planes <- matrix(unlist(planes), nrow=2^(n_dim-1))
    coord <- 1.0 - rowSums(planes)
    
    intersection[,i] <- coord
    intersection[,-i] <- planes
    
    # I couldn't use simple >= or <= because of floating point number resolutions. (e.g. 0.8 - 1 + 0.2 != 0)
    legit_coord <- element.greater.equal(coord, range[1,i]) & element.smaller.equal(coord, range[2,i])
    inters <- rbind(inters, intersection[legit_coord,])
  }
  
  inters <- unique(inters)
  draw_standard <- rdirichlet(n_draw, rep(1, dim(inters)[1]))
  draw_projected <- draw_standard %*% inters
  
  plot3d(draw_projected[,1], draw_projected[,2], draw_projected[,3], size=4)
}

element.greater.equal <- Vectorize(function(x, y) {isTRUE(all.equal(x, y)) | x>y})
element.smaller.equal <- Vectorize(function(x, y) {isTRUE(all.equal(x, y)) | x<y})

# Approach #3
# Just do Dirichlet with three points in n-dim space
# Not sure whether this is correct

get_draws = function(n_draw, n_dim=3, coords) {
  # alpha = 1
  if(missing(coords)) {coords <- diag(n_dim)} # Coords can be n_dim X k (any k)
  
  draw_standard <- rdirichlet(n_draw, rep(1, dim(coords)[2]))  # n_draw X k
  draw_projected <- draw_standard %*% t(as.matrix(coords))  # n_draw X n_dim
  
  return(draw_projected)
  
  # plot3d(draw_projected[,1], draw_projected[,2], draw_projected[,3], size=4)
}


b<-get_draws(10000, 3)
b<-get_draws(100000, 3, min=c(0.3, 0, 0.1), max=c(0.8, 1, 1))
hist(b[,1], 100)
hist(b[,2], 100)
hist(b[,3], 100)
plot3d(b[,1], b[,2], b[,3], size=5)

# check_draws = function(n_draw, min, max) {
#   min_ok <- apply(draws, 1, function(x) {all(x >= min)})
#   max_ok <- apply(draws, 1, function(x) {all(x <= max)})
#   oks <- sum(min_ok & max_ok)
#   return(oks==n_draw) 
# }
  