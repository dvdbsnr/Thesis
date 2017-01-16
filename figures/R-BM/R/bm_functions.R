# calculates moving minimum of numeric vector
# min(s) = min(v(s'), s'<=s)
moving.minimum <- function(vector){
  minima <- rep(0, length(vector))
  for(i in seq(along=vector)){
    minima[i] <- min(vector[1:i])
  }
  return(minima)
}

# calculates moving maximum of numeric vector
# max(s) = max(v(s'), s'<=s)
moving.maximum <- function(vector){
  maxima <- rep(0, length(vector))
  for(i in seq(along=vector)){
    maxima[i] <- max(vector[1:i])
  }
  return(maxima)
}


# adds reflection to brownian motion
reflect.brownian.motion <- function(bm, positive=TRUE){
  if(positive){
    bm.ref <- bm - moving.minimum(bm)
  } else {
    bm.ref <- bm - moving.maximum(bm)
  }
  return(bm.ref)
}

# calculates lengths of excursions
get.excursion.lengths <- function(bm){
  zero.points <- which(bm == 0)
  length.steps <- diff(zero.points)
  return(length.steps)
}






