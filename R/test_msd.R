getBreaks
msd <- function(v, k = 1, middle = FALSE, plot = TRUE){
  vec <- v[!is.na(v)]
  minVec <- min(vec)
  maxVec <- max(vec)
  avgVec <- mean(vec)
  sdVec <- sqrt(sum((vec - avgVec) ^ 2)  / length(vec))
  
  if (middle == FALSE){
    pose <- ceiling((maxVec - avgVec) / (sdVec * k))
    nege <- ceiling((avgVec - minVec) / (sdVec *k))
    
    avgVec + (1:pose) * (sdVec * k)
    bks <- c(avgVec - (1:nege) * (sdVec * k), avgVec, avgVec + (1:pose) * (sdVec * k) )
    bks <- c(minVec, bks[bks> minVec & bks <maxVec], maxVec)
  }else{
    pose <- ceiling((maxVec - (avgVec + 0.5 * sdVec * k)) / (sdVec * k))
    nege <- ceiling(((avgVec - 0.5 * sdVec * k ) - minVec) / (sdVec *k))
    
    bks <- c((avgVec -  0.5 * sdVec * k) - (1:nege) * (sdVec * k), 
             (avgVec -  0.5 * sdVec * k), 
             (avgVec +  0.5 * sdVec * k), 
             (avgVec + 0.5 * sdVec * k) + (1:pose) * (sdVec * k))
    bks <- c(minVec, bks[bks> minVec & bks <maxVec], maxVec)
  }
  
  if(plot==TRUE){
    hist(vec, 100)
    points(avgVec, 0, col = "red", pch = 20, cex = 2)
    if(middle==FALSE){
    points(avgVec + k * sdVec, 0, col = "blue", pch = 20, cex = 2)
    points(avgVec - k * sdVec, 0, col = "blue", pch = 20, cex = 2)
    }else{
      points(avgVec + k * 0.5 * sdVec, 0, col = "blue", pch = 20, cex = 2)
      points(avgVec - k * 0.5 * sdVec, 0, col = "blue", pch = 20, cex = 2)
    }
    points(bks, rep(0, length(bks)), col = "green", pch = 20, cex = 1)
  }
  
  return(bks)
}

# vec <- (nuts3.df$birth_2008 - nuts3.df$death_2008) / nuts3.df$birth_2008
# vec <- nuts3.df$gdppps2008 / nuts3.df$pop2008
# msd(v = vec, k = 1, middle = F, plot = T)
# vec <- vec[vec>(-1.5)]
# max(vec, na.rm=T)
# vec <- (nuts3.df$birth_2008 - nuts3.df$death_2008) / nuts3.df$birth_2008




