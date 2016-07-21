# # 
# 
# vec <- nuts3.df$birth_2008
# 
# vec[1:10] <- NA
# nbcl <- 6
# 
# msd(vec, nbcl = 6)
# 
# 
# 
# 
# msd <- function(vec, nbcl){
#   if(length(vec) < nbcl) {
#     stop("Vector is to short.")
#   } else {
#     vec <- vec[!is.na(vec)]
#     
#     minVec <- min(vec, na.rm = TRUE)
#     maxVec <- max(vec, na.rm = TRUE)
#     avgVec <- mean(vec, na.rm = TRUE)
#     sdVec <- sqrt(sum((vec - avgVec) ^ 2)  / length(vec[!is.na(vec)])) 
# 
#     
#     nbcl <- 7
#     # écart-type population
#     evenClass <- nbcl %% 2 == 0
#     
#     
#     nbSd <- floor((nbcl - 1) / 2)
#     
# 
# 
#     if (evenClass){
#       seqNbSd <- seq(-nbSd, nbSd, by = 1)
#       seqSd <- seqNbSd[seqNbSd != 0] * sdVec
#         bks <- c(minVec, avgVec + seqSd[1:nbSd], avgVec, 
#         avgVec + seqSd[(nbSd+1):length(seqSd)], 
#         maxVec)
#     }else{
#       seqNbSd <- seq(-nbSd-0.5, nbSd+0.5, by = 1)
#       seqSd <- seqNbSd[seqNbSd != 0] * sdVec
#       
#       
#     }
#       
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     nbSd <- floor((nbcl - 1) / 2)
#     seqNbSd <- seq(-nbSd, nbSd, by = 1)
#     seqSd <- seqNbSd[seqNbSd != 0] * sdVec
#     
#     if(avgVec + min(seqSd) < minVec | avgVec + max(seqSd) > maxVec){
#       stop("Attention tu vas m'énerver")
#     } else {
#       
#       
#       
#       
#       
#       if(evenClass){
#         brks <- c(minVec, avgVec + seqSd[1:nbSd], avgVec, 
#                   avgVec + seqSd[(nbSd+1):length(seqSd)], 
#                   maxVec)
#       } else {
#         brks <- c(minVec, avgVec + seqSd[1:nbSd], 
#                   avgVec + seqSd[(nbSd+1):length(seqSd)], 
#                   maxVec)
#       }
#       return(brks[!is.na(brks)])
#     }
#   }
# }
