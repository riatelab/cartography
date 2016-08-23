checkMergeOrder <- function(spdf = spdf, spdfid = spdfid, 
                            df = df, dfid = dfid, var = var){
  # Create dots by merging spdf coordinates & df data
  dots <- cbind(spdf@data[,spdfid], as.data.frame(sp::coordinates(spdf)))
  colnames(dots)[1] <- spdfid
  dots <- data.frame(dots, df[match(dots[,spdfid], df[,dfid]),])
  # remove NAs and 0 values
  dots <- dots[!is.na(x = dots[,var]),]
  dots <- dots[dots[,var]!=0, ]
  # Order the dots
  dots <- dots[order(abs(dots[, var]), decreasing = TRUE),]
  return(dots)
}

sizer <- function(dots, inches, var, fixmax, symbols){
  smax <- inches * inches * pi
  switch(symbols, 
         circle = {
           smax <- inches * inches * pi
           size <- sqrt((abs(dots[, var]) * smax  / fixmax) / pi)
         }, 
         square = {
           smax <- inches * inches
           size <- sqrt(abs(dots[, var]) * smax   / fixmax)
         }, 
         bar = {
           smax <- inches
           size <- abs(dots[,var]) * smax  / fixmax
         })
  return(size)
}



