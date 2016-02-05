checkMergeOrder <- function(spdf = spdf, spdfid = spdfid, 
                            df = df, dfid = dfid, var = var){
  # Check identifier field
  if (is.null(spdfid)){spdfid <- names(spdf@data)[1]}
  if (is.null(dfid)){dfid <- names(df)[1]}
  
  # Merge spdf & df
  dots <- cbind(spdf@data[,spdfid], as.data.frame(sp::coordinates(spdf)))
  colnames(dots)[1] <- c(spdfid)
  dots <- data.frame(dots, df[match(dots[,spdfid], df[,dfid]),])
  dots <- dots[!is.na(x = dots[,var]),]
  # names(dots)[4] <- var
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



