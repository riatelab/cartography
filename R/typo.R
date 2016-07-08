checkOrder <- function(legend.values.order, mod){
  if (!is.null(legend.values.order)){
    m <- match(mod, legend.values.order)
    m <- m[!is.na(m)]
    
    if(length(m) != length(mod) | length(mod) != length(legend.values.order)){
      stop(paste("'legend.values.order' modalities must fit the modalities of the variable (",
                 paste(mod, collapse=","),").",sep = ""),
           call. = FALSE)
    }
  }else{
    legend.values.order <- mod
  }
  return(legend.values.order)
}


checkCol <- function(col, mod){
  if (is.null(col)){
    lm <- length(mod)
    if (lm<=20){
      col <- carto.pal(pal1 = "pastel.pal", n1 = lm)
    }else{
      lc <- carto.pal(pal1 = "pastel.pal", 20)
      col <- sample(x = lc, size = lm , replace = T)
    } 
  }else{
    if (length(col) < length(mod)){
      stop(paste("'col' length (",length(col),
                 ") must fit the number of modalities of the variable (",
                 length(mod),").",sep = ""),
           call. = FALSE)
    }
  }
  return(col)
}