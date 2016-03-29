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
    col <- grDevices::rainbow(length(mod))
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