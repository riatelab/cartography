#' @title Build Cartographic Palettes
#' @description \code{carto.pal} builds sequential, diverging and qualitative color palettes. 
#' Diverging color palettes can be dissymmetric (different number of colors in 
#' each of the two gradients).
#' @name carto.pal
#' @param pal1 name of the color gradient (see Details).
#' @param n1 number of colors (up to 20).
#' @param pal2 name of the color gradient (see Details). 
#' @param n2 number of colors (up to 20).
#' @param middle  a logical value. If TRUE, a neutral color ("#F6F6F6", light 
#' grey) between two gradients is added.
#' @param transparency a logical value. If TRUE, contrasts are enhanced by 
#' adding an opacity variation.
#' @details Sequential palettes: "blue.pal", "orange.pal", "red.pal", 
#' "brown.pal", "green.pal", "purple.pal", "pink.pal", "wine.pal", "grey.pal", 
#' "turquoise.pal", "sand.pal", "taupe.pal", "kaki.pal" or "harmo.pal". \cr\cr
#' Qualitative palettes: "pastel.pal" or "multi.pal".
#' @references Qualitative palettes were generated with "i want hue" 
#' (\url{https://medialab.github.io/iwanthue/}) by Mathieu Jacomy at 
#' the Sciences-Po Medialab.
#' @return \code{carto.pal} returns a vector of colors.
#' @examples 
#' # Simple gradient: blue
#' carto.pal(pal1 = "blue.pal" ,n1 = 20)
#' 
#' # Double gradient: blue & red
#' carto.pal(pal1 = "blue.pal", n1 = 10, pal2 = "red.pal", n2 = 10)
#' 
#' # Adding a neutral color
#' carto.pal(pal1 = "blue.pal", n1 = 10, pal2 = "red.pal", n2 = 10, middle = TRUE)
#' 
#' # Enhancing contrasts with transparency
#' carto.pal(pal1="blue.pal", n1 = 10, pal2 = "red.pal", n2 = 10, middle = TRUE,
#'           transparency = TRUE)
#' 
#' # The double gradient can be asymetric
#' carto.pal(pal1 = "blue.pal", n1 = 5, pal2 = "red.pal", n2 = 15, middle = TRUE,
#'           transparency = TRUE)
#' 
#' # Build and display a palette
#' mypal <- carto.pal(pal1 = "blue.pal", n1 = 5, pal2 = "red.pal", n2 = 15,
#'                    middle = TRUE, transparency = TRUE)
#' k <- length(mypal)
#' image(1:k, 1, as.matrix(1:k), col =mypal, xlab = paste(k," classes",sep=""),
#'       ylab = "", xaxt = "n", yaxt = "n",bty = "n")
#' @export
carto.pal <- function(pal1, n1, pal2 = NULL, n2 = NULL, middle = FALSE,
                      transparency = FALSE){
  alphainit <- 30
  alpha <- "FF"
  middlecol <- "#F6F6F6"
  if(is.null(pal2) & is.null(n2)){
    pal<-as.character(unlist(cartography.colors[[pal1]][n1]))
    if(transparency == TRUE){
      for ( i in 1:n1-1) {
        alpha <- as.hexmode(floor(alphainit+(255-alphainit)/n1*i))
        pal[i] <- paste(pal[i],alpha,sep="")
      }
      alpha <- as.hexmode(alphainit)
    }
  }
  
  if(!is.null(pal2) & !is.null(n2)){
    n <- max(n1,n2)
    pal1 <- as.character(unlist(cartography.colors[[pal1]][n]))
    pal2 <- as.character(unlist(cartography.colors[[pal2]][n]))
    
    if(transparency==TRUE){
      for ( i in 1:n-1) {
        alpha <- as.hexmode(floor(alphainit+(255-alphainit)/n*i))
        pal1[i] <- paste(pal1[i],alpha,sep="")
        pal2[i] <- paste(pal2[i],alpha,sep="")
      }
      alpha <- as.hexmode(alphainit)
    }
    
    pal1 <- pal1[1:n1]
    pal1 <- rev(pal1)
    pal2 <- pal2[1:n2]
    
    pal <- c(pal1,pal2)
    if(middle){pal <- c(pal1,paste(middlecol,alpha,sep=""),pal2)}
  }
  return(pal)
}

#' @description \code{carto.pal.info} displays the names of all color palettes.
#' @name carto.pal
#' @return \code{carto.pal.info} returns a vector of color palettes names. 
#' @examples
#' carto.pal.info()
#' @export
carto.pal.info <- function(){
  names(cartography.colors)
}




#' @description \code{display.carto.pal} displays one color palette.
#' @name carto.pal
#' @param name name of the palette available in the package (see Details).
#' @examples
#' display.carto.pal("orange.pal")
#' @export
display.carto.pal<-function(name)
{
  old.par <- par(no.readonly = TRUE)
  par(mfrow=c(5,4))
  par(mar=c(0.2, 0.2, 1, 0.2), xaxs='i', yaxs='i')
  for ( i in 1:20) {
    mypal <- carto.pal(name,i)
    k<-length(mypal)
    image(1:k, 1, as.matrix(1:k), col = mypal, 
          xlab = paste(k," classes",sep=""), 
          ylab = "", xaxt = "n", yaxt = "n",bty = "n")
    
    if (i==1){cl <- "classe"}else{cl <- "classes"}
    title(paste(i,cl,sep=" "))
  }
  par(old.par)
}


#' @description \code{display.carto.all} displays all the available color palettes.
#' @name carto.pal
#' @param n number of colors in the gradient (up to 20).
#' @examples
#' display.carto.all(8)
#' @export
display.carto.all <- function(n = 10)
{
  nbpal <- length(cartography.colors)
  ncol <- 2
  nrow <- round(nbpal/ncol+0.1)
  old.par <- par(no.readonly = TRUE)
  par(mfrow=c(nrow,ncol))
  par(mar=c(0.2, 0.2, 1, 0.2), xaxs='i', yaxs='i')
  
  for ( i in 1:nbpal) {
    #  i <- 1
    pal <- names(cartography.colors)[i]
    mypal <- carto.pal(pal,n)
    k<-length(mypal)
    image(1:k, 1, as.matrix(1:k), col =mypal,
          xlab = paste(k," classes",sep=""), 
          ylab = "", xaxt = "n", yaxt = "n",bty = "n")
    title(names(cartography.colors)[i])
  }
  par(old.par)
}






