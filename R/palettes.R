#' @title Build Cartographic Palettes
#' @description Builds sequantial and diverging color palettes. 
#' Diververging color palettes can be dissymetric (two gradients with different number of colors)
#' @name carto.pal
#' @param pal1 character; name of the color gradiant (see Details).
#' @param n1 numeric; number of colors (up to 20)
#' @param pal2 character; name of the color gradiant (see Details). 
#' @param n2 numeric; number of colors (up to 20)
#' @param middle boolean; if TRUE, add a neutral color ("#F6F6F6") between two gradients.
#' @param transparency boolean; if TRUE, enhance contrasts by adding opacity variation.
#' @details Available palettes are: "blue.pal", "orange.pal", "red.pal", "brown.pal", "green.pal", "purple.pal"
#' "pink.pal", "wine.pal", "grey.pal", "turquoise.pal", "sand.pal", "taupe.pal", "kaki.pal" , "harmo.pal". 
#' Use  \code{display.carto.all(5)} to show all palettes. Use display.carto.pal("PALETTE NAME") to show one palette. 
#' @return A vector of colors is returned
#' @examples 
#' # Simple gradient in blue
#' carto.pal(pal1 = "blue.pal" ,n1 = 20)
#' 
#' # double gradient blue / red
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
#' ylab = "", xaxt = "n", yaxt = "n",bty = "n")
#' @export
#' 
carto.pal <- function(pal1, n1, pal2 = NULL, n2 = NULL, middle = FALSE,
                      transparency = FALSE){

  
  data("cartography.colors", envir = environment())
  cartography.colors <- get("cartography.colors", envir  = environment())
  alphainit<-30
  alpha="FF"
  middlecol<-"#F6F6F6"
  if(is.null(pal2) & is.null(n2)){
    pal<-as.character(unlist(cartography.colors[[pal1]][n1]))
    if(transparency==TRUE){
      for ( i in 1:n1-1) {
        alpha<-as.hexmode(floor(alphainit+(255-alphainit)/n1*i))
        pal[i]<-paste(pal[i],alpha,sep="")
      }
      alpha<-as.hexmode(alphainit)
    }
  }
  
  if(!is.null(pal2) & !is.null(n2)){
    n <- max(n1,n2)
    pal1<-as.character(unlist(cartography.colors[[pal1]][n]))
    pal2<-as.character(unlist(cartography.colors[[pal2]][n]))
    
    if(transparency==TRUE){
      for ( i in 1:n-1) {
        alpha<-as.hexmode(floor(alphainit+(255-alphainit)/n*i))
        pal1[i]<-paste(pal1[i],alpha,sep="")
        pal2[i]<-paste(pal2[i],alpha,sep="")
      }
      alpha<-as.hexmode(alphainit)
    }
    
    pal1 <-pal1[1:n1]
    pal1<-rev(pal1)
    pal2 <-pal2[1:n2]
    
    pal<-c(pal1,pal2)
    if(middle==T){pal<-c(pal1,paste(middlecol,alpha,sep=""),pal2)}
  }
  return(pal)
}



#' @title Display All Cartographic Palettes
#' @description Display all the available color palettes.
#' @name display.carto.all
#' @param n numeric; number of colors by palettes (from 1 to 20).
#' @examples
#' display.carto.all(1)
#' display.carto.all(5)
#' display.carto.all(8)
#' display.carto.all(12)
#' display.carto.all(20)
#' @return A plot is returned.
#' @export
display.carto.all<-function(n = 10)
{
  data("cartography.colors", envir = environment())
  cartography.colors <- get("cartography.colors", envir  = environment())
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
    image(1:k, 1, as.matrix(1:k), col =mypal, xlab = paste(k," classes",sep=""), 
          ylab = "", xaxt = "n", yaxt = "n",bty = "n")
    title(names(cartography.colors)[i])
  }
  par(old.par)
}


#' @title Display One Cartographic Palette
#' @description Display one color palette.
#' @name display.carto.pal
#' @param name character; name of the palette available in the package (see Details)
#' @details Available palettes are: "blue.pal", "orange.pal", "red.pal", "brown.pal", "green.pal", "purple.pal"
#' "pink.pal", "wine.pal", "grey.pal", "turquoise.pal", "sand.pal", "taupe.pal", "kaki.pal" , "harmo.pal". 
#' @examples
#' display.carto.pal("orange.pal")
#' display.carto.pal("sand.pal")
#' @return A plot is returned.
#' @export
display.carto.pal<-function(name)
{
  old.par <- par(no.readonly = TRUE)
  par(mfrow=c(5,4))
  par(mar=c(0.2, 0.2, 1, 0.2), xaxs='i', yaxs='i')
  for ( i in 1:20) {
    mypal <- carto.pal(name,i)
    k<-length(mypal)
    image(1:k, 1, as.matrix(1:k), col =mypal, xlab = paste(k," classes",sep=""), 
          ylab = "", xaxt = "n", yaxt = "n",bty = "n")
    
    if (i==1){cl <- "classe"}else{cl <- "classes"}
    title(paste(i,cl,sep=" "))
  }
  par(old.par)
}
