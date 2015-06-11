#' @title carto.pal
#' @description Build a color palette
#' @name carto.pal
#' @details This function call and combine colors from the colors object with several paraemeters
#' @param pal1 name of the color gradiant
#' @param n1 number of colors (up to 20)
#' @param pal2 name of a second color gradiant (optionnal)
#' @param n2 number of colors for the second color gradiant (up to 20) (optionnal)
#' @param middle boolean. if true, add a neutral color in the middle (optionnal)
#' @param alphaeffect boolean. If true, enhance contrasts by adding opacity (optionnal)
#' @examples
#' # Simple gradient in blue
#' carto.pal("blue.pal",20)
#'
#' # double gradient blue / red
#' carto.pal(pal1="blue.pal",n1=10, pal2="red.pal",n2=10)
#'
#' # Adding a neutral color
#' carto.pal(pal1="blue.pal",n1=10,pal2="red.pal",n2=10,middle=TRUE)
#'
#' # Enhancing contrasts with alphaeffect
#' carto.pal(pal1="blue.pal",n1=10,pal2="red.pal",n2=10,middle=TRUE,alphaeffect=TRUE)
#'
#' # The double gradient can be asymetric
#' carto.pal(pal1="blue.pal",n1=5,pal2="red.pal",n2=15,middle=TRUE,alphaeffect=TRUE)
#'
#' # Build and display a palette
#' mypal <- carto.pal(pal1="blue.pal",n1=5,pal2="red.pal",n2=15,middle=TRUE,alphaeffect=TRUE)
#' k<-length(mypal)
#' image(1:k, 1, as.matrix(1:k), col =mypal, xlab = paste(k," classes",sep=""),
#' ylab = "", xaxt = "n", yaxt = "n",bty = "n")
#'
#' @return vector
#'
#' @export

carto.pal<-function(pal1,n1,pal2=NULL,n2=NULL,middle=FALSE,alphaeffect=FALSE){

  # PARAMETRES
  alphainit<-30
  alpha="FF"
  middlecol<-"#F6F6F6"

  # CREATION DE LA PALETTE

  # 1.Simple gradation ----------------------------

  if(is.null(pal2) & is.null(n2)){
    pal<-as.character(unlist(colors[[pal1]][n1]))
    if(alphaeffect==T){
      for ( i in 1:n1-1) {
        alpha<-as.hexmode(floor(alphainit+(255-alphainit)/n1*i))
        pal[i]<-paste(pal[i],alpha,sep="")
      }
      alpha<-as.hexmode(alphainit)
    }
  }

  # 2. Double gradation ------------------------

  if(!is.null(pal2) & !is.null(n2)){
    n<-max(n1,n2)
    pal1<-as.character(unlist(colors[[pal1]][n]))
    pal2<-as.character(unlist(colors[[pal2]][n]))

    if(alphaeffect==T){
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

#' @title display.carto.all
#' @description Display all the color palettes available
#' @name display.carto.all
#' @details This function displays all the palettes available in the package
#'
#' @param nb numeric number of colors by palettes (from 1 to 20)
#'
#' @examples
#' display.carto.all(1)
#' display.carto.all(5)
#' display.carto.all(8)
#' display.carto.all(12)
#' display.carto.all(20)
#'
#' @return vector
#'
#' @export


# FONCTION 2 : RECUPERATION ET AFFICHAGE DES DEGRADES
display.carto.all<-function(nb=10)
{

  nbpal <- length(colors)

  ncol <- 2
  nrow <- round(nbpal/ncol+0.1)
  old.par <- par(no.readonly = TRUE)
  par(mfrow=c(nrow,ncol))
  par(mar=c(0.2, 0.2, 1, 0.2), xaxs='i', yaxs='i')

  for ( i in 1:nbpal) {
    #  i <- 1
    pal <- names(colors)[i]
    mypal <- carto.pal(pal,nb)
    k<-length(mypal)
    image(1:k, 1, as.matrix(1:k), col =mypal, xlab = paste(k," classes",sep=""), ylab = "", xaxt = "n", yaxt = "n",bty = "n")
    title(names(colors)[i])


  }
  par(old.par)
}


#' @title display.carto.pal
#' @description Display one color palette
#' @name display.carto.pal
#' @details This function displays all the content of one palette
#' @param name character name of the palette available in the package
#'
#' @examples
#' display.carto.pal("orange.pal")
#' display.carto.pal("sand.pal")
#'
#' @return plot
#'
#' @export



display.carto.pal<-function(name)
{
  old.par <- par(no.readonly = TRUE)
  par(mfrow=c(5,4))
  par(mar=c(0.2, 0.2, 1, 0.2), xaxs='i', yaxs='i')
  for ( i in 1:20) {
    mypal <- carto.pal(name,i)
    k<-length(mypal)
    image(1:k, 1, as.matrix(1:k), col =mypal, xlab = paste(k," classes",sep=""), ylab = "", xaxt = "n", yaxt = "n",bty = "n")

    if (i==1){cl <- "classe"}else{cl <- "classes"}
    title(paste(i,cl,sep=" "))
  }
  par(old.par)
}
