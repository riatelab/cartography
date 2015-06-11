# load("/home/nlambert/Documents/R/github/FreeCarto/freeCarto/data/colors.RData")
# load("/home/nlambert/Documents/R/github/FreeCarto/freeCarto/data/TNdeleg.rda")
# source("/home/nlambert/Documents/R/github/FreeCarto/freeCarto/R/internL_discretization.R")
# source("/home/nlambert/Documents/R/github/FreeCarto/freeCarto/R/internL_palette.R")

#' @name choro
#' @title choro
#' @description add color gradients to spdf according to data classes
#' @param var vector of valuesused
#' @param distr vector of classes
#' @param col vector of colors
#' @param nbclass number of classes targeted (if null, the Huntsberger method is used)
#' @param method discretization method ("sd", "equal", "quantile", "jenks","q6","geom")
#' @return List: a vector of colors, colors and distr
choro <- function(var, distr = NULL, col = NULL, nbclass = NULL,
                  method="quantile")
{
  # Discretization
  if (is.null(distr)){
    field <- var
    distr <- discretization(v = field, nbclass = nbclass, method = method)
  }
  # Colors
  if(is.null(col)){
    col <- carto.pal(pal1 = "blue.pal",n1 = (length(distr) - 1))
    # Si pas de breakval, on breakval = le min de la distribution
    #     if (is.null(breakval)) {
    #       breakval <- distr[1]
    #     }
    #     # one color
    #     if (breakval < distr[2] | breakval > distr[length(distr)-1]) {
    #       col <- carto.pal(pal1 = pal1, n1 = length(distr)-1, alphaeffect=alpha)
    #     }else{
    #       bornes <- distr[2:(length(distr)-1)]
    #       if (breakval %in% bornes) { middle <- FALSE } else { middle <- TRUE }
    #       num1 <- 0
    #       num2 <- 0
    #       for(i in 1:length(bornes)){if(bornes[i]<=breakval){num1 <- num1 + 1}}
    #       for(i in 1:length(bornes)){if(bornes[i]>=breakval){num2 <- num2 + 1}}
    #       col <- carto.pal(pal1,num1,pal2,num2,middle=middle,alphaeffect=alpha)
    # }
  }
  # Affectation des couleurs au spdf
  colMap <- col[(findInterval(var,distr,all.inside=TRUE))]
  return(list(colMap = colMap, distr = distr, col = col))
}



# mycols <- c("#A2D6EC","#F6F6F6FF","#BBDAAD","#97C38B","#74AC69","#468E3D","#247524","#16642A")
#test <-choro(spdf = TNdeleg.spdf, df = TNdeleg, spdfid = NULL, dfid = NULL, col = mycols, breakval = 15000 ,pal1="red.pal",var =  "housing", nbclass = 8, method = "quantile",alpha=TRUE)
# test$distr
# head(test$spdf@data)
# test$col
# display.gradient(test$col)

