
############## 1. Extract and label the boundary points ##############

#' Extract boundary points
#'
#' extract_bpts identifies the boundary points to a scatterplot and labels the
#' relevant data for further analysis in the sobir package.
#'
#' @param xdat a vector of the independent data
#' @param ydat a vector of the dependent data
#'
#' @return a data frame of the two vectors with the boundary points and other relevant data labelled.
#' @importFrom rlang .data
#' @importFrom stats na.omit
#' @export
#'
#' @examples
#' a = rnorm(100,0,1)
#' b = rnorm(100,0,1)
#' extract_bpts(a,b)
extract_bpts = function(xdat, ydat) {
  
  x = xdat
  y = ydat

  voi_dat <- cbind.data.frame(x,y)
  voi_dat <- na.omit(voi_dat)

  #########################################################################

  ### topright
  voi_ordered <- voi_dat[order(-voi_dat$y),]
  binned_points <- {}
  topr <- {}
  for(i in 2:100){
    binned_points[[1]] <- voi_ordered
    binned_points[[i]] <- binned_points[[i-1]][which(binned_points[[i-1]][,1] > binned_points[[i-1]][1,1]),]
    ##binned_points##
    topr[[1]] <- binned_points[[1]][1,]
    topr[[i]] <- binned_points[[i]][1,]
    topr
  }
  topr <- do.call("rbind.data.frame", topr)
  topr <- na.omit(topr)

  #########################################################################

  ### topleft
  topl <- {}
  for(i in 2:100){
    binned_points[[1]] <- voi_ordered
    binned_points[[i]] <- binned_points[[i-1]][which(binned_points[[i-1]][,1] < binned_points[[i-1]][1,1]),]
    ##binned_points##
    topl[[1]] <- binned_points[[1]][1,]
    topl[[i]] <- binned_points[[i]][1,]
    topl
  }
  topl <- do.call("rbind.data.frame", topl)
  topl <- na.omit(topl)

  ######################################################################
  ### bottomright
  voi_ordered_botr <- voi_dat[order(-voi_dat$x),]
  botr <- {}
  for(i in 2:100){
    binned_points[[1]] <- voi_ordered_botr
    binned_points[[i]] <- binned_points[[i-1]][which(binned_points[[i-1]][,2] < binned_points[[i-1]][1,2]),]
    ##binned_points##
    botr[[1]] <- binned_points[[1]][1,]
    botr[[i]] <- binned_points[[i]][1,]
    botr
  }

  botr <- do.call("rbind.data.frame", botr)
  botr <- na.omit(botr)
  botr = botr[order(botr$y),]
  botr = botr[order(botr$x),]


  #####################################################################

  # Label the boundary points
  botr$type = "botr"
  topl$type = "topl"
  topr$type = "topr"
  voi_ordered$type = "datum"

  # Provide legend orders to the boundary points
  botr$legend = "3"
  topl$legend = "1"
  topr$legend = "2"
  voi_ordered$legend = "4"

  # Combine the respective outputs into one data frame
  collated = rbind(topl, topr, botr, voi_ordered)

  ### Define the extreme corners

  corners = matrix(NA, 3, 4)
  colnames(corners) = c("x", "y", "type", "legend")

  # Top left corner
  corners[1,1] = min(collated[collated$type == "topl", 1])
  corners[1,2] = max(collated[collated$type == "topl", 2])
  corners[1,3] = "topl_corner"
  corners[1,4] = "5"

  # Top right corner
  corners[2,1] = max(collated[collated$type == "topr", 1])
  corners[2,2] = max(collated[collated$type == "topr", 2])
  corners[2,3] = "topr_corner"
  corners[2,4] = "5"

  # Bottom right corner
  corners[3,1] = max(collated[collated$type == "botr", 1])
  corners[3,2] = min(collated[collated$type == "botr", 2])
  corners[3,3] = "botr_corner"
  corners[3,4] = "5"

  collated = rbind(collated, corners)

  ### Define corner polygons

  lim_lower = 1

  # Top left
  topl_poly = if(length(collated[collated$type == "topl",][[1]]) > lim_lower) {
    rbind(collated[collated$type == "topl_corner",],
          collated[collated$type == "topl",],
          collated[collated$type == "topl_corner",])

  } else {
    collated[collated$type == "topl",]
  }

  topl_poly$type = "topl_poly"
  topl_poly$legend = "0"

  # poly_tl <- topl_poly
  # poly_tl$legend <- 1:length(poly_tl[,1])
  # poly_tl[,1:2] <- lapply(poly_tl[,1:2], as.numeric)
  # poly_tl[,3:4] <- lapply(poly_tl[,1:2], as.factor)
  # poly_tl <<- poly_tl


  # Top right
  topr_poly = if(length(collated[collated$type == "topr",][[1]]) > lim_lower) {
    rbind(collated[collated$type == "topr_corner",],
          collated[collated$type == "topr",],
          collated[collated$type == "topr_corner",])

  } else {
    collated[collated$type == "topr",]
  }

  topr_poly$type = "topr_poly"
  topr_poly$legend = "0"

  # poly_tr <- topr_poly
  # poly_tr$legend <- 1:length(poly_tr[,1])
  # poly_tr[,1:2] <- lapply(poly_tr[,1:2], as.numeric)
  # poly_tr[,3:4] <- lapply(poly_tr[,1:2], as.factor)
  # poly_tr <<- poly_tr


  # Bottom right
  botr_poly = if(length(collated[collated$type == "botr",][[1]]) > lim_lower) {
    rbind(collated[collated$type == "botr_corner",],
          collated[collated$type == "botr",],
          collated[collated$type == "botr_corner",])

  } else {
    collated[collated$type == "botr",]
  }

  botr_poly$type = "botr_poly"
  botr_poly$legend = "0"

  # poly_br <- botr_poly
  # poly_br$legend <- 1:length(poly_br[,1])
  # poly_br[,1:2] <- lapply(poly_br[,1:2], as.numeric)
  # poly_br[,3:4] <- lapply(poly_br[,1:2], as.factor)
  # poly_br <<- poly_br

  collated = rbind(collated, topl_poly, topr_poly, botr_poly)

  # Convert column types and matrix into factored data frame
  collated$x = as.numeric(collated$x)
  collated$y = as.numeric(collated$y)
  collated$type = as.factor(collated$type)
  collated$legend = as.factor(collated$legend)
  rownames(collated) = 1:length(collated$x)
  output = as.data.frame(collated)

  # Return output
  return(output)

}
