
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
#' @import dplyr
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
  voi_n = nrow(voi_dat)

  ############################### Define Top-Right Boundary ##########################################

  ### topright
  voi_ordered_y <- voi_dat[order(-voi_dat$y),]
  binned_points <- {}
  topr <- {}
  
  binned_points[[1]] <- voi_ordered_y 
  topr[[1]] <- binned_points[[1]][1,]
  for(i in 2:100){
    binned_points[[i]] <- binned_points[[i-1]][which(binned_points[[i-1]][,1] > binned_points[[i-1]][1,1]),]
    ##binned_points##
    topr[[i]] <- binned_points[[i]][1,]
    # topr
  }
  topr <- do.call("rbind.data.frame", topr)
  topr <- na.omit(topr)
  
  ############################### Define Top-Left Boundary ##########################################

  ### topleft
  topl <- {}
  binned_points[[1]] <- voi_ordered_y 
  topl[[1]] <- binned_points[[1]][1,]
  for(i in 2:100){
    binned_points[[i]] <- binned_points[[i-1]][which(binned_points[[i-1]][,1] < binned_points[[i-1]][1,1]),]
    ##binned_points##
    topl[[i]] <- binned_points[[i]][1,]
    # topl
  }
  topl <- do.call("rbind.data.frame", topl)
  topl <- na.omit(topl)
  
  ############################### Define Bottom-Right Boundary ##########################################
  ### bottomright
  voi_ordered_x <- voi_dat[order(-voi_dat$x),]
  botr <- {}
  binned_points[[1]] <- voi_ordered_x
  botr[[1]] <- binned_points[[1]][1,]
  for(i in 2:100){
    binned_points[[i]] <- binned_points[[i-1]][which(binned_points[[i-1]][,2] < binned_points[[i-1]][1,2]),]
    ##binned_points##
    botr[[i]] <- binned_points[[i]][1,]
    # botr
  }

  botr <- do.call("rbind.data.frame", botr)
  botr <- na.omit(botr)
  botr = botr[order(botr$y),]
  botr = botr[order(botr$x),]
  
  ############################### Define Bottom-Left Boundary ##########################################
  ### bottomleft
  voi_ordered_xmin <- dplyr::arrange(voi_dat, x, -y)
  botl <- {}
  binned_points[[1]] <- voi_ordered_xmin
  botl[[1]] <- binned_points[[1]][1,]
  for(i in 2:100){
    
    # ifelse(binned_points[[i-1]][1,2] == binned_points[[i-1]][2,2],
    #        binned_points[[i]] <- binned_points[[i-1]][-1,],
    #        binned_points[[i]] <- binned_points[[i-1]][which(binned_points[[i-1]][,2] < binned_points[[i-1]][1,2]),])
    binned_points[[i]] <- binned_points[[i-1]][which(binned_points[[i-1]][,2] < binned_points[[i-1]][1,2]),]
    
    ##binned_points##
    botl[[i]] <- binned_points[[i]][1,]
    # botl
  }
  
  botl <- do.call("rbind.data.frame", botl)
  botl <- na.omit(botl)
  
  # last_extreme = binned_points[[1]][1,]
  # binned_points[[1]][-1,]

  
  ############################### Process Boundaries ##########################################

  # Label the boundary points
  botl$type = "botl"
  botr$type = "botr"
  topl$type = "topl"
  topr$type = "topr"
  voi_dat$type = "datum"

  # Provide legend orders to the boundary points
  botl$legend = "4"
  botr$legend = "3"
  topl$legend = "1"
  topr$legend = "2"
  voi_dat$legend = "5"

  # Combine the respective outputs into one data frame
  collated = rbind(topl, topr, botr, botl, voi_dat)

  ### Define the extreme corners

  corners = matrix(NA, 4, 4)
  colnames(corners) = c("x", "y", "type", "legend")

  # Top left corner
  corners[1,1] = min(collated[collated$type == "topl", 1])
  corners[1,2] = max(collated[collated$type == "topl", 2])
  corners[1,3] = "topl_corner"
  corners[1,4] = "6"

  # Top right corner
  corners[2,1] = max(collated[collated$type == "topr", 1])
  corners[2,2] = max(collated[collated$type == "topr", 2])
  corners[2,3] = "topr_corner"
  corners[2,4] = "6"

  # Bottom right corner
  corners[3,1] = max(collated[collated$type == "botr", 1])
  corners[3,2] = min(collated[collated$type == "botr", 2])
  corners[3,3] = "botr_corner"
  corners[3,4] = "6"
  
  # Bottom left corner
  corners[4,1] = min(collated[collated$type == "botl", 1])
  corners[4,2] = min(collated[collated$type == "botl", 2])
  corners[4,3] = "botl_corner"
  corners[4,4] = "6"

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

  topl_poly$type = "topl"
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

  topr_poly$type = "topr"
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

  botr_poly$type = "botr"
  botr_poly$legend = "0"
  
  # Bottom left
  botl_poly = if(length(collated[collated$type == "botl",][[1]]) > lim_lower) {
    rbind(collated[collated$type == "botl_corner",],
          collated[collated$type == "botl",],
          collated[collated$type == "botl_corner",])
    
  } else {
    collated[collated$type == "botl",]
  }
  
  botl_poly$type = "botl"
  botl_poly$legend = "0"

  # poly_br <- botr_poly
  # poly_br$legend <- 1:length(poly_br[,1])
  # poly_br[,1:2] <- lapply(poly_br[,1:2], as.numeric)
  # poly_br[,3:4] <- lapply(poly_br[,1:2], as.factor)
  # poly_br <<- poly_br

  collated = rbind(collated, topl_poly, topr_poly, botr_poly, botl_poly)

  # Convert column types and matrix into factored data frame
  collated$x = as.numeric(collated$x)
  collated$y = as.numeric(collated$y)
  collated$type = factor(collated$type, levels = c("topl", "topr", "botl", "botr"), 
                         labels = c("Top-left",  "Top-right", "Bottom-left", "Bottom-right"))
  collated$legend = as.factor(collated$legend)
  rownames(collated) = 1:length(collated$x)
  output = as.data.frame(collated)

  # Return output
  return(output)

}
