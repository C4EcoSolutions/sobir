
############## 4. Calculate areas ##############

# Calculate areas of the BLA polygons defined by extract_bpts()

#' Caculate the no-data zone areas
#'
#' This function supports the other sobir functions by calculating the no-data zone areas
#'
#' @param xdat a vector of the independent data
#' @param ydat a vector of the dependent data
#'
#' @return a list of the no-data zone areas
#' @import tidyr
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' a = rnorm(100,0,1)
#' b = rnorm(100,0,1)
#' calc_area(a,b)
calc_area = function(xdat, ydat){
  
  # Extract boundary points

  extracted = extract_bpts(xdat, ydat)

  # Rescale polygon vertices to percent of x and y range
  topl_x = as.numeric(dplyr::filter(extracted, type == "Top-left", legend == 0)$x)
  topl_y = as.numeric(dplyr::filter(extracted, type == "Top-left", legend == 0)$y)
  topl_xcoord =  topl_x/range( extracted[, 1])[[2]]*100
  topl_ycoord =  topl_y/range( extracted[, 2])[[2]]*100
  topl_coords = cbind(topl_xcoord, topl_ycoord)

  topr_x = as.numeric(dplyr::filter(extracted, type == "Top-right", legend == 0)$x)
  topr_y = as.numeric(dplyr::filter(extracted, type == "Top-right", legend == 0)$y)
  topr_xcoord =  topr_x/range( extracted[, 1])[[2]]*100
  topr_ycoord =  topr_y/range( extracted[, 2])[[2]]*100
  topr_coords = cbind(topr_xcoord, topr_ycoord)

  botr_x = as.numeric(dplyr::filter(extracted, type == "Bottom-right", legend == 0)$x)
  botr_y = as.numeric(dplyr::filter(extracted, type == "Bottom-right", legend == 0)$y)
  botr_xcoord =  botr_x/range( extracted[, 1])[[2]]*100
  botr_ycoord =  botr_y/range( extracted[, 2])[[2]]*100
  botr_coords = cbind(botr_xcoord, botr_ycoord)
  
  botl_x = as.numeric(dplyr::filter(extracted, type == "Bottom-left", legend == 0)$x)
  botl_y = as.numeric(dplyr::filter(extracted, type == "Bottom-left", legend == 0)$y)
  botl_xcoord =  botl_x/range( extracted[, 1])[[2]]*100
  botl_ycoord =  botl_y/range( extracted[, 2])[[2]]*100
  botl_coords = cbind(botl_xcoord, botl_ycoord)
  

  # Create a Polygons
  p = sp::Polygon(topl_coords)
  topl_poly = sp::Polygons(list(p), 1)

  p = sp::Polygon(topr_coords)
  topr_poly = sp::Polygons(list(p), 1)

  p = sp::Polygon(botr_coords)
  botr_poly = sp::Polygons(list(p), 1)
  
  p = sp::Polygon(botl_coords)
  botl_poly = sp::Polygons(list(p), 1)

  # Extract areas
  area_topl = topl_poly@area
  area_topr = topr_poly@area
  area_botr = botr_poly@area
  area_botl = botl_poly@area
  output <- list(botl = area_botl, botr = area_botr, topl = area_topl, topr = area_topr)
  
  return(output)
}
