
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
#' @import stringr
#' @import dplyr
#' @import DescTools
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
  
  # Rescale polygon vertices to proportion of x and y range
  topl_x = as.numeric(dplyr::filter(extracted, type == "Top-left", legend == 0)$x)
  topl_y = as.numeric(dplyr::filter(extracted, type == "Top-left", legend == 0)$y)
  topl_coords = dplyr::mutate(data.frame(x = scales::rescale(topl_x, from = range(extracted$x)), 
                                         y = scales::rescale(topl_y, from = range(extracted$y))),
                              label = ifelse(x == min(x) | y == max(y), 
                                             ifelse(x == min(x) & y == max(y), "corner", "bound_corner"),
                                             "bound"))
  
  topr_x = as.numeric(dplyr::filter(extracted, type == "Top-right", legend == 0)$x)
  topr_y = as.numeric(dplyr::filter(extracted, type == "Top-right", legend == 0)$y)
  topr_coords = dplyr::mutate(data.frame(x = scales::rescale(topr_x, from = range(extracted$x)), 
                                         y = scales::rescale(topr_y, from = range(extracted$y))),
                              label = ifelse(x == max(x) | y == max(y),
                                             ifelse(x == max(x) & y == max(y), "corner", "bound_corner"),
                                             "bound"))
  
  botr_x = as.numeric(dplyr::filter(extracted, type == "Bottom-right", legend == 0)$x)
  botr_y = as.numeric(dplyr::filter(extracted, type == "Bottom-right", legend == 0)$y)
  botr_coords = dplyr::mutate(data.frame(x = scales::rescale(botr_x, from = range(extracted$x)), 
                                         y = scales::rescale(botr_y, from = range(extracted$y))),
                              label = ifelse(x == max(x) | y == min(y), 
                                             ifelse(x == max(x) & y == min(y), "corner", "bound_corner"),
                                             "bound"))
  
  botl_x = as.numeric(dplyr::filter(extracted, type == "Bottom-left", legend == 0)$x)
  botl_y = as.numeric(dplyr::filter(extracted, type == "Bottom-left", legend == 0)$y)
  botl_coords = dplyr::mutate(data.frame(x = scales::rescale(botl_x, from = range(extracted$x)), 
                                         y = scales::rescale(botl_y, from = range(extracted$y))),
                              label = ifelse(x == min(x) | y == min(y), 
                                             ifelse(x == min(x) & y == min(y), "corner", "bound_corner"),
                                             "bound"))

  topr_bound = dplyr::filter(topr_coords, stringr::str_detect(label, "bound"))
  topr_crnr = dplyr::filter(topr_coords, stringr::str_detect(label, "corner"))
  area_topr = DescTools::AUC(topr_crnr$x, topr_crnr$y, absolutearea = TRUE) - DescTools::AUC(topr_bound$x, topr_bound$y, absolutearea = TRUE) 
  
  topl_bound = dplyr::filter(topl_coords, stringr::str_detect(label, "bound"))
  topl_crnr = dplyr::filter(topl_coords, stringr::str_detect(label, "corner"))
  area_topl = DescTools::AUC(topl_crnr$x, topl_crnr$y, absolutearea = TRUE) - DescTools::AUC(topl_bound$x, topl_bound$y, absolutearea = TRUE)
  
  botr_bound = dplyr::filter(botr_coords, stringr::str_detect(label, "bound"))
  # botr_crnr = dplyr::filter(botr_coords, stringr::str_detect(label, "corner"))
  area_botr = DescTools::AUC(botr_bound$x, botr_bound$y, absolutearea = TRUE)
  
  botl_bound = dplyr::filter(botl_coords, stringr::str_detect(label, "bound"))
  # botl_crnr = dplyr::filter(botl_coords, stringr::str_detect(label, "corner"))
  area_botl = DescTools::DescTools::AUC(botl_bound$x, botl_bound$y, absolutearea = TRUE)
  
  output <- list(botl = area_botl, botr = area_botr, topl = area_topl, topr = area_topr)
  
  return(output)
}
