
############## 8. Calculate permuted polygon areas ##############


#' Calculate the permuted area
#'
#' perm_area calculates the no-data zone areas for each permutation of the data simulated nsim times.
#'
#' @param xdat a vector of the independent data
#' @param ydat a vector of the dependent data
#' @param nsim the number of simulations to run
#'
#' @return a perm table that can be plotted directly using perm_plot()
#' @import tidyr
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' a = rnorm(100,0,1)
#' b = rnorm(100,0,1)
#' perm_area(a,b,10)
perm_area = function(xdat, ydat, nsim){
  
  obs = cbind.data.frame(xdat, ydat)
  fix_ymax = obs[obs$ydat == max(ydat), ]
  fix_ymax = fix_ymax[fix_ymax$xdat == max(fix_ymax$xdat), ]
  fix_ymax = unique(fix_ymax)

  fix_xmax = obs[obs$xdat == max(xdat), ]
  fix_xmax = fix_xmax[fix_xmax$ydat == max(fix_xmax$ydat), ]
  fix_xmax = unique(fix_xmax)


  ID_free = ( match(obs$xdat, fix_xmax$xdat) & match(obs$ydat, fix_xmax$ydat) ) | ( match(obs$xdat, fix_ymax$xdat) & match(obs$ydat, fix_ymax$ydat) )
  ID_free[is.na(ID_free)] = F

  free_dat <- obs[!ID_free, ]

  x_dat = free_dat$xdat
  y_dat = free_dat$ydat
  fix_ymax = as.matrix(fix_ymax)
  fix_xmax = as.matrix(fix_xmax)

  # Create empty matrix with a column for each polygon area and a row for each iteration
  result = matrix(NA, nrow = nsim, ncol = 3)

  for(j in 1:nsim){
    # Create simulation
    sim_x = sample(x_dat, size = length(x_dat), replace = F)
    sim_y = sample(y_dat, size = length(y_dat), replace = F)
    sim_x = c(sim_x, fix_xmax[ , 1], fix_ymax[ , 1])
    sim_y = c(sim_y, fix_xmax[ , 2], fix_ymax[ , 2])

    # Calculate simulated area
    sim_area =    suppressWarnings(calc_area(sim_x, sim_y))

    for(i in 1:3){
      result[j , i] = sim_area[[i]]
    }
  }

  # Create tidy dataframe for the simulated areas
  result_df = as.data.frame(result)
  colnames(result_df) = c("botr", "topl", "topr")
  df_tidy = gather(result_df, "polygon", "val")
  df_tidy$source = "sim"

  # Calculate observed area
  obs_area = suppressWarnings(calc_area(xdat, ydat))

  # Create tidy dataframe for the observed areas
  obs_tidy = gather(as.data.frame(obs_area), "polygon", "val")
  obs_tidy$source = "obs"

  # Collate the df
  collated_tidy = rbind.data.frame(df_tidy, obs_tidy)
  collated_tidy$rescale = scales::rescale(collated_tidy$val)
  return(collated_tidy)
}
