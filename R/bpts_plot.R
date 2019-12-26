
############## 2. Plot the graphs with boundary polygons ##############


#' Boundary Points Plot
#'
#' bpts_plot creates a ggplot2 scatterplot of your bpts object with the boundary
#' lines and no-data zones identified.
#'
#' @param bpts your bpts object created by the extract_bpts() function.
#' @param xlab the x-axis label. Defaults to "".
#' @param ylab the y-axis label. Defaults to "".
#' @param export_name the filename of your plot if you choose to export it. Include .png suffix. Defaults to "bpts plot.png" if save_plot = TRUE.
#' @param save_plot TRUE or FALSE to save the plot. Defaults to FALSE.
#' @param colour TRUE or FALSE to plot using colour or in black and white. Defaults to TRUE.
#'
#' @return a ggplot2 scatterplot
#' @import ggplot2
#' @import dplyr
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' a = rnorm(100,0,1)
#' b = rnorm(100,0,1)
#' bptsExample = extract_bpts(a,b)
#' bpts_plot(bptsExample, "a", "b")

bpts_plot = function(bpts, xlab = "x", ylab = "y", export_name = "bpts plot.png", save_plot = FALSE, colour = TRUE, legend.position = "none"){

  
 bpts_baseplot = ggplot(data = filter(bpts, !legend %in% c(0, 6)), aes(x, y)) +
    
    geom_point(shape = 21, fill = "grey", alpha = 0.5) +
    
    geom_polygon(data = dplyr::filter(bpts, legend == 0), 
                 aes(fill = type), alpha = 0.3, show.legend = F) +
    geom_line(data = dplyr::filter(bpts, legend %in% 1:4), 
              aes(col = type, linetype = type)) +
    geom_point(data = dplyr::filter(bpts, legend %in% 1:4), 
               aes(fill = type), shape = 21) +
    
    scale_linetype_manual(values = c(1, 2, 3, 4), name = "No-data boundaries") +
   
   labs(x = xlab, y = ylab) +

    theme_bw() +
   theme(legend.position = legend.position)
 
 if(colour == TRUE){
   bpts_graph = bpts_baseplot +
    scale_fill_brewer(type = "qual", name = "No-data boundaries", palette = "Set1", direction = -1) +
    scale_color_brewer(type = "qual", name = "No-data boundaries", palette = "Set1", direction = -1) 
 } else {
   bpts_graph = bpts_baseplot +
    scale_fill_manual(name = "No-data boundaries", values = rep("darkgrey", times = 4)) +
    scale_color_manual(name = "No-data boundaries", values = rep("black", times = 4))
 }
 
 ifelse(save_plot == FALSE, return(bpts_graph), ggsave(export_name))
  
}

