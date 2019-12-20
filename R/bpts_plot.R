
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
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' a = rnorm(100,0,1)
#' b = rnorm(100,0,1)
#' bptsExample = extract_bpts(a,b)
#' bpts_plot(bptsExample, "a", "b")

bpts_plot = function(bpts, xlab = "", ylab = "", export_name = "bpts plot.png", save_plot = FALSE, colour = TRUE){
  
  if(colour == TRUE){
    bpts_graph = ggplot(data = bpts, aes(x = .data$x, y = .data$y, fill = .data$legend))+
      geom_polygon(data = subset(bpts, .data$type == "topl_poly"), fill = "blue", alpha = 0.3) +
      geom_polygon(data = subset(bpts, .data$type == "topr_poly"), fill = "red", alpha = 0.3) +
      geom_polygon(data = subset(bpts, .data$type == "botr_poly"), fill = "green", alpha = 0.3) +
      geom_point(data = subset(bpts, .data$legend == "4"), shape = 21, alpha = 0.5) +
      geom_point(data = subset(bpts, .data$legend == "3"), shape = 21) +
      geom_line(data = subset(bpts, .data$legend == "3"), col = "green") +
      geom_point(data = subset(bpts, .data$legend == "1"), shape = 21) +
      geom_line(data = subset(bpts, .data$legend == "1"), col = "blue") +
      geom_point( data = subset(bpts, .data$legend == "2"), shape = 21) +
      geom_line(data = subset(bpts, .data$legend == "2"), col = "red") +
      scale_fill_manual(values = c("blue","red","green","grey"), name = "Legend",
                        labels = c("Top left boundary",  "Top right boundary",
                                   "Bottom right boundary", "Non-boundary points")) +
      labs(x = xlab,
           y = ylab) +
      theme_classic()
  } else {

    bpts_graph = ggplot(data = bpts, aes(x = .data$x, y = .data$y, fill = .data$legend))+
      geom_polygon(data = subset(bpts, .data$type == "topl_poly"), fill = "grey", alpha = 0.3) +
      geom_polygon(data = subset(bpts, .data$type == "topr_poly"), fill = "grey", alpha = 0.3) +
      geom_polygon(data = subset(bpts, .data$type == "botr_poly"), fill = "grey", alpha = 0.3) +

      geom_line(data = subset(bpts, .data$legend == "3"), col = "darkgrey", linetype = "dotted") +
      geom_line(data = subset(bpts, .data$legend == "1"), col = "darkgrey") +
      geom_line(data = subset(bpts, .data$legend == "2"), col = "darkgrey", linetype = "longdash") +

      geom_point(data = subset(bpts, .data$legend == "4"), col = "black") +

      geom_point(data = subset(bpts, .data$legend == "3"), col = "black") +
      geom_point(data = subset(bpts, .data$legend == "1"), col = "black") +
      geom_point( data = subset(bpts, .data$legend == "2"),col = "black") +

      scale_fill_manual(values = c("black","black","black","black"), name = "Legend",
                        labels = c("Top left boundary",  "Top right boundary",
                                   "Bottom right boundary", "Non-boundary points")) +

      labs(x = xlab,
           y = ylab) +

      theme_bw() +
      theme(legend.position = "none")
  }

  ifelse(save_plot == FALSE, return(bpts_graph), ggsave(export_name))

}
