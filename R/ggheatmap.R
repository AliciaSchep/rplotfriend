#' Makes heatmap
#'
#' @param X matrix with values to plot as heatmap
#' @param xlabel label for x axis
#' @param yabel label for y axis
#' @param guide label for guide
#' @param name title
#' @param colors colors for scale
#' @param limits limits for y axis
#' @param fixed wether x and y axes should be on same scale
#' @param guidebreaks breaks for color scale
#' @return ggplot
#' @export
ggheatmap <- function(X, xlabel = NA, ylabel = NA, guide = "Value", name = NA, 
                      colors = c(scales::muted("blue"),"white",scales::muted("red")), limits = NA,
                      fixed = T, guidebreaks = NA){
  # Makes a heatmap based on input array.  Does not do any clustering, normalization, etc.
  # Input must already be formatted so values in matrix represent values that should go into
  # heatmap.
  #
  df = cbind(data.frame("y" = factor(rownames(X), levels = rownames(X), ordered=T)),X)
  mdf = reshape2::melt(df, id = "y")
  p = ggplot(mdf, aes(x=variable, y=y, col = value)) + 
    geom_raster(aes(fill=value)) + 
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(angle=90, vjust = 0.5, hjust = 1),
      plot.margin = grid::unit(c(0.1,0.1,0.1,0.1),"cm")
    )
  if (fixed){
    p = p + coord_fixed()
  }
  if (is.na(limits[1])){
    limits = c(min(mdf$value,na.rm=T), max(mdf$value,na.rm=T))
  }
  if (is.na(guidebreaks[1])){
    if (length(colors)== 3 && limits[1] < 0){
      guidebreaks = c(limits[1],0,limits[2])
    }
    else if (length(colors)>3){
      guidebreaks = seq(limits[1],limits[2],length.out = length(colors))
    }
    else{
      guidebreaks = limits
    }    
  }
  if (length(colors) == 0){
    stop("Must input at least one color")
  }
  else if (length(colors) == 1){
    p = p + scale_fill_gradient(name = guide, limits=limits,low="white",high=colors[1], breaks = guidebreaks, label = pretty_scale_format, expand=c(0,0))+
      scale_colour_gradient(name = guide,limits=limits,low="white",high=colors[1], breaks = guidebreaks, label = pretty_scale_format, expand=c(0,0))
  }
  else if (length(colors) == 2){
    p = p + scale_fill_gradient(name = guide,limits=limits,low=colors[1],high=colors[2], breaks = guidebreaks, label = pretty_scale_format, expand=c(0,0))+
      scale_colour_gradient(name = guide,limits=limits,low=colors[1],high=colors[2], breaks = guidebreaks, label = pretty_scale_format, expand=c(0,0))
  }  
  else if (length(colors) == 3){
    p = p + scale_fill_gradient2(name = guide,limits = limits,low = colors[1], mid = colors[2], high = colors[3], breaks = guidebreaks, label = pretty_scale_format, expand=c(0,0))+
      scale_colour_gradient2(name = guide,limits = limits,low = colors[1], mid = colors[2], high = colors[3], breaks = guidebreaks, label = pretty_scale_format, expand=c(0,0))
  }  
  else if (length(colors) > 3){
    p = p + scale_fill_gradientn(name = guide,limits = limits,colours = colors, breaks = guidebreaks, label = pretty_scale_format, expand=c(0,0))+
      scale_colour_gradientn(name = guide,limits = limits,colours = colors, breaks = guidebreaks, label = pretty_scale_format, expand=c(0,0))
  }  
  else{
    p = p + theme(legend.position="none")
  }
  if (!is.na(xlabel)){
    p = p + xlab(xlabel)
  }
  else{
    p = p + theme(axis.title.x = element_blank())
  }
  if (!is.na(ylabel)){
    p = p + ylab(ylabel)
  }
  else{
    p = p + theme(axis.title.y = element_blank())
  }  
  if (!is.na(name)){
    p = p + ggtitle(name)
  }
  return(p)
}

