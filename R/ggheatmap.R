#' Makes heatmap
#'
#' @param X matrix with values to plot as heatmap
#' @param xlabel label for x axis
#' @param yabel label for y axis
#' @param guide label for guide
#' @param name title
#' @param colors colors for scale
#' @param midpoint midpoint for color scale if 3 colors given
#' @param limits limits for y axis
#' @param fixed wether x and y axes should be on same scale
#' @param guidebreaks breaks for color scale
#' @param cluster_row should rows be clustered? (default = FALSE)
#' @param cluster_col should columns be clustered? (default = FALSE)
#' @param cluster_row_dist distance function for clustering rows (default = stats::dist)
#' @param cluster_col_dist distance function for clustering columns (default = stats::dist)
#' @return ggplot
#' @export
ggheatmap <- function(X, xlabel = NA, ylabel = NA, guide = "Value", name = NA, 
                      colors = NULL, midpoint = NULL, limits = NA,
                      fixed = FALSE, guidebreaks = NA, cluster_row = FALSE, cluster_col = FALSE, 
                      cluster_row_dist = stats::dist, cluster_col_dist = stats::dist){
  if (is.null(rownames(X))){
    rownames(X) = seq_len(nrow(X))
  }
  if (is.null(colnames(X))){
    colnames(X) = seq_len(ncol(X))
  }
  if (cluster_row){
    rowclust = hclust(cluster_row_dist(X))
    X = X[rowclust$order,]
  }
  if (cluster_col){
    colclust = hclust(cluster_col_dist(t(X)))
    X = X[,colclust$order]
  }
  
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
  if (cluster_row){
    dendro_row = ggdendro::dendro_data(rowclust)
    dendro_segments_row = dendro_row$segments
    dendro_segments_row[,c("y","yend")] = (dendro_segments_row[,c("y","yend")] * mean(dim(X)) / max(dendro_segments_row[,c("y","yend")]) * 0.4) + ncol(X) + 1
     p = p + geom_segment(data = dendro_segments_row,  mapping = aes(x=y,xend=yend, y = x, yend = xend, col=NULL))
  }
  if (cluster_col){
    dendro_col = ggdendro::dendro_data(colclust)
    dendro_segments_col = dendro_col$segments
    dendro_segments_col[,c("y","yend")] = (dendro_segments_col[,c("y","yend")]* mean(dim(X)) / max(dendro_segments_col[,c("y","yend")])  * 0.4) + nrow(X) + 1
    p = p + geom_segment(data = dendro_segments_col,  mapping = aes(x=x,xend=xend, y = y, yend = yend, col=NULL))
  }
  
  if (fixed){
    p = p + coord_fixed()
  }

  if (is.na(limits[1])){
    limits = c(min(mdf$value,na.rm=T), max(mdf$value,na.rm=T))
  }
  if (is.null(colors)){
    if (limits[1] < 0 && limits[2] > 0){
      colors = c(scales::muted("blue"),"white",scales::muted("red"))
    } else{
      colors =  c("white",scales::muted("red"))
    }
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
  if (length(colors) == 1){
    p = p + scale_fill_gradient(name = guide, limits=limits,low="white",high=colors[1], breaks = guidebreaks, label = pretty_scale_format, expand=c(0,0))+
      scale_colour_gradient(name = guide,limits=limits,low="white",high=colors[1], breaks = guidebreaks, label = pretty_scale_format, expand=c(0,0))
  }
  else if (length(colors) == 2){
    p = p + scale_fill_gradient(name = guide,limits=limits,low=colors[1],high=colors[2], breaks = guidebreaks, label = pretty_scale_format, expand=c(0,0))+
      scale_colour_gradient(name = guide,limits=limits,low=colors[1],high=colors[2], breaks = guidebreaks, label = pretty_scale_format, expand=c(0,0))
  }  
  else if (length(colors) == 3){
    if (is.null(midpoint)){
      if (limits[1] < 0 && limits[2] > 0){
        midpoint = 0
      } else{
        midpoint = median(mdf$value)
      }
    }
    p = p + scale_fill_gradient2(name = guide,limits = limits,low = colors[1], mid = colors[2], high = colors[3], breaks = guidebreaks, label = pretty_scale_format, expand=c(0,0), midpoint = midpoint)+
      scale_colour_gradient2(name = guide,limits = limits,low = colors[1], mid = colors[2], high = colors[3], breaks = guidebreaks, label = pretty_scale_format, expand=c(0,0), midpoint = midpoint)
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

