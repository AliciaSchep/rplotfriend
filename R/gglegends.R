#' Gets legend from plot
#'
#' @param gplot a ggplot
#' @param height desired value of legend.key.height
#' @param width desired value of legend.key.width
#' @return returns grob
#' @seealso \code{\link{add_legend}} 
#' @export
get_legend<-function(gplot, height = NA, width = NA){
  if (!is.na(height)){
    gplot <- gplot + theme(legend.key.height = height)
  }
  if (!is.na(width)){
    gplot <- gplot + theme(legend.key.width = width)
  }  
  grob <- ggplotGrob(gplot)
  legend <- grob$grobs[[which(sapply(grob$grobs, function(x) x$name =="guide-box"))]]
  return(legend)
}

#' Add to ggplot to remove legend
#' @export
no_legend <- theme(legend.position = "none")

#' Adds legend to plot
#'
#' @param gtab a gtable
#' @param legends legend grobs
#' @param height height of column in which legend is added (only use if pos is "right")
#' @param width width of column in which legend is added (only use if pos is "bottom" or "top")
#' @param pos position 
#' @return gtable
#' @seealso \code{\link{get_legend}} 
#' @export
add_legend<-function(gtab, legends, height = NA, width = NA, pos = c("right","top","bottom")){
  pos <- match.arg(pos)
  if (pos == "right"){
    if (is.na(width)){
      stop("Must enter width if pos is right")
    }
    gtab = gtable::gtable_add_cols(gtab, width = width)
    bottom = max(gtab$layout$b)
    top = min(gtab$layout$t)
    b = bottom - 1
    t = top + 1
    l = max(gtab$layout$r)+1
    r = l
    gtab = gtable::gtable_add_grob(gtab, legends, t = t, l = l, b = b, r = r,
                           z = Inf, clip = "on") 
  }
  else if (pos == "top"){
    if (is.na(height)){
      stop("Must enter height if pos is top")
    }
    gtab = gtable::gtable_add_rows(gtab, height= height)
    left = max(gtab$layout$l)
    right = min(gtab$layout$r)
    b = min(gtab$layout$b)-1
    t = b
    l = left - 1
    r = right + 1
    gtab = gtable::gtable_add_grob(gtab, legends, t = t, l = l, b = b, r = r,
                           z = Inf, clip = "on") 
  }  
  else if (pos == "bottom"){
    if (is.na(height)){
      stop("Must enter height if pos is top")
    }
    gtab = gtable::gtable_add_rows(gtab, height= height)
    left = max(gtab$layout$l)
    right = min(gtab$layout$r)
    b = max(gtab$layout$t)+1
    t = b
    l = left - 1
    r = right + 1
    gtab = gtable::gtable_add_grob(gtab, legends, t = t, l = l, b = b, r = r,
                           z = Inf, clip = "on") 
  }  
  
  return(gtab)
}



