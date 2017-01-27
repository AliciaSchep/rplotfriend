#' Theme for scientific publication
#'
#' @param base_size base font size
#' @param base_family font family
#' @return ggplot2 theme
#' @export
pub_theme <-function(base_size = 8, base_family="Helvetica"){
  theme_bw(base_size, base_family) %+replace%
    theme(axis.line = element_line(colour = "black"),
          panel.background = element_blank(), 
          panel.border = element_blank(), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.background = element_blank(), 
          strip.background = element_blank() ,
          legend.background = element_blank(),
          legend.key = element_blank()
    )
}




#' Draw a plot based on a gtable
#'
#' @param x a gtable or other object that can be passed to grid.draw
#' @return draws a plot
#' @export
draw_plot<-function(x){
  grid::grid.newpage()
  grid::grid.draw(x)
}

#' Unclip panel in a ggplot
#'
#' @param gg a gggplot
#' @return grob
#' @export
unclip<-function(gg){
  grob <- ggplotGrob(gg)
  grob$layout$clip[grob$layout$name=="panel"] <- "off"
  return(grob)
}

#' Format numbers in scientific notation
#'
#' @param l list of numbers
#' @return expression with formatted numbers
#' @seealso \code{\link{pretty_scale_format}}
#' @export
pretty_scientific <- function(l) {
  # format as scientific
  l <- format(l, nsmall = 0, scientific = TRUE)
  # remove + sign
  l <- gsub("+", "", l, fixed=T)
  # break into prefix and suffix
  pre <- sapply(l, function(x) substr(x,1,gregexpr("e",x)[[1]][1]-1))
  post <- format(as.numeric(sapply(l, function(x) substr(x,gregexpr("e",x)[[1]][1]+1,nchar(x)))))
  # combine prefix and suffix with plotmath
  out <- sapply(1:length(l), function(x) paste(pre[x],"%*%10^",post[x],sep="",collapse=""))
  out[which(pre=="")]=NA
  # return as expression
  return(parse(text=out))
}

#' Determine order of magnitude
#'
#' @param x number
#' @return numeric
#' @export
order_of_magnitude <- function(x){
  if (x==0){
    return(0)
  }
  else if (x< 0){
    x = -1 * x
  }
  return(floor(log10(x)))
}

#' Format numbers in pretty manner
#'
#' @param l list of numbers
#' @return expression with formatted numbers
#' @seealso \code{\link{pretty_scientific}}
#' @export
pretty_scale_format <- function(l){
  digits = max(sapply(l,order_of_magnitude)) - order_of_magnitude(min(diff(l))) + 1
  l = signif(l, digits = digits)
  if (max(sapply(l,order_of_magnitude))>3){
    return(pretty_scientific(l))
  }
  else{return(format(l, nsmall = 0))}
}
