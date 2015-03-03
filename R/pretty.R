#' Theme for scientific publication
#'
#' @param base_size
#' @param base_family
#' @return ggplot2 theme
#' @export
pub_theme <-function(base_size = 8, base_family="Helvetica"){
  theme(line = element_line(colour = "black", size = 0.5, linetype = 1, 
                            lineend = "butt"), 
        rect = element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1), 
        text = element_text(family = base_family, face = "plain", colour = "black", size = base_size, hjust = 0.5, 
                            vjust = 0.5, angle = 0, lineheight = 0.9), 
        strip.text = element_text(size = rel(0.8)), 
        axis.line = element_line(colour = "black"), 
        axis.text = element_text(size = rel(0.8), colour = "black"),
        axis.text.x = element_text(vjust = 1), 
        axis.text.y = element_text(hjust = 1), 
        axis.ticks = element_line(colour = "black"), 
        axis.title.x = element_text(), 
        axis.title.y = element_text(angle = 90), 
        axis.ticks.length = unit(0.15, "cm"), 
        axis.ticks.margin = unit(0.1, "cm"), 
        legend.background = element_rect(colour = NA), 
        legend.margin = unit(0.2, "cm"), 
        legend.key = element_blank(), 
        legend.key.size = unit(1.2, "lines"), 
        legend.key.height = NULL, 
        legend.key.width = NULL, 
        legend.text = element_text(size = rel(0.8)), 
        legend.text.align = NULL, 
        legend.title = element_text(size = rel(0.8), face = "bold", hjust = 0), 
        legend.title.align = NULL, 
        legend.position = "right", 
        legend.direction = NULL, 
        legend.justification = "center", 
        legend.box = NULL, 
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.margin = unit(0.25, "lines"), 
        panel.margin.x = NULL, 
        panel.margin.y = NULL, 
        strip.background = element_blank(), 
        strip.text.x = element_text(), 
        strip.text.y = element_text(angle = -90), 
        plot.background = element_blank(), 
        plot.title = element_text(size = rel(1.2)), 
        plot.margin = unit(c(0.5, 0.5, 0.25, 0.25), "lines"), complete = TRUE)
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
  digits = order_of_magnitude(max(l)) - order_of_magnitude(min(diff(l))) + 2
  l = signif(l, digits = digits)
  if (max(l)>1000){
    return(pretty_scientific(l))
  }
  else if (max(l)<0.001){
    return(pretty_scientific(l))
  }
  else{return(format(l, nsmall = 0))}
}
