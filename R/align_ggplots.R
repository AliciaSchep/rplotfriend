# align_plots functions modified from post at http://stackoverflow.com/a/21503904

#' Aligns a set of plots horizontally.
#'
#' @param ... some ggplots (or gtables)
#' @param widths  the width of each plot in output
#' @return If plot is True, then makes plot and returns gtable of plot invisibly.  Else, just returns gtable
#' @seealso \code{\link{align_plots_vert}} and \code{\link{plot_custom_grid}}
#' @export
align_plots_hor <- function (..., widths = NA, plot=T) {

  inputList <- list(...)
  
  if (all(sapply(inputList, inherits,"gg"))){
    grobsList <- lapply(inputList,ggplotGrob)
  }
  else if (all(sapply(inputList, inherits,"gtable"))){
    grobsList <- inputList
  }
  else{stop("Input must be ggplot or gtable")}  
  out <- Reduce(function(x, y) gtable:::cbind_gtable(x, y, "first"), grobsList[-1], grobsList[[1]])
  out$heights <- do.call(grid::unit.pmax, lapply(grobsList, "[[", "heights"))
  if (!is.na(widths[1])){
    out$widths[out$layout$l[grepl("panel", out$layout$name)]] <- lapply(widths, function(x) grid::unit(x,"null"))
  }
  if (plot){
    grid::grid.newpage()
    grid::grid.draw(out)
    invisible(out)
  }
  else{
    return(out)
  }
}

#' Aligns a set of plots vertically.
#'
#' @param ... some ggplots (or gtables)
#' @param heights  the height of each plot in output
#' @return If plot is True, then makes plot and returns gtable of plot invisibly.  Else, just returns gtable
#' @seealso \code{\link{align_plots_hor}} and \code{\link{plot_custom_grid}}
#' @export
align_plots_vert <- function (..., heights = NA, plot = T) {
  inputList <- list(...)
  if (all(sapply(inputList, inherits,"gg"))){
    grobsList <- lapply(inputList,ggplotGrob)
  }
  else if (all(sapply(inputList, inherits,"gtable"))){
    grobsList <- inputList
  }
  else{stop("Input must be ggplot or gtable")}
  out <- Reduce(function(x, y) gtable:::rbind_gtable(x, y, "first"), grobsList[-1], grobsList[[1]])
  out$widths <- do.call(grid::unit.pmax, wl <- lapply(grobsList, "[[", "widths"))
  if (!is.na(heights[1])){
    out$heights[out$layout$t[grepl("panel", out$layout$name)]] <- lapply(heights, function(x) grid::unit(x,"null"))
  }
  if (plot){
    grid::grid.newpage()
    grid::grid.draw(out)
    invisible(out)
  }
  else{
    return(out)
  }
}



#' Make a custom grid of plots
#'
#' @param ... some ggplots (or gtables)
#' @param nrow number of rows
#' @param ncol number of columns
#' @param heights the heights of each row (must correspond to nrow)
#' @param widths  the width of each plot in output (must correspond to ncol)
#' @return If plot is True, then makes plot and returns gtable of plot invisibly.  Else, just returns gtable
#' @seealso \code{\link{align_plots_vert}} and \code{\link{align_plots_hor}}
#' @export
plot_custom_grid <-function(..., nrow = 1, ncol = 1, heights = NA, widths = NA, plot = T){
  inputList <- list(...)
  if (length(inputList)<2){
    print("Need to input 2+ ggplot objects!")
    return(NULL)
  }  
  if (all(sapply(inputList, inherits,"gg"))){
    grobs <- lapply(inputList,ggplotGrob)
  }
  else if (all(sapply(inputList, inherits,"gtable"))){
    grobs <- inputList
  }
  else{stop("Input must be ggplot or gtable")}
  ##Make rows
  rows <- list() 
  for (i in 1:nrow){
    row.plots <- grobs[(1+(i-1)*ncol):((i-1)*ncol+ncol)]    
    row <- row.plots[[1]]
    if (!is.na(widths[1])){
      tmp <- unique(row$layout$l[grepl("panel",row$layout$name)])
      row$widths[tmp] <- list(unit(widths[1],"null"))
    }
    for (j in 2:ncol){
      add <- row.plots[[j]]
      if (!is.na(widths[1])){
        tmp <- unique(add$layout$l[grepl("panel",add$layout$name)])
        add$widths[tmp] <- list(unit(widths[j],"null"))
      }      
      add$layout$l  <- add$layout$l + ncol(row)
      add$layout$r <- add$layout$r + ncol(row)
      row$layout <- rbind(row$layout, add$layout)
      
      row$widths <- gtable:::insert.unit(row$widths, add$widths)
      row$colnames <- c(row$colnames, add$colnames)
      row$heights <- grid::unit.pmax(row$heights, add$heights)
      row$grobs <- append(row$grobs, add$grobs)
    }
    rows[[i]]<-row}
  out <- rows[[1]]
  if (!is.na(heights[1])){
    tmp <- unique(out$layout$t[grepl("panel",out$layout$name)])
    out$heights[tmp] <- list(unit(heights[1],"null"))
  } 
  for (i in 2:nrow){
    row <- rows[[i]]
    if (!is.na(heights[1])){
      tmp <- unique(row$layout$t[grepl("panel",row$layout$name)])
      row$heights[tmp] <- list(unit(heights[i],"null"))
    }    
    row$layout$t <- row$layout$t + nrow(out)
    row$layout$b <- row$layout$b + nrow(out)
    out$layout <- rbind(out$layout,row$layout)
    out$heights <- gtable:::insert.unit(out$heights, row$heights)
    out$widths <- grid::unit.pmax(out$widths, row$widths)
    out$rownames <- c(out$rownames, row$rownames)
    out$grobs <- append(out$grobs, row$grobs)          
  }
  if (plot){
    grid::grid.newpage()
    grid::grid.draw(out)
    invisible(out)
  }
  else{
    return(out)
  }
}
  

#' Force widths of sub-elements of plots to be equal
#'
#' @param ... some ggplots (or gtables)
#' @return list of gtables
#' @seealso \code{\link{align_plots_vert}} and \code{\link{plot_custom_grid}}
#' @export
force_widths_equal<-function(...){
  inputList <- list(...)
  if (all(sapply(inputList, inherits,"gg"))){
    grobsList <- lapply(inputList,ggplotGrob)
  }
  else if (all(sapply(inputList, inherits,"gtable"))){
    grobsList <- inputList
  }
  else{stop("Input must be ggplot or gtable")}  
  newWidths <- do.call(grid::unit.pmax, wl <- lapply(grobsList, "[[", "widths"))
  for (i in 1:length(grobsList)){
    grobsList[[i]]$widths = newWidths
  }
  return(grobsList)
}
  
