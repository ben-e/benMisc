#' range01 takes a vector and maps it in to a 0-1 range.
#'
#' @export
range01 <- function(x) {
  (x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))
}

#' vscale is a wrapper around scale that takes a vector and returns a vector.\
#'
#' @export
vscale <- function(x, center = T, scale = T) {as.vector(scale(x, center, scale))}

#' pca_scatter is used to generate plots to visualize the results of a PCA.
#'
#' @export
pca_scatter <- function(df_name, index_name, pca) {
  require(ggplot2)
  vars <- all.vars(pca$call$formula)
  plots <- paste0("qplot(data = ", df_name,", x = ", index_name,", y = ", vars,")")
  return(lapply(plots, FUN = function(x) eval(parse(text = x))))
}

#' gpca_scatter returns the results of pca_scatter as a single plot.
#'
#' @export
gpca_scatter <- function(df_name, index_name, pca) {
  require(gridExtra)
  grid.arrange(grobs = pca_scatter(df_name, index_name, pca))
}

#' multiplot is a function which allows for multiple plots on a page
#'
#' ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
#' - cols:   Number of columns in layout
#' - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#'
#' If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
#' 3 will go all the way across the bottom.
#'
#' Source: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#'
#' @export
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#' Convert p-value to stars where:
#' < 0.10 is *, < 0.05 is **, and < 0.01 is ***
#' @export
p_to_stars <- function(p) {
  require(dplyr)
  case_when(
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.1 ~ "*",
    p >= 0.1 ~ ""
  )
}

#' Get lower triangle of matrix.
#' @export
get.lower.tri <- function(cormat) {
  # http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
#' Get upper triangle of matrix.
#' @export
get.upper.tri <- function(cormat) {
  # http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

