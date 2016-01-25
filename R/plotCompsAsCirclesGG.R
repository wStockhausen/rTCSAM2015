#'
#' @title Create or add to a circle plot of age or size comp.s.
#'
#' @description Creates or adds to a circle plot of age or size comp.s.
#'
#'@param dfr - dataframe from which to extract columns
#'@param x - column name with x coordinates of circle centers
#'@param y - column name with y coordinates of circle centers
#'@param z - column name with values to be plotted as circles
#'@param category - column name with values used as categories for plot
#'@param faceting - formula (as string) for faceting (NULL=no faceting)
#'@param title - title for plot
#'@param xlim - x axis limits
#'@param ylim - y axis limits
#'@param xlab - x axis label
#'@param ylab - y axis label
#'@param alpha - alpha level for transparency (0-1)
#'@param ggtheme - ggplot2 theme
#'@param showPlot - flag to show (print) plot immediately on current graphics device
#'@param verbose - flag (T/F) to print dagnostic info
#'
#'@return ggplot2 object
#'
#'@import ggplot2
#'
#'@export
#'
plotCompsAsCirclesGG<-function(dfr,
                               x=NULL,
                               y=NULL,                             
                               z=NULL,
                               category=NULL,
                               faceting=NULL,
                               title=NA,
                               xlim=NULL,
                               ylim=NULL,
                               xlab=NA,
                               ylab=NA,
                               alpha=0.6,
                               ggtheme=theme_grey(),
                               showPlot=TRUE,
                               verbose=FALSE) {
  
    p <- ggplot(aes_string(x=x,y=y,size=z,fill=category),data=dfr);
    p <- p + scale_size_area(max_size=10);
    p <- p + geom_point(alpha=alpha,shape=21,color='black');
    p <- p + geom_point(alpha=1.0,shape=21,color='black',fill=NA);
#    if (!is.null(xlim)) p <- p + xlim(xlim);
#    if (!is.null(ylim)) p <- p + ylim(ylim);
    p <- p + xlab(xlab);
    p <- p + ylab(ylab);
    p <- p + guides(fill=guide_legend(override.aes=list(alpha=1.0,size=6),order=2),
                    size=guide_legend(order=1));
    p <- p + ggtitle(title);
    p <- p + ggtheme;
    p <- p + theme(legend.box='horizontal')
    if (!is.null(faceting)) p <- p + facet_grid(faceting)
    if (showPlot) print(p);

    return(p)
}

