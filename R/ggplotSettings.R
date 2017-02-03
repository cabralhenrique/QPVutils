#' This function sets the default settings for ggplots
#'
#' @param boost integer. value by which to increase or decrease font size. Default = 0
#' @param ... other params to be passed to theme()
#' @return settings to be added to a ggplot plot
#' @examples
#'  ggplotSetTheme()
#'  ggplotSetTheme(axis.text.x = element_text(angle = 45, hjust = 1))
#' @author Henrique Cabral
#' @export
ggplotSetTheme <- function(boost = 0,...) {

    ggplot2:: theme_minimal() +
        ggplot2::theme(
            legend.key = ggplot2::element_blank(),
            legend.text = ggplot2::element_text(size = 14 + boost),
            legend.title = ggplot2::element_text(size = 16 + boost),
            axis.text = ggplot2::element_text(size = 18 + boost),
            axis.title = ggplot2::element_text(size = 20 + boost),
            strip.text = ggplot2::element_text(size = 24 + boost),
            plot.title = ggplot2::element_text(size = 24 + boost),
            ...
        )
}

#' this function sets the default colours for fill gradient plots
#'
#' @param return_cols logical. if TRUE, return only colours, not scale_fill_gradient2 object. Default is FALSE
#' @param ... other parameters passed on to scale_fill_gradient2
#' @return scale_fill_gradient2 object with standard colours (red-green)
#' @examples
#'  fillGradientPlot()
#'  fillGradientPlot(name='TITLE')
#' @author Henrique Cabral
#' @export
fillGradientPlot <- function(return_cols = F, ...) {


    # get colours for scale (colorblind friendly)
    cols <- RColorBrewer::brewer.pal(11, 'RdYlBu')[c(1, 6, 11)]

    if (return_cols) {
        return(cols)
    } else {
        # add scale to plot
        return(
            ggplot2::scale_fill_gradient2(
                low = cols[1], mid = cols[2], high = cols[3], ...
        ))
    }


}

