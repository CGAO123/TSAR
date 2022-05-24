#' Extract ggplot2 legend
#'
#' A function to enable separation of legends from plots within the TSAR package
#'
#' @param input_plot a ggplot2 object
#' @return two ggplots, one containing the legend and another containg all else.
#' @export
#'

get_legend <- function(input_plot) {
    legend_plot <- input_plot
    legend_plot <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(legend_plot))
    legend <- which(sapply(legend_plot$grobs, function(x) x$name) == "guide-box")
    if (length(legend) == 0) { #Prevents error
        warning("No Legend to Return")
    } else {
        legend <- legend_plot$grobs[[legend]]
        legend_plot <- ggpubr::as_ggplot(legend)
        legend_plot <- legend_plot + ggplot2::theme_void()
        return(legend_plot)
    }
}

