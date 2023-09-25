#' Extract ggplot2 legend
#'
#' Function enables separation of legends from plots within the TSAR package.
#'
#' @import ggplot2
#' @param input_plot a ggplot2 object
#' @return two ggplots, one containing the legend and another containing
#'   all else.
#' @family TSA Plots
#' @export
#'
#' @examples
#' data("example_tsar_data")
#' boxplot <- TSA_boxplot(example_tsar_data,
#'     color_by = "Protein",
#'     label_by = "Ligand", separate_legend = FALSE
#' )
#' get_legend(boxplot)
#'
get_legend <- function(input_plot) {
    #obtain plot
    legend_plot <- input_plot
    legend_plot <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(legend_plot))
    legend <- which(vapply(legend_plot$grobs, function(x) x$name,
        FUN.VALUE = character(1)
    )
    == "guide-box")
    #obtain legend only if it exists
    if (length(legend) == 0) { # Prevents error
        warning("No Legend to Return")
    } else {
        legend <- legend_plot$grobs[[legend]]
        legend_plot <- ggpubr::as_ggplot(legend)
        legend_plot <- legend_plot + ggplot2::theme_void()
        return(legend_plot)
    }
}
