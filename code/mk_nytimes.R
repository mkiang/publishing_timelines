library(ggplot2)

#' A theme I often use -- NYTimes variation
#'
#' @param ... parameters to pass to theme()
#'
#' @return None
#' @export
#' @import ggplot2

mk_nytimes <- function(...) {
    ## http://minimaxir.com/2015/02/ggplot-tutorial/
    ## paste0('https://timogrossenbacher.ch/2016/12/',
    ##        'beautiful-thematic-maps-with-ggplot2-only/')
    ## https://github.com/hrbrmstr/hrbrthemes/blob/master/R/theme-ipsum.r
    
    ## Colos — stick with the ggplot2() greys
    c_bg    <- "white"
    c_grid  <- "grey80"
    c_btext <- "grey5"
    c_mtext <- "grey30"
    
    # Begin construction of chart
    ggplot2::theme_bw(base_size = 12, base_family = "Arial Narrow") +
        
        # Region
        ggplot2::theme(
            panel.background = ggplot2::element_rect(fill = c_bg, color = c_bg),
            plot.background  = ggplot2::element_rect(fill = c_bg, color = c_bg),
            panel.border     = ggplot2::element_rect(color = c_bg)
        ) +
        
        # Grid
        ggplot2::theme(
            panel.grid.major = ggplot2::element_line(color = c_grid, size = .25),
            panel.grid.minor = ggplot2::element_blank(),
            axis.ticks       = ggplot2::element_blank()
        ) +
        
        # Legend
        ggplot2::theme(
            legend.position = c(0, 1),
            legend.justification = c(0, 1),
            legend.key           = ggplot2::element_rect(fill = NA, color = NA),
            legend.background    = ggplot2::element_rect(fill = "transparent", color = NA),
            legend.text          = ggplot2::element_text(color = c_mtext)
        ) +
        
        # Titles, labels, etc.
        ggplot2::theme(
            plot.title     = ggplot2::element_text(
                color = c_btext,
                vjust = 1.25,
                face = "bold",
                size = 18
            ),
            axis.text      = ggplot2::element_text(size = 10, color = c_mtext),
            axis.title.x   = ggplot2::element_text(
                size = 12,
                color = c_mtext,
                hjust = 1
            ),
            axis.title.y   = ggplot2::element_text(
                size = 12,
                color = c_mtext,
                hjust = 1
            )
        ) +
        # Facets
        ggplot2::theme(
            strip.background = ggplot2::element_rect(fill = c_bg, color = c_btext),
            strip.text = ggplot2::element_text(size = 10, color = c_btext)
        ) +
        
        # Plot margins
        ggplot2::theme(plot.margin = ggplot2::unit(c(0.35, 0.2, 0.3, 0.35), "cm")) +
        
        # Additionals
        ggplot2::theme(...)
}
