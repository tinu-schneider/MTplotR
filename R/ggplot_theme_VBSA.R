
#' ggplot2-Theme for VBSA
#'
#' @param base_size NormalSchriftgrösse
#'
#' @export
#'
theme_vbsa <- function(base_size = 12) {

    bg_hex      <- "#F7F7F7"
    caption_hex <- "#555555"
    grid_hex    <- "#cccccc"

    # Download and install font
    # https://fonts.google.com/specimen/Open+Sans?selection.family=Open+Sans

    font_family       <- "Roboto" # ACHTUNG: für die Waterfall-Charts werden die Fonts dort noch ergänzt

    font_size_axis_text  <- 0.9
    font_size_axis_title <- 0.8

  bg_color <- bg_hex
  bg_rect  <- element_rect(fill = bg_color, color = bg_color)

  theme_bw(base_size) +
    theme(
      text = element_text(family = font_family),
      plot.subtitle = element_text(family = font_family,
                                   size   = rel(0.7),
                                   lineheight = 1),
      plot.caption  = element_text(size = rel(0.5),
                                   margin = unit(c(1, 0, 0, 0), "lines"),
                                   lineheight = 1.1,
                                   color = caption_hex),
      plot.background = bg_rect,
      axis.ticks   = element_blank(),
      axis.text.x  = element_text(size = rel(font_size_axis_text)),
      axis.title.x = element_text(size = rel(font_size_axis_title),
                                  margin = margin(1, 0, 0, 0, unit = "lines")),
      axis.text.y  = element_text(size = rel(font_size_axis_text)),
      axis.title.y = element_text(size = rel(font_size_axis_title)),
      panel.background  = bg_rect,
      panel.border      = element_blank(),
      panel.grid.major  = element_line(color = grid_hex, size = 0.25),
      panel.grid.minor  = element_line(color = grid_hex, size = 0.25),
      panel.spacing     = unit(1.25, "lines") ,
      legend.background = bg_rect,
      legend.key.width  = unit(1.5, "line") ,
      legend.title      = element_text(size = rel(0.8)),
      legend.text       = element_text(size = rel(0.7)),
      legend.key        = element_blank(),
      strip.background  = element_blank()
    )
}

