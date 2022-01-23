
#' Eine Grafik plotten
#'
#' @param grafik Eine ggplot2-Grafik
#' @param hoehe (2.5) Die Höhe der interaktiven Grafik (in inches...), ca. 2/3 von `fig.height` im R-chunk
#' @param interaktiv (TRUE) Soll für die `HTML`-Version eine interaktive Grafik sein?
#' @param testen (FALSE) Damit wir auch ohne `knitr` bwz. `bookdown` die interaktive Version testen können.
#'
#'
#' @import ggiraph
#' @import widgetframe
#'
#' @export
#'
plot_grafik <- function(grafik, hoehe = 2.5, interaktiv = TRUE, testen = FALSE) {

    if (knitr::is_html_output() & interaktiv | testen & interaktiv) {

        padding <- ifelse(hoehe > 4, 5, 10)

        t_css <- paste0("background-color: lightgray;
                    font-family: Roboto, Arial, Helvetica, sans-serif;
                    padding: ", padding, "px;
                    border-radius: 5px;")

        pp <- ggiraph(code = {print(grafik)},
                      # hover_css = h_css,
                      tooltip_opacity = 0.85,
                      # zoom_max = 5,
                      tooltip_extra_css = t_css,
                      tooltip_offx = -60,
                      tooltip_offy = ifelse(hoehe > 4, -38, -75), # vgl Vorjahr weniger Abstand
                      pointsize = 11,
                      height = hoehe,
                      width = 1.0
                     )
        frameWidget(pp, width = "100%")

    } else {
        # PDF
        print(grafik)
    }
}
