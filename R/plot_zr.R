

#' Plotte eine Zeitreihe
#'
#' Wir plotten die vorbereiteten Daten. Achtung: Wenn das Minimum der y-Achse > 0 gewählt wird, werden keine Säulen
#' geplottet, sondern Punkte.
#'
#' @param daten Die vorbereiteten Daten.
#' @param farben Ein Vector mit den Farben (hex oder Namen)
#' @param typ Der Typ der Grafik, default = `bars`. Bisher sind `punkte`, `linien` oder `bars` implementiert.
#' @param x_label Beschriftung x-Achse, Default ist leer
#' @param y_label Beschriftung y-Achse, Default ist leer
#' @param y_min (0) Minimaler Wert der y-Achse, Default = 0
#' @param y_max (NA) Maximaler Wert der y-Achse, Default = max(Werte)*1.05
#' @param legend_lab (nULL) Beschriftung der Legende, ein Vektor mit den Einträgen. Achtung auf die Reihenfolge! Default ist `NULL`, dann gibt es keine Legende.
#' @param caption_lab (NULL) Wir können im Plot unten rechts zB die Herkunft der Daten
#'     (Sheet im Monitoring-Tool) angeben
#'
#' @return Ein ggplot2-Objekt; es wird _nicht_ geplottet.
#'
#' @import ggplot2
#'
#' @export
#'
plot_zr <- function(daten,
                      farben,
                      typ = "bars",
                      x_label = NULL,
                      y_label = NULL,
                      y_min   = 0,
                      y_max   = NA,
                      legend_lab  = NULL,
                      caption_lab = NULL) {

    # bars
    if (typ == "bars") {
        p <- .plot_zr_bars(daten, farben, legend_lab)
    }

    # punkte
    if (typ == "punkte") {
        p <- .plot_zr_points(daten, farben)
    }

    # linien
    if (typ == "linien") {

        anz_typ <- length(unique(daten$Typ))

        p <- .plot_zr_lines(daten, farben, y_min)

        if (anz_typ == 4) {
            p <- p + scale_linetype_manual(values = c(1, 1, 2, 2), guide = "none")
        } else {
            p <- p + scale_linetype_manual(values = rep(1, anz_typ), guide = "none")
        }
    }



    # und alles feintunen
    p <- p + scale_y_continuous(limits = c(y_min, y_max),
                                labels = scales::format_format(big.mark = "'", scientific = FALSE)) +
             theme_vbsa() +
             labs(x = x_label, y = NULL,
                  subtitle = y_label)

    if (!is.null(legend_lab)) {
        p <- p +
            theme(legend.position  = c(0.5, 1.0))  + # "top" macht Abstand zum subtitle, wollen wir nicht!
            theme(legend.key.width = unit(1.0, "line")) +
            theme(legend.text.align = 0) +
            theme(legend.title = element_blank())

    } else {
        p <- p + theme(legend.position = "none")
    }

    # Scales für die Legende
    if (typ == "bars") {
        p <- p + scale_fill_manual(values = farben,
                                   labels = legend_lab,
                                   guide  = guide_legend(direction = "horizontal")
                                )
    } else {
        p <- p + scale_colour_manual(values = farben,
                                     labels = legend_lab,
                                     guide  = guide_legend(direction = "horizontal"))
        }
    # caption?
    if (!is.null(caption_lab)) {
        p <- p + labs(caption = caption_lab )
    }

    p
}



