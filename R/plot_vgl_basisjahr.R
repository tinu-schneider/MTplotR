




#' Plotte den Vergleich aller Anlagen zwischen Basisjahr und Bezugsjahr
#'
#' Für jede Anlage werden Balken (horizontal) geplottet.
#'
#'
#' @param daten Die präparierten Daten; wir erwarten die drei Spalten `Kurz`, `Jahr`, `Wert`
#' @param farben Ein Vektor mit den Farben für die Jahre
#' @param x_label optional, Default = `NULL`; die Achsen sind vertauscht, dh x (Anlagen) ist links
#' @param y_label optional, Default = `NULL`; die Achsen sind vertauscht, dh y (Werte) ist unten
#' @param caption_lab optinal,  Default = `NULL`
#'
#' @return Ein `ggplot2`-Objekt, das geprintet werden kann.
#'
#' @import ggplot2
#'
#' @export
#'
plot_vgl_basisjahr <- function(daten,
                      farben,
                      x_label = NULL,
                      y_label = NULL,
                      caption_lab = NULL) {

    jahre <- unique(daten$Jahr)

    breite_bars <- 0.65

    daten <- .erstelle_tooltips_vgl(daten, y_label)

    p <- ggplot(daten) +
            # geom_bar(stat = "identity",
            #          width = breite_bars,
            #          position = "dodge") +
            geom_bar_interactive(aes(x = Kurz, y = Wert, fill = Jahr, tooltip = TT),
                                 stat = "identity",
                                 width = breite_bars,
                                 position = "dodge") +
            labs(x = x_label, y = NULL,
                 subtitle = y_label) +
            scale_fill_manual(values = farben,
                              labels = paste(" ", rev(jahre))) +
            coord_flip() +
            guides(fill = guide_legend(reverse = TRUE)) +
            scale_y_continuous(labels = scales::format_format(big.mark = "'", scientific = FALSE)) +
            theme_vbsa() +
            theme(legend.title = element_blank(),
                  legend.key.size = unit(1.5, 'lines'),
                  axis.text.y = element_text(family = "Roboto Mono")
                  )


    # caption?
    if (!is.null(caption_lab)) {
        p <- p + labs(caption = caption_lab )
    }

    p
}

