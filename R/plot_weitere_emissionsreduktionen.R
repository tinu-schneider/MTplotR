
#' Die weiteren Emissionsreduktionen plotten
#'
#' @param daten Die aufbereiteten Daten
#' @param titel Der Titel der Grafik (bzw. die Beschriftung der y-Achse)
#' @param alpha Alpha (Transparenz) einstellen, default = 0.5
#'
#' @return Ein `ggplot2-`Objekt, das nicht geplottet wird.
#'
#' @import ggplot2
#'
#' @export
#'
plot_weitere_emissionsreduktionen <- function(daten, titel, alpha = 0.6) {
    p <- ggplot(daten, aes(x = Jahr, y = Wert)) +
        geom_bar(aes(fill = Kurz),
                 stat = "identity",
                 color = "gray30",
                 alpha = alpha) +
        scale_y_continuous(labels = scales::format_format(big.mark = "'", scientific = FALSE)) +
        # scale_fill_brewer(type = "qual", palette = 2) + # "Dark2") +
        theme_vbsa() +
        labs(x = NULL,
             y = NULL,
             fill  = NULL,
             subtitle = titel
        ) +
    theme(legend.text = element_text(size = rel(0.6))) +
    theme(text = element_text(colour = "#303030",
                              # size = 14,
                              family = "Roboto"))
    p
}

# p <- plot_weitere_emissionsreduktionen(dat_weitere, titel)
# print(p)