
#' Plot spezifische Grössen bzgl. dem verbrannten Abfall
#'
#' @param daten Eine Liste mit Daten, Labels, Titel, Legendentitel und den Farben
#'
#' @return Ein `ggplot2-`Objekt, es wird _nicht_ geplottet
#'
#' @examples
#' # energie <- list(
#' #     dat    = filter(dat_spezifisch, Spezifisch %in% c("spez_waerme", "spez_strom")),
#' #     labels = c("Strom", "Wärme"),
#' #     titel  = "kWh pro Tonne Abfall",
#' #     fill   = "Spezifische\nEnergieabgabe",
#' #     farben = col_vbsa(c("orange", "dunkelrot"))
#' # )
#' # plot_spezifisch(energie)
#'
#' @import ggplot2
#'
#' @export
#'
plot_spezifisch <- function(daten) {
    text_dunkelgrau <- "#303030"

    # für die Linien
    bar_width <- 0.40 # mit x_0 und spacer abstimmen!
    x_0    <-  1.21
    spacer <-  0.58

    dat_text   <- .create_text_points_spez(daten$dat)
    dat_points <- .create_dat_points_spez(daten$dat, x_0, spacer)

    p <- ggplot(daten$dat, aes(x = Jahr, y = Wert)) +
        geom_bar(aes(fill = Spezifisch),
                 position = "stack",
                 width    = bar_width,
                 stat = "identity") +
        geom_text(aes(x = x,
                      y = y,
                      label = signif(Wert, digits = 3)),
                  data = dat_text,
                  stat = "identity",
                  size = 3.25,
                  color = text_dunkelgrau,
                  family = "Roboto") +
        geom_segment(aes(x = x1,
                         y = y1,
                         xend = x2,
                         yend = y2),
                     data = dat_points,
                     color = rev(daten$farben), size = 1.0) +
        scale_fill_manual(values = daten$farben,
                          labels = daten$labels) +
        scale_y_continuous(labels = scales::format_format(big.mark = "'", scientific = FALSE)) +
        theme_vbsa() +
        labs(x = NULL,
             y = NULL,
             subtitle = daten$titel,
             fill  = daten$fill) +
        theme(legend.title = element_text(size = rel(0.7), color = "#303030")) +
        theme(text = element_text(colour = "#303030",
                                  # size = 14,
                                  family = "Roboto"))

    p
}


# helpers
.create_text_points_spez <- function(daten) {

    y1 <- filter(daten, Jahr == BASISJAHR)$Wert[2]
    y2 <- filter(daten, Jahr == BASISJAHR)$Wert[1]

    y3 <- filter(daten, Jahr == BEZUGSJAHR)$Wert[2]
    y4 <- filter(daten, Jahr == BEZUGSJAHR)$Wert[1]

    res <- tribble(
        ~x,  ~y,       ~Wert,
        1,  y1/2,      y1,
        1,  y1 + y2/2, y2,
        2,  y3/2,      y3,
        2,  y3 + y4/2, y4)
    res
}

.create_dat_points_spez <- function(daten, x_0, spacer) {

    y11 <- filter(daten, Jahr == BASISJAHR)$Wert[2]
    y12 <- filter(daten, Jahr == BEZUGSJAHR)$Wert[2]
    y21 <- filter(daten, Jahr == BASISJAHR)$Wert[1] + y11
    y22 <- filter(daten, Jahr == BEZUGSJAHR)$Wert[1] + y12

    res <- tribble(
        ~x1,  ~x2,          ~y1, ~y2,
        x_0,  x_0 + spacer, y11, y12,
        x_0,  x_0 + spacer, y21, y22)
    res
}



