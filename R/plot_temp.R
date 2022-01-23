






#' Die mittelere Temperatur der Schweiz zeigen
#'
#' @param daten Die aufbereiteten Daten
#' @param y_min Minimum der y-Achse
#' @param y_max Das Maximum der y-Achse
#' @param farben (optional) Zwei Farben: 1. für die Linie (default grün), 2. für den Punkt 2010 (default rot)
#'
#' @import ggplot2
#'
#' @export

plot_temp <- function(daten, y_min, y_max, farben = NULL) {

    if (is.null(farben)) {
        farben <- col_vbsa(c("gruen", "dunkelrot"))
    } else {
        if (length(farben) != 2) stop("Wir erwarten genau 2 Farben in einem Vector.")
    }

    daten <- dplyr::filter(daten, Typ == "year")

    y_2010 <- subset(daten, Jahr == 2010)$Wert

    p <- plot_zr(daten, farben[1],
                 typ   = "linien",
                 y_min = y_min,
                 y_max =  y_max,
                 y_label = "Mittlere Jahrestemperatur, °C"
                )
                 # legend_lab = c("Winter (Oktober--März)", "Ganzes Jahr"))

    p <- p + annotate("point",
                      x = 2010,
                      y = y_2010,
                      colour = farben[2],
                      size = 3) +
         annotate("text",
                   x = 2010,
                   y = y_2010,
                   label = "2010",
                   size = 3.5,
                  color = "gray45",
                  vjust = 1.5)

    p
}


# p   <- plot_temp(dp, y_min = 4, y_max = 7)
# print(p)






## Version mit Winter
# plot_temp <- function(daten, y_min) {
#
#     farben <- c("darkgreen", "darkred")
#
#     y_2010 <- subset(daten, Jahr == 2010 & Typ %in% c("year", "winter"))$Wert
#
#     p <- plot_zr(daten, farben,
#                  typ = "linien",
#                  y_min = y_min,
#                  y_max =  8,
#                  y_label = "Mittlere Temperatur, °C",
#                  legend_lab = c("Winter (Oktober--März)", "Ganzes Jahr"))
#
#     p <- p + annotate("point",
#                       x = c(2010, 2010),
#                       y = y_2010,
#                       colour = "red2",
#                       size = 3) +
#         annotate("text",
#                  x = 2010,
#                  y = y_2010,
#                  label = "2010",
#                  size = 3.5,
#                  color = "gray45",
#                  vjust = 1.5)
#
#     p
# }