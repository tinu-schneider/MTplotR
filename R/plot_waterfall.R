


#' Plot den Wasserfall CH
#'
#' @param daten Die präparierten Daten. Wir erwarten das Total Vorjahr in der ersten
#'     und das Total Bezugsjahr in der letzten Zeile
#' @param farben Die Farben können hier angegeben werden; die Anzahl muss genau stimmen
#' @param shift Dieser Wert bestimmt, um wieviel die Grafik um die grossen Werte vertikal zentriert wird.
#'
#' @return Ein `ggplot2`-Objekt, das anschliessend mit `print()` geplottet werden kann.
#'
#' @import waterfalls
#' @import ggplot2
#'
#' @export
#'
plot_waterfall_ch <- function(daten, farben = NULL, y_min = NULL, shift = 0.003) {

    if (is.null(farben)) {
        farb <- .gib_farben_wf()

        farbe_total <- farb["total"]
        farben_alle <- ifelse(daten[, "values"] > 0, farb["zunahme"], farb["abnahme"])
        farben_alle <- farben_alle[-c(1, length(farben_alle)) ] # ohne erste und letzte
        farben      <- c(farbe_total, farben_alle, farbe_total)
    }

    if (is.null(y_min)) {
        y_min <- min(daten[c(1, 5), "values"]) * (1 - shift)
    }
    # y_min <- min(daten$values) * (1 - shift)

    totals <- daten[c(1, 5), "values"]

    p <- waterfalls::waterfall(daten[1:4, ],  # ohne Endwert, wird berechnet
                               calc_total = TRUE,
                               total_axis_text = daten[5, "labels"],
                               fill_colours = farben,
                               fill_by_sign = FALSE,
                               total_rect_color = farben[1],
                               theme_text_family = "Roboto",
                               linetype = 1
                               ) +
        labs(x = NULL, y = NULL,
             subtitle = "Tonnen") +
        coord_cartesian(ylim = c(y_min, max(daten$values) * (1 + shift))) +
        scale_y_continuous(labels = scales::format_format(big.mark = "'", scientific = FALSE)) +
        theme_vbsa()

    if (nrow(daten) < 10) {
        labels <- prettyNum(totals, big.mark = "'")
        p <- p +
            annotate("text",
                     x = c(1, 5),
                     y = totals * (1 + shift / 10) ,
                     label = labels,
                     family = "Roboto",
                     size = 3.0
                    )
    }
    p
}






#' Der Waterfall- oder Kaskadenplot aller Anlagen der Schweiz
#'
#'
#' @param daten Die präparierten Daten mit `prep_daten_waterfall_alle`
#' @param bezugsjahr Das aktuelle Bezugsjahr
#' @param y_min Der minimale Wert der y-Achse
#' @param y_max  Der maximale Wert der y-Achse
#' @param farben (optional) Die Farben für den Plot. Die Anzahl muss exakt mit der Anzahl der Säulen übereinstimmen.
#'
#' @import waterfalls
#' @import ggplot2
#'
#' @export
#'
plot_waterfall_alle <- function(daten, bezugsjahr, y_min = NULL, y_max = NULL, farben = NULL) {


    if (is.null(farben)) {
        farb <- .gib_farben_wf()
        farben_alle <- ifelse(daten[, "values"] > 0, farb["zunahme"], farb["abnahme"])
        farben_alle <- farben_alle[-1] # ohne erste und letzte
        farben      <- c(farb["total"], farben_alle)
    }

    if (is.null(y_min)) {
        y_min <- 3.99e6 # min(daten[c(1, 5), "values"]) * (1 - shift)
    }
    if (is.null(y_max)) {
        y_max <- max(daten$values) * (1 + 0.01)
    }



    anz_anlagen <- nrow(daten) - 1 # ohne Total Vorjahr





    p <- waterfalls::waterfall(daten,  # ohne Endwert, wird berechnet
                               calc_total = TRUE,
                               total_axis_text = paste("CH", bezugsjahr),
                               fill_colours = farben,
                               fill_by_sign = FALSE,
                               total_rect_color = farben[1] ,
                               rect_text_labels = rep("", 31),
                               theme_text_family = "Roboto",
                               linetype = 1
                               ) +
        labs(x = NULL, y = NULL,
             subtitle = "Tonnen") +
        coord_cartesian(ylim = c(y_min, y_max)) +
        scale_y_continuous(labels = scales::format_format(big.mark = "'", scientific = FALSE)) +
        theme_vbsa() +
        theme(panel.grid.minor   = element_blank(),
              panel.grid.major.x = element_blank(),
              axis.text.x  = element_text(family = "Roboto Mono",
                                          angle = 90,
                                          vjust = 0.4,
                                          hjust = 1)
        )



    # x und y für die vertikalen Linien basteln;
    # Aus `waterfall` kommen 2 * Anzahl Säulen.
    # Wir müssen die Hälfte auf NA setzen.

    anz_saulen <- as.integer(nrow(daten) + 1 )
    ohne_xy    <- rep(c(TRUE, FALSE), anz_saulen)

    x_linien <- seq(0.5, anz_saulen, by = 0.5)
    x_linien <- ifelse(ohne_xy, NA, x_linien)

    # die y-Werte können wir aus dem ggplot2-objekt ziehen
    y_dat <- p$data$y
    y1 <- y_dat[1:anz_saulen]
    y2 <- y_dat[anz_saulen:length(y_dat)]
    y  <- numeric()

    for (i in 1:(anz_saulen - 1)) {
        y[i] <- min(y1[i], y2[i])
    }
    y <- c(y, NA)
    y_double <- rep(y, each = 2)
    y_werte <- ifelse(ohne_xy, NA, y_double)

    # daten für x und y in data.frame für den plot
    dat_linien <- data.frame(x = x_linien, y = y_werte)

    p <- p +
        geom_segment(
            data = dat_linien,
            aes(x = x,
                xend = x,
                y = rep(0, 64),
                yend = y),
            size = 0.5,
            color = "darkgray",
            na.rm = TRUE # die Hälfte der Werte sind NA...
        )

    p
}





# library(MTplotR)
# library(ggplot2)
# pfad <- "tests/Monitoringtool_TEST.xlsm"
# roh  <- alle_daten_einlesen(pfad)
# dat  <- alle_daten_aufbereiten(roh)


# dat_wf_alle <- prep_daten_waterfall_alle(dat$alle, "Verbrannte Menge", 2016)
# p <- plot_waterfall_alle(dat_wf_alle, 2016, y_min = 3.85e6, y_max = 4.05e6)

# dat_wf_alle <- prep_daten_waterfall_alle(dat$alle, "Verbrannte Menge", dat$bez)
# p <- plot_waterfall_alle(dat_wf_alle, dat$bez, y_min = 4e6, y_max = 4.05e6)
# print(p)


