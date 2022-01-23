

#' Der Basis-Plot für die HGT-Korrektur
#'
#' @param daten Die präparierten Daten für diesen Plot (mit `prep_daten_hgt`)
#' @param bezugsjahr Das aktuelle Bezugsjahr
#' @param y_max Der maximale Wert der y-Achse.
#'
#' @return Ein ggplot2-Objekt
#'
#' @import dplyr
#' @import ggplot2
#'
#' @export
#'
plot_hgt_basis <- function(daten, bezugsjahr, y_max) {

    basisjahr <- gib_basisjahr()
    farben    <- MTplotR:::.gib_farben_hgt()

    schriftgroesse <- 3

    daten <- dplyr::filter(daten, Jahr == basisjahr)


    HGT_2010 <- daten$HGT # vertikale Linie
    Abg_2010 <- daten$Abgabe_unabh
    Abg_tot  <- daten$Abgabe_total


    p <- ggplot(daten) +
        # HGT von 2010 vertikal
        geom_vline(xintercept = HGT_2010, size = 1.0, col = farben["basisjahr"]) +
        # Horizontale Linie
        geom_segment(aes(x    = 0,
                         xend = HGT_2010,
                         y    = Abg_2010,
                         yend = Abg_2010),
                     size  = 1,
                     linetype = "dashed") +
        # # alle Linien von HGT = 0 bis HGT
        geom_segment(aes(x    = 0,
                         xend = HGT_2010,
                         y    = Abg_2010,
                         yend = Abg_tot),
                     size  = 2,
                     color = daten$Color) +
        # # alle Punkte bei den aktuellen HGT
        geom_point(aes(x = HGT, y = Abgabe_total),
                   size  = daten$Size,
                   color = daten$Color) +
        # # alle Punkte bei HGT = 0
        geom_point(aes(x = HGT_NULL, y = Abgabe_unabh),
                   size = daten$Size, color = daten$Color) +
        # # x- und y- Achsen strecken
        scale_x_continuous(limits = c(0, max(daten$HGT)*1.30 ),
                           labels = scales::format_format(big.mark = "'", scientific = FALSE)) +
        scale_y_continuous(limits = c(0, y_max ),
                           labels = scales::format_format(big.mark = "'", scientific = FALSE)) +
        # Text HGT-Referenz dazu
        annotate("text",
                 x = HGT_2010*1.02,
                 y = 0,
                 label = "Referenz-HGT 2010",
                 size = schriftgroesse,
                 hjust = 0,
                 vjust = -1) +
        # # Achsen beschriften
        labs(x = "Anzahl Heizgradtage (HGT)", y = NULL,
             subtitle = "Wärmeabgabe, MWh") +
        # # Pfeil HGT-unabhängiger Teil
        geom_segment(aes(x = 0,
                         xend = 0,
                         y = 0,
                         yend = Abg_2010 * 0.96),
                     size = 2,
                     color = farben["hgt_pfeil"],
                     arrow = arrow(length = unit(0.45, "cm"), type = "open")
        ) +
        # # Beschriftung HGT-Unabhängig
        annotate("text",
                 x = HGT_2010 * 0.02,
                 y = Abg_2010 / 2,
                 size = schriftgroesse,
                 label = "HGT-unabhängige\nWärmeabgabe, 60%\nHGT = 0",
                 hjust = 0) +
        # # Pfeil HGT-abhängiger Teil
        geom_segment(aes(x = HGT_2010,
                         xend = HGT_2010,
                         y = Abg_2010,
                         yend = Abg_tot * 0.96),
                     size = 2,
                     color = farben["hgt_pfeil"],
                     arrow = arrow(length = unit(0.45, "cm"), type = "open")
        ) +
        # # Beschriftung HGT-abhängig
        annotate("text",
                 x = HGT_2010 * 1.02,
                 y = (Abg_tot - Abg_2010) / 2 + Abg_2010 * 0.91,
                 label = paste("HGT-abhängige\nWärmeabgabe, 40%\nHGT =", HGT_2010),
                 size = schriftgroesse,
                 hjust = 0) +
        # # Linie 2010 beschriften
        annotate("text",
                 x = HGT_2010 / 2,
                 y = (Abg_tot - Abg_2010) / 2 + Abg_2010,
                 label = basisjahr,
                 size = schriftgroesse,
                 color = farben["basisjahr"],
                 vjust = -1,
                 hjust =  0.8) +
        theme_vbsa() +
        theme(axis.title.x  = element_text(size = rel(0.7)))
}

# fig_22 <- prep_daten_hgt(dat$ch, dat$bez)
# p <- plot_hgt_basis(fig_22, dat$bez, 4.2e6)
# print(p)








#' Die Wärmeabgabe im Bezugsjahr mit der HGT-korrigierten Version vergleichen
#'
#' @param daten Die präparierten Daten für diesen Plot (mit `prep_daten_hgt`)
#' @param bezugsjahr Das aktuelle Bezugsjahr
#' @param y_max Das Maximum der y-Achsen. Sinnvollerweise identisch mit `y_max` des HGT-Basisplots
#'
#' @return Ein ggplot2-Objekt
#'
#' @import dplyr
#' @import ggplot2
#'
#' @export
#'
plot_hgt_bezugsjahr <- function(daten, bezugsjahr, y_max) {

    basisjahr <- gib_basisjahr()
    farben    <- .gib_farben_hgt()

    schriftgroesse <- 3


    daten <- dplyr::filter(daten, Jahr %in% c(basisjahr, bezugsjahr))


    HGT_2010 <- subset(daten, Jahr == basisjahr)$HGT # vertikale Linie

    x_2010 <- HGT_2010 / 2
    y_2010 <- subset(daten, Jahr == basisjahr)
    y_2010 <- (y_2010$Abgabe_unabh + y_2010$Abgabe_total) / 2

    y_bez  <- subset(daten, Jahr == bezugsjahr)
    y_bez  <- (y_bez$Abgabe_unabh + y_bez$Abgabe_total) / 2


    daten_bez <- dplyr::filter(daten, Jahr == bezugsjahr)
    delta_bez  <- (daten_bez$Abgabe_korr - daten_bez$Abgabe_total) / daten_bez$Abgabe_total * 100
    lab_Delta_bez <- paste(ifelse(sign(delta_bez) < 0, "-", "+"), round(delta_bez, 1), "%")



    p <- ggplot(daten) +
        # HGT von 2010 vertikal
        geom_vline(xintercept = HGT_2010, size = 1.0, col = farben["basisjahr"]) +
        # alle Linien von HGT = 0 bis HGT
        geom_segment(aes(x    = HGT_NULL,
                         xend = HGT,
                         y    = Abgabe_unabh,
                         yend = Abgabe_total),
                     size  = 2,
                     color = daten$Color) +
        # alle Punkte bei den aktuellen HGT
        geom_point(aes(x = HGT, y = Abgabe_total),
                   size  = daten$Size,
                   color = daten$Color) +
        # alle Punkte bei HGT = 0
        geom_point(aes(x = HGT_NULL, y = Abgabe_unabh),
                   size = daten$Size, color = daten$Color) +
        # Linie/Pfeil ab aktuellem HGT zu koorigiertem Wert bei HGT_2010
        geom_segment(data = daten_bez,
                     aes(x = HGT,
                         xend = HGT_2010,
                         y    = Abgabe_total,
                         yend = Abgabe_korr),
                     # linetype = 1,
                     size  = 0.75,
                     color = farben["korr_pfeil"]) +
                     # arrow = arrow(length = unit(0.25, "cm"), type = "open")) +
        # Linie/Pfeil vom koorigiertem Wert bei HGT_2010 zur y-Achse
        geom_segment(data = daten_bez,
                     aes(x = HGT_2010,
                         xend = 0,
                         y    = Abgabe_korr,
                         yend = Abgabe_korr),
                     # linetype = 1,
                     size  = 0.75,
                     color = farben["korr_pfeil"],
                     arrow = arrow(length = unit(0.25, "cm"), type = "open")) +
        # Linie/Pfeil vom koorigiertem Wert bei HGT_2010 zur y-Achse
        geom_segment(data = daten_bez,
                     aes(x = HGT,
                         xend = 0,
                         y    = Abgabe_total,
                         yend = Abgabe_total),
                     linetype = 2,
                     size  = 0.75,
                     color = farben["bezugsjahr"]) +
        # x- und y- Achsen strecken
        scale_x_continuous(limits = c(0, max(daten$HGT)*1.30 ),
                           labels = scales::format_format(big.mark = "'", scientific = FALSE)) +
        scale_y_continuous(limits = c(0, y_max ),
                           labels = scales::format_format(big.mark = "'", scientific = FALSE)) +
        # Achsen beschriften
        labs(x = "Anzahl Heizgradtage (HGT)", y = NULL,
             subtitle = "Wärmeabgabe, MWh") +
        # Wert vom korrigierten Bezugsjahr rechts dazu
        geom_label(aes(
                    x = HGT_2010*1.02,
                    y = daten_bez$Abgabe_korr,
                    label = paste("Wärmeabgabe", bezugsjahr, "HGT-korrigiert:", lab_Delta_bez)),
                 fill = "#F7F7F7",
                 # linetype = NA,
                 size = schriftgroesse + 0.25,
                 label.size = NA,
                 hjust = 1.15,
                 vjust = -0.15) +
        # Text HGT-Referenz dazu
        annotate("text",
                 x = HGT_2010*1.02,
                 y = 0,
                 label = "Referenz-HGT 2010",
                 size = schriftgroesse,
                 hjust = 0,
                 vjust = -1) +
        # Linie 2010 beschriften
        annotate("text",
                 x = x_2010,
                 y = y_2010,
                 label = basisjahr,
                 size  = schriftgroesse,
                 color = farben["basisjahr"],
                 vjust = 1.25,
                 hjust = -0.5) +
        # Linie Bezugsjahr beschriften
        annotate("text",
                 x = x_2010,
                 y = y_bez,
                 label = bezugsjahr,
                 size = schriftgroesse,
                 vjust = -0.8,
                 hjust = 1.75,
                 # hjust = 2, # zB für 2014, dh das muss ein Parameter sein.
                 color = farben["bezugsjahr"]) +
        theme_vbsa() +
        theme(axis.title.x  = element_text(size = rel(0.7)))

    p
}


# dp <- prep_daten_hgt(dat$ch, dat$bez)
# p <- plot_hgt_basis(dp, dat$bez, 4.2e6)
# print(p)


# fig_23 <- prep_daten_hgt(dat$ch, dat$bez)
# p <- plot_hgt_bezugsjahr(fig_23, dat$bez, 4.2e6)
# print(p)


