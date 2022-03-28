

#' Title
#'
#' @param daten Die aufbereiteten Daten von `prep_daten_co2_alle_boni`
#' @param farben (optional) Ein Vektor mit 6 Farben, default siehe `gib_farben_co2_boni`
#' @param y_legend_pos (0.35) Ein Wert zwischen 0 und 1. Er gibt die Höhe der Legende an,
#'     so dass sie exakt auf die 0-Linie ausgerichtet werden kann. Die Wahl des Wertes ist
#'     abhängig von der Grösse der Grafik, dh muss/kann von Hand angepasst werden.
#'
#' @return Ein ggplot2-Objekt
#'
#' @export
#'
plot_co2_boni <- function(daten, farben = NULL, y_legend_pos = 0.3) {

    if (is.null(farben)) {
        farben <- gib_farben_co2_boni()
    }

    # levels_typ <- levels(daten$Typ) %>%  print()


    dat <- daten %>%
        mutate(Typ = as.character(Typ)) %>%
        mutate(Typ = replace(Typ, Typ == "CO2-Emissionen Fossil (brutto)",
                             "brutto")) %>%
        mutate(Typ = replace(Typ, Typ == "Bonus CO2 Wärme negativ",
                             "bonus_waerme")) %>%
        mutate(Typ = replace(Typ, Typ == "Bonus CO2 Strom negativ",
                             "bonus_strom")) %>%
        mutate(Typ = replace(Typ, Typ == "Bonus CO2 Metalle negativ",
                             "bonus_metall")) %>%
        mutate(Typ = replace(Typ, Typ == "Malus", "malus")) %>%
        mutate(Typ = replace(Typ, Typ == "Netto-CO2 Emissionen nach Malus",
                             "netto")) %>%
        mutate(Wert = Wert / 1e6)

    # farb <- tibble::rownames_to_column(as.data.frame(farben), "Typ")
    # dat <- left_join(dat, farb, by = "Typ") %>% print()

    # anz_typ <- length(unique(daten$Typ))
    levels_legend <-  c("malus",
                        "brutto",
                        "bonus_waerme",
                        "bonus_strom",
                        "bonus_metall")

    dat_bars <- filter(dat, Typ != "netto") %>%
        mutate(Typ = factor(Typ, levels = c("malus",
                                            "brutto",
                                            "bonus_metall",
                                            "bonus_strom",
                                            "bonus_waerme"
        ))) # ab 0 wird die skala von ggplot gedreht....

    dat_line <- filter(dat, Typ == "netto")

    # labels für die Legende

    p <- ggplot(dat_bars, aes(Jahr, Wert, order = Typ)) +
        geom_bar(aes(x = Jahr, y = Wert, fill = Typ),
                 stat = "identity",
                 width = 0.45,
                 position = "stack") +
        geom_hline(yintercept = 0) + # Null-Linie
        geom_line(data = dat_line,
                  aes(x = Jahr, y = Wert, group = 1, color = Typ),
                  size  = 1.0) +
        geom_point(data = dat_line,
                   aes(color = Typ),
                   size  = 4) +
        scale_y_continuous(labels = scales::format_format(big.mark = "'", scientific = FALSE)) +
        scale_fill_manual(values = farben,
                          breaks = levels_legend,
                          labels = c(" Malus   ",
                                     expression(paste(" Brutto CO"['2'],  "-Emission")),
                                     " Bonus Wärme   ",
                                     " Bonus Strom   ",
                                     " Bonus Metalle   ")
        ) +
        scale_color_manual(values = farben["netto"],
                           labels = expression(paste(" Netto-CO"['2'],  "-Emission"))) +
        guides(color = guide_legend(override.aes = list(size = 1.25))) +
        labs(x = NULL, y = NULL,
             subtitle = expression("Mio. Tonnen CO"["2-eq"])) +
        theme_vbsa() +
        theme(legend.title = element_blank(),
              legend.position = "right",
              legend.justification = c(0, y_legend_pos),
              legend.text.align = 0
        )

    # labels x-Achse
    x_labels <- .gib_x_labels_zr(daten)
    x_breaks <- .gib_x_breaks_zr(daten)
    x_min_breaks <- min(daten$Jahr):2035

    p <- p +
        scale_x_continuous(breaks = x_breaks,
                           labels = x_labels,
                           minor_breaks = x_min_breaks)

    p

}

# dp <- prep_daten_co2_alle_boni(dat$ch, dat$bez)
# p <- plot_co2_boni(dp, y_legend_pos = 0.35)
# print(p)

