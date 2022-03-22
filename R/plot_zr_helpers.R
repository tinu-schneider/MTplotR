

#' Bars
#'
#' @param daten Die Daten
#' @param farben Die Farben
#' @param legend_lab (optional) Die Labels
#'
#' @import ggplot2
#' @import ggiraph
#'
.plot_zr_bars <- function(daten,
                         farben,
                         legend_lab  = NULL) {

    breite_bars <- ifelse(length(unique(daten$Typ)) == 1, 0.3, 0.45)

    # Tooltips erstellen, differenziert für die Plots
    daten <- .erstelle_tooltips_zr_bars(daten)

    # plot starten
    p <- ggplot(daten, aes(x = Jahr, y = Wert, fill = Typ, group = Typ))

    # plot ergänzen
    if ("TT" %in% colnames(daten)) { # interactive
        p <- p + geom_bar_interactive(aes(tooltip = TT),
                                      stat     = "identity",
                                      width    = breite_bars,
                                      position = "dodge",
                                      na.rm = TRUE)
    } else {
        p <- p + geom_bar(stat     = "identity",
                          width    = breite_bars,
                          position = "dodge",
                          na.rm = TRUE)
    }

    p
}



#' Punkte
#'
#' @param daten Die Daten
#' @param farben Die Farben
#'
#' @import ggplot2
#' @import ggiraph
#'
.plot_zr_points <- function(daten, farben) {

    groesse_punkte <- 6

    # Tooltips erstellen, differenziert für die Plots
    daten <- .erstelle_tooltips_zr_points(daten)

    # plot starten
    p <- ggplot(daten, aes(x = Jahr, y = Wert, color = Typ))

    # plot ergänzen
    if ("TT" %in% colnames(daten)) { # interactive
        p <- p + geom_point_interactive(aes(tooltip = TT),
                                      size  = groesse_punkte,
                                      na.rm = TRUE)
    } else {
        p <- p + geom_geom_point(size  = groesse_punkte,
                                 na.rm = TRUE)
    }

    p <- p + scale_fill_manual(values = farben)

    p
}


.plot_zr_lines <- function(daten, farben, y_min) {

    dicke_linien <- 1

    daten <- .erstelle_tooltips_zr_lines(daten)

    p <- ggplot(daten, aes(x = Jahr,
                           y = Wert,
                           group    = Typ,
                           color    = Typ,
                           linetype = Typ)
                )

    if (y_min < 0) {
        p <- p + geom_hline(yintercept = 0, size = 1.0, col = "gray")  # die 0-Linie zuerst
    }

    p <- p + geom_line(size = dicke_linien, na.rm = TRUE)

    if ("TT" %in% colnames(daten)) { # interactive
        # line_interavtive vermasselt die Tooltips, darum
        # points mit alpha = 0.01
        # https://stackoverflow.com/questions/44017293/tooltip-missing-for-geom-line-interactive-in-ggiraph
        p <- p + geom_point_interactive(aes(tooltip = TT),
                                       size = 10,
                                       alpha = 0.01,
                                       na.rm = TRUE)
    }

    p
}


#' Helper: Die Tooltips für die Bars erstellen
#'
#' @param daten Die aufbereiteten Daten (mit Spalte TT)
#' @return Die ergänzten Daten (Spalte TT)
#' @noRd
.erstelle_tooltips_zr_bars <- function(daten) {

    dat <- daten

    if (any(grepl("Import", daten$Typ))) {
        dat <- .prep_tt_import(daten)
    }

    if (any(grepl("Stromabgabe MWh", daten$Typ))) {
        dat <- .prep_tt_waerme_strom(daten)
    }

    if (any(grepl("Bonus CO2 Strom", daten$Typ))) {
        dat <- .prep_tt_bonus_waerme_strom(daten)
    }

    if (any(grepl("NE", daten$Typ))) {
        dat <- .prep_tt_fe_ne(daten)
    }

    if (any(grepl("Zn", daten$Typ))) {
        dat <- .prep_tt_zn(daten)
    }

    # if (any(grepl("Summe Boni", daten$Typ))) {
    if (any(grepl("Bonus CO2 Metalle", daten$Typ))) {
        dat <- .prep_tt_boni_metalle(daten)
    }

    dat
}



#' Helper: Die Tooltips für die Punkte erstellen
#'
#' @param daten Die aufbereiteten Daten (mit Spalte TT)
#' @return Die ergänzten Daten (Spalte TT)
#' @noRd
.erstelle_tooltips_zr_points <- function(daten) {

    dat <- daten

    if ("Verbrannte Menge" == levels(daten$Typ)) {
        dat <- .prep_tt_verbrannte_menge(daten)
    }

    if ("CO2-Emissionen Fossil (brutto)" == levels(daten$Typ)) {
        dat <- .prep_tt_co2_fossil_brutto(daten)
    }

    dat
}




#' Helper: Die Tooltips für die lININE erstellen
#'
#' @param daten Die aufbereiteten Daten (mit Spalte TT)
#' @return Die ergänzten Daten (Spalte TT)
#' @noRd
.erstelle_tooltips_zr_lines <- function(daten) {

    dat <- daten

    if (any(grepl("dAbfall", daten$Typ))) {
        dat <- .prep_tt_abfall_bip(daten)
    }

    if (identical(levels(daten$Typ), c("Netto-CO2-Emissionen", "Zielpfad Mio. Tonnen"))) {
        dat <- .prep_tt_netto_co2_zielpfad(daten)
    }

    if (any(grepl("year", daten$Typ))) {
        dat$TT <- NA
        dat <- .prep_tt_temperatur(dat)
    }

    if (any(grepl("Netto-CO2 Emissionen, witterungskorrigiert", daten$Typ))) {
        dat <-
            daten %>%
            mutate(Typ = as.character(Typ)) %>%
            mutate(Typ = replace(Typ, grepl("Emissionen nach Abz", daten$Typ), "Netto-CO2 Emissionen nach Abzug"))
        dat <- .prep_tt_netto_co2_zielpfad_hgtkorr(dat)
    }

    if (any(grepl("Szenario", daten$Typ))) {
        dat <- .prep_tt_szenarien(dat)
    }


    dat
}



#' Helper: Gib die Labels für die x-Achse
#'
#' @param daten Die aufbereiteten Daten
#' @param by Schritte der Labels
#'
#' @return Ein numerischer Vector mit den Labels == Brakes
#'
#' @export
#'
.gib_x_Labels_zr <- function(daten, by = 2) {

    x_min <- min(daten$Jahr)

    if ((x_min %% 2) != 0) {
        x_min <- x_min + 1
    }

    x_labels <- sort(c(seq(x_min, 2035, by = 2), BEZUGSJAHR))
    x_labels
}







