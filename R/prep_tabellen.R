

# library(MTplotR)

# library(extrafont)
# font_import()
# loadfonts(device="win")

# pfad <- "tests/2018-05_Monitoringtool_V.2.3.xlsm"
# roh  <- alle_daten_einlesen(pfad)
# dat  <- alle_daten_aufbereiten(roh)



#' helper für die Tabellen
#'
#' @param daten Die Rohdaten Witterung
#' @param zeilen Die (korrekten) Namen der Zeilen, die gewählt werden sollen
#' @param bezugsjahr Das aktuelle Bezugsjahr bzw. ein Jahr, das in den Daten vorhanden ist.
#'
.prep_tabelle <- function(daten, zeilen, bezugsjahr) {

    # nach GWh umrechnen
    gwh <- function(wert) {
        ifelse(wert > 1e6, wert / 1000, wert)
    }

    dat <- daten %>%
        dplyr::filter(Was %in% zeilen) %>%
        dplyr::select(-Einheit) %>%
        dplyr::mutate_if(is.numeric, gwh) %>%
        dplyr::mutate(Was = factor(Was, levels = zeilen)) %>%
        dplyr::arrange(Was)

    spalte_bezugsjahr <- which(colnames(dat) == bezugsjahr)
    dat <- dat[, 1:spalte_bezugsjahr]
    dat
}



#' Tabelle `hgt_zr` vorbereiten
#'
#' Wir erwarten hier die Rohdaten
#'
#' @param daten Die Rohdaten der Witterung
#' @param bezugsjahr Das aktuelle Bezugsjahr
#'
#' @export
#'
prep_tab_hgt_zr <- function(daten, bezugsjahr) {

    runden <- function(x) {
        ifelse(x > 100, round(x, 0), round(x, 2))
    }

    zeilen <- c("HGT",
                "HGT, indexiert (2010 = 100%)")

    dat <- .prep_tabelle(daten, zeilen, bezugsjahr) %>%
            dplyr::mutate_if(is.numeric, runden)

    zeilen_neu <- c("HGT",
                    "HGT, indexiert")
    dat$Was <- zeilen_neu

    colnames(dat)[1] <- " "
    dat
}
# print(prep_tab_hgt_zr(roh$witt, dat$bez) )



#' Tabelle `waerme_anteil_hgt` vorbereiten
#'
#' Diese Daten sind fix aus dem Resultat Rytec
#' dh sie werden hier von Hand eingegeben und
#' nicht berechnet
#' @param daten Die Rohdaten der Witterung
#'
#' @export
#'
prep_tab_waerme_anteil_hgt <- function(daten) {

    waerme <-  .prep_tabelle(daten, "Wärmeabsatz total", 2015)

    waerme$Was <- "Wärmeabgabe gesamt, GWh"

    # Hier von Hand aus dem Bericht 2016 übertragen
    HGT_unabh <- waerme
    HGT_unabh[1, ] <- list("Davon HGT-unabhängig (%)", 52, 55, 56, 55, 55, 56)
    dat <- dplyr::bind_rows(waerme, HGT_unabh)

    colnames(dat)[1] <- " "
    dat
}
# print(prep_tab_waerme_anteil_hgt_zr(roh$witt) )



#' Tabelle `waerme_hgt_zr` vorbereiten
#'
#' Wir erwarten die Rohdaten
#'
#' @param daten Die Rohdaten der Witterung
#' @param bezugsjahr Das aktuelle Bezugsjahr
#'
#' @export
#'
prep_tab_waerme_hgt_zr <- function(daten, bezugsjahr) {

    zeilen <- c("Wärmeabsatz total",
                "Wärmeabsatz HGT-Abhängig",
                "Wärmeabsatz HGT-Unabhängig")

    dat <- .prep_tabelle(daten, zeilen, bezugsjahr)

    zeilen_neu <- c("Wärmeabgabe gesamt",
                    "HGT-abhängig, 40%",
                    "HGT-unabhängig, 60%")
    dat$Was <- zeilen_neu
    # dat$Anteil <- c(100, 40, 60)
    # dat <- dplyr::select(dat, Was, Anteil, dplyr::starts_with("20"))

    colnames(dat)[1] <- "Wärmeabgabe, GWh"
    dat
}
# print(prep_tab_waerme_hgt_zr(roh$witt, dat$bez))



#' Tabelle `waerme_korr_zr` vorbereiten
#'
#' Wir erwarten die Rohdaten
#'
#' @param daten Die Rohdaten der Witterung
#' @param bezugsjahr Das aktuelle Bezugsjahr
#'
#' @export
#'
prep_tab_waerme_korr_zr <- function(daten, bezugsjahr) {

    runden <- function(x) {
        ifelse(x > 100, as.integer(x), round(x, 2))
    }

    zeilen <- c("Wärmeabsatz total",
                "korr. Wärmeabsatz total",
                "% Diff. gegenüber unkorrigiert W-Abgabe")

    dat <- .prep_tabelle(daten, zeilen, bezugsjahr) %>%
            dplyr::mutate_if(is.numeric, runden)

    zeilen_neu <- c("Gesamt",
                    "HGT-korrigiert",
                    "Differenz (%)")
    dat$Was <- zeilen_neu

    colnames(dat)[1] <- "Wärmeabgabe, GWh"
    dat
}
# print(prep_tab_waerme_korr_zr(roh$witt, dat$bez))



#' Tabelle `co2n_hgt_korr_zr` vorbereiten
#'
#' Wir erwarten die Rohdaten
#'
#' @param daten Die Rohdaten der Witterung
#' @param bezugsjahr Das aktuelle Bezugsjahr
#'
#' @export
#'
prep_tab_co2n_hgt_korr_zr <- function(daten, bezugsjahr) {

    formatieren <- function(x) {
        formatC(x, digits = 3, format = "fg")
    }

    zeilen <- c("Zielpfad Mio. Tonnen",
                "Netto-CO2-Emissionen",
                "Netto-CO2-Emissionen, HGT-korrigiert")

    dat <- .prep_tabelle(daten, zeilen, bezugsjahr) # %>%
        # dplyr::mutate_if(is.numeric, formatieren)

    zeilen_neu <- c("Zielpfad",
                    "Netto-Em.",
                    "Netto-Em., HGT-korr.")
    dat$Was <- zeilen_neu

    colnames(dat)[1] <- " "  # "CO~2~-Emission, Mio. t CO~2-eq~"
    dat

}
# print(prep_tab_co2n_hgt_korr_zr(roh$witt, dat$bez))





















