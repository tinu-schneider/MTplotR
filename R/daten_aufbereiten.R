
#' Die Daten aufbereiten
#'
#' Die eingelesenen Rohdaten aufbereiten. Es gibt eine lange Form mit einer Zeile pro Jahr.
#'
#' @param roh Die eingelesenen Rohdaten
#'
#' @return Eine Liste mit Werten fuer die Schweiz und Werten fuer alle Anlagen.
#' * Daten CH `ch`: Alles, was fuer CH gilt, dh alles aus `R_CH` + `Witterung` + BIP etc.
#' * Daten aller Anlagen `alle`: Alles, was aus dem Sheet `R_Alle`, dh alle Anlagen stammt
#' * Bezugsjahr `bez`
#' * Version `vers` Version des Monitoring-Tools
#'
#' @export
#'
alle_daten_aufbereiten <- function(roh) {
    dat <- list()

    dat$ch   <- .daten_ch_aufbereiten(roh)
    dat$alle <- .daten_alle_aufbereiten(roh$alle)
    dat$bez  <- roh$bez
    dat$vers <- roh$vers
    dat$sz   <- roh$sz

    # daten TESTEN: 1) dimensionen, 2) vgl. ch vs ch aus alle
    .pruefe_dimensionen(dat)
    .pruefe_ch_vs_alle(dat)

    dat
}




#' Daten CH aufbereiten
#'
#' Hier werden R_CH, Witterung und BIP zusammengezogen und 'lang' gemacht.
#'
#' @param roh Die Rohdaten
#'
.daten_ch_aufbereiten <- function(roh) {

    ch_roh   <- dplyr::select(roh$ch , -Einheit, -Zeile)
    div_roh  <- dplyr::select(roh$div, -Einheit)
    witt_roh <- dplyr::select(roh$witt, -Einheit)

    dat <- dplyr::bind_rows(ch_roh, witt_roh, div_roh) %>%
            # print(n = 1e6) %>%
            tidyr::gather(Jahr, Wert,  '2010':'2035', -Was)
    dat
}


#' Daten Alle aufbereiten
#'
#' @param roh Die Rohdaten
#'
.daten_alle_aufbereiten <- function(roh) {

    dat <- tidyr::gather(roh, Jahr, Wert,  '2010':'2035', -Was, -Kurz, -Anlage) %>%
            dplyr::mutate(Wert = suppressWarnings(as.numeric(Wert)) ) # we introduce NA's
    dat
}



# Pruefe die Dimensionen
#
.pruefe_dimensionen <- function(dat) {

    # Spalten
    spalten_ch   <- c("Was", "Jahr", "Wert")
    spalten_alle <- c("Was", "Anlage", "Kurz", "Jahr", "Wert")

    # Zeilen; all(duplicated(ch_2010) == FALSE)
    # CH: 86 'Was', 26 'Jahr'
    zeilen_ch <- 86 * 26

    # Alle: 15 'Was', 32 'Kurz' == 'Anlage', 26 'Jahr'
    anz_zeilen_alle <- 15 * 32 * 26

    # Pruefen
    testthat::expect_equal(colnames(dat$ch),   spalten_ch)
    testthat::expect_equal(colnames(dat$alle), spalten_alle)

    testthat::expect_equal(dim(dat$ch),   c(zeilen_ch, 3))
    testthat::expect_equal(dim(dat$alle), c(anz_zeilen_alle, 5))
}


# Pruefe, ob CH mit dem Total CH aus Alle idenitsch sind.
#
.pruefe_ch_vs_alle <- function(dat) {

    zeilen <- c("Plankapazität", "Importe",
                "Stromabgabe MWh", "Wärmeabgabe MWh",
                "Anfall Schlacke", "Anfall Filterasche",
                "CO2-Emissionen Fossil (brutto)")

    # aus ch
    ch_2010 <- subset(dat$ch, Jahr == 2010 & Was %in% zeilen) %>%
                    dplyr::select(-Jahr) %>%
                    dplyr::arrange(Was)

    # aus alle
    alle_2010 <- subset(dat$alle, Jahr == 2010 & Kurz == "CH" & Was %in% zeilen) %>%
                    dplyr::select(-Anlage, -Kurz, -Jahr) %>%
                    dplyr::arrange(Was)

    testthat::expect_equal(ch_2010$Wert, alle_2010$Wert)
}


# pfad <- "tests/2018-05_Monitoringtool_V.2.3.xlsm"
# roh  <- MTplotR::alle_daten_einlesen(pfad)
# dat  <- alle_daten_aufbereiten(roh)
# print(dat)



