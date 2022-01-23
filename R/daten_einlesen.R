
#' Die Daten einlesen
#'
#' Wir lesen die Daten aus dem Monitoring-Tool ein
#'
#' @param pfad Der volle Pfad zum Monitoring-Tool inkl. Dateiname und Endung
#'
#' @return Eine Liste mit
#' * Daten CH `ch`
#' * Daten aller Anlagen `alle`
#' * Daten der Witterung `witt`
#' * Diverse (BIP, ...?) `div`
#' * Bezugsjahr `bez`
#' * Definition der Szenarien `sz`
#'
#' @importFrom  readxl read_excel
#'
#' @export
#'
alle_daten_einlesen <- function(pfad) {
    res <- list()

    res$ch   <- .daten_einlesen(pfad, ".db_CH")
    res$alle <- .daten_einlesen(pfad, ".db_Alle")
    res$witt <- .daten_einlesen(pfad, ".db_Witterung")
    res$div  <- .daten_einlesen(pfad, ".db_Diverse") # BIP, ...
    res$bez  <- .bezugsjahr_einlesen(pfad)
    res$vers <- .version_einlesen(pfad)
    res$sz   <- .szenarien_einlesen(pfad)

    res
}


# helpers
# ------------------------


.daten_einlesen <- function(pfad, sheet) {
    readxl::read_excel(pfad, sheet)
}

.bezugsjahr_einlesen <- function(pfad) {
    bez <- readxl::read_excel(pfad, "Main", range = "F2", col_names = FALSE)
    as.integer(bez)
}

.version_einlesen <- function(pfad) {
    vers <- readxl::read_excel(pfad, "log", range = "C6", col_names = FALSE)
    as.character(vers)
}

.szenarien_einlesen <- function(pfad) {
    vers <- readxl::read_excel(pfad, "Szenarien", range = "U3:V5", col_names = TRUE)
}




# pfad  <- file.path(getwd(), "tests/Monitoringtool_TEST.xlsm")
# daten <- MTplotR::alle_daten_einlesen(pfad)
# print(daten)
