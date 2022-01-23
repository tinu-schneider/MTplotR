



#' Gib die Farbpalette VBSA
#'
#' Grundlage: Datei FARBEN_VBSA.xlsm
#'
#' @return Ein
#'
#'
#' @importFrom dplyr tribble
#'
#' @export
#'
gib_farb_palette <- function() {
    farb <- dplyr::tribble(
        ~Name, 	       ~Hex,
        "blau", 	   '#3E6fA7',
        "hellblau",    "#adbedb",  #  '#92A9CF',
        "gruen", 	   '#7AAA3C',
        "hellgruen",   '#B8CD95',
        "violett",     '#8063A1',
        "hellviolett", '#B3A1C6',
        "dunkelrot",   '#B83131',
        "altrosa", 	   '#D19292',
        "orange", 	   '#F08D2C',
        "senf", 	   '#E1CC61',
        "dunkelgrau",  '#5F5F5F'
    )
    farb
}



#' Initialisiere die Farben fuer den Bericht
#'
#' Es wird die Farb-Palette als 'farb_palette' ins .GlobalEnv geschrieben
#'
#' @return invisible
#'
#' @export
#'
init_farben <- function() {
    assign("farb_palette", gib_farb_palette(), envir = .GlobalEnv)
}






#' Gib eine oder mehrere Farbe(n) aus der VBSA-Palette
#'
#' @param name Der Name der Farbe oder ein Vector mit mehreren Farben
#'
#' @return Die Farbe(n) als Hex-String, zB '#3E6fA7'
#'
#' @import dplyr
#'
#' @export
#'
col_vbsa <- function(name) {
    res <- gib_farb_palette() %>%
            filter(Name %in% name) %>%
            mutate(Name = factor(Name, levels = name)) %>% # nach `name` ordnen
            arrange(Name) %>%
            pull(Hex)

    if (length(res) < 1) stop("Diese Farbe gibt es hier nicht.")
    if (length(res) != length(name)) stop("Bei der Farbauswahl ging etwas schief, es gibt nicht alle gewünschten Farben.")
    res
}







# Die Default-Farben der Kaskaden
#
.gib_farben_wf <- function() {
    farb <- c(total   = col_vbsa("gruen"),
              zunahme = col_vbsa("blau"),
              abnahme = col_vbsa("dunkelrot"))
    farb
}



# Die Default-Farben der HGT-Korrektur
#
.gib_farben_hgt <- function() {

    farb <- c(basisjahr  = col_vbsa("gruen"),
              bezugsjahr = col_vbsa("violett"),
              vorjahre   = col_vbsa("dunkelgrau"), # stehen lassen
              hgt_pfeil  = col_vbsa("orange"),
              korr_pfeil = col_vbsa("hellviolett"))
    farb
}


#' Die Default-Farben fuer den Plot der CO2-Emissionen mit Boni
#'
#' @export
#'
gib_farben_co2_boni <- function() {
    farb <- c(malus        = col_vbsa("dunkelrot"),
              brutto       = col_vbsa("blau"),
              bonus_waerme = col_vbsa("altrosa"),
              bonus_strom  = col_vbsa("senf"),
              bonus_metall = col_vbsa("hellviolett"),
              netto        = col_vbsa("orange"))
    farb
}