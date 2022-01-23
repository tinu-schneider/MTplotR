
#' Tooltips Importe
#' @param daten Wir erwarten bereits eine Spalte TT.
#' @return `TT` mit sinnvollen Tooltips
#' @noRd
.prep_tt_import <- function(daten) {
    dp <- daten %>%
        mutate(tt_typ = as.character(Typ)) %>%
        mutate(tt_typ = replace(tt_typ, tt_typ == "Import Ö", "OE")) %>%
        mutate(tt_typ = replace(tt_typ, tt_typ == "Import D", "DE")) %>%
        mutate(tt_typ = replace(tt_typ, tt_typ == "Import I", "IT")) %>%
        mutate(tt_typ = replace(tt_typ, tt_typ == "Import F", "FR")) %>%
        mutate(TT = paste0(tt_typ, ", ", Jahr, ":\n",
                           .formatiere_tt_wert(Wert),
                           " t")
               ) %>%
        select(-tt_typ)
    dp
}


#' Tooltips Wärme- und Stromababe
#' @noRd
.prep_tt_waerme_strom <- function(daten) {
    dp <- daten %>%
        mutate(tt_typ = as.character(Typ)) %>%
        mutate(tt_typ = replace(tt_typ, tt_typ == "Stromabgabe MWh", "Stromabgabe")) %>%
        mutate(tt_typ = replace(tt_typ, tt_typ == "Wärmeabgabe MWh", "W&auml;rmeabgabe")) %>%
        mutate(TT = paste0(tt_typ, ", ", Jahr, ":\n",
                           .formatiere_tt_wert(Wert),
                           " MWh")
        ) %>%
        select(-tt_typ)
    dp
}


#' Tooltips Bonus CO2 Wärme und Strom
#' @noRd
.prep_tt_bonus_waerme_strom <- function(daten) {
    dp <- daten %>%
        mutate(tt_typ = as.character(Typ)) %>%
        mutate(tt_typ = replace(tt_typ, tt_typ == "Bonus CO2 Strom", "Bonus CO<sub>2</sub> Strom")) %>%
        mutate(tt_typ = replace(tt_typ, tt_typ == "Bonus CO2 Wärme", "Bonus CO<sub>2</sub> W&auml;rme")) %>%
        mutate(TT = paste0(tt_typ, ", ", Jahr, ":\n",
                           .formatiere_tt_wert(Wert),
                           " t CO<sub>2-eq</sub>")
        ) %>%
        select(-tt_typ)
    dp
}

#' Tooltips Eisen, Nicht-Eisen
#' @noRd
.prep_tt_fe_ne <- function(daten) {
    dp <- daten %>%
        mutate(tt_typ = as.character(Typ)) %>%
        mutate(tt_typ = replace(tt_typ, tt_typ == "Fe", "Eisen")) %>%
        mutate(tt_typ = replace(tt_typ, tt_typ == "NE als Summe zur Vergleichbarkeit", "Nicht-Eisen")) %>%
        mutate(TT = paste0(tt_typ, ", ", Jahr, ":\n",
                           .formatiere_tt_wert(Wert),
                           " t")
        ) %>%
        select(-tt_typ)
    dp
}


#' Tooltips Zink
#' @noRd
.prep_tt_zn <- function(daten) {
    dp <- daten %>%
        mutate(tt_typ = as.character(Typ)) %>%
        mutate(tt_typ = replace(tt_typ, tt_typ == "Zn", "Zink")) %>%
        mutate(TT = paste0(tt_typ, ", ", Jahr, ":\n",
                           .formatiere_tt_wert(Wert),
                           " t")
        ) %>%
        select(-tt_typ)
    dp
}


#' Tooltips Boni Metalle
#' @noRd
.prep_tt_boni_metalle <- function(daten) {
    dp <- daten %>%
        mutate(tt_typ = as.character(Typ)) %>%
        mutate(tt_typ = replace(tt_typ, tt_typ == "Bonus CO2 Metalle", "Summe Boni Metalle")) %>%
        mutate(TT = paste0(tt_typ, ", ", Jahr, ":\n",
                           .formatiere_tt_wert(Wert),
                           " t CO<sub>2-equ</sub>")
        ) %>%
        select(-tt_typ)
    dp
}


# Tooltips Verbrannte Menge
#' @noRd
.prep_tt_verbrannte_menge <- function(daten) {
    dp <- daten %>%
        mutate(tt_typ = as.character(Typ)) %>%
        mutate(tt_typ = replace(tt_typ, tt_typ == "Verbrannte Menge", "Verbrannte Abfallmenge")) %>%
        mutate(TT = paste0(tt_typ, ", ", Jahr, ":\n",
                           .formatiere_tt_wert(Wert, digits = 3),
                           " Mio. t")
        ) %>%
        select(-tt_typ)
    dp
}

# Tooltips Fossile Brutto-CO2-Emissionen
#' @noRd
.prep_tt_co2_fossil_brutto <- function(daten) {
    dp <- daten %>%
        mutate(tt_typ = as.character(Typ)) %>%
        mutate(tt_typ = replace(tt_typ, tt_typ == "CO2-Emissionen Fossil (brutto)",
                                 "Fossile Brutto-CO<sub>2</sub>-Emissionen")) %>%
        mutate(TT = paste0(tt_typ, ", ", Jahr, ":\n",
                           .formatiere_tt_wert(Wert, digits = 0),
                           " t CO<sub>2-eq</sub>")
        ) %>%
        select(-tt_typ)
    dp
}

#' Tooltips Vergleich Abfall, BIP
#' @noRd
.prep_tt_abfall_bip <- function(daten) {
    dp <- daten %>%
        mutate(tt_typ = as.character(Typ)) %>%
        mutate(tt_typ = replace(tt_typ, tt_typ == "dAbfall", "Verwertete Abfallmenge")) %>%
        mutate(tt_typ = replace(tt_typ, tt_typ == "BIP", "BIP")) %>%
        mutate(TT = paste0(tt_typ, ", ", Jahr, ":\n",
                           .formatiere_tt_wert(Wert, digits = 2),
                           " % &Auml;nderung bezgl. Vorjahr")
        ) %>%
        select(-tt_typ)
    dp
}


#' Tooltips Netto-CO2 und Zielpfad
#' @noRd
.prep_tt_netto_co2_zielpfad <- function(daten) {
    dp <- daten %>%
        mutate(tt_typ = as.character(Typ)) %>%
        mutate(tt_typ = replace(tt_typ, tt_typ == "Netto-CO2-Emissionen", "Netto-CO<sub>2</sub>-Emissionen")) %>%
        mutate(tt_typ = replace(tt_typ, tt_typ == "Zielpfad Mio. Tonnen", "Zielpfad")) %>%
        mutate(TT = paste0(tt_typ, ", ", Jahr, ":\n",
                           .formatiere_tt_wert(Wert, digits = 2),
                           " Mio.t CO<sub>2-eq</sub>")
        ) %>%
        select(-tt_typ)
    dp
}


#' Tooltips Temperatur
#' @noRd
.prep_tt_temperatur <- function(daten) {
    dp <- daten %>%
        mutate(tt_typ = as.character(Typ)) %>%
        mutate(tt_typ = replace(tt_typ, tt_typ == "year",   "Mittlere Jahrestemperatur")) %>%
        mutate(tt_typ = replace(tt_typ, tt_typ == "winter", "Mittlere Wintertemperatur")) %>%
        mutate(TT = paste0(tt_typ, "\n",
                           Jahr, ": &nbsp;", .formatiere_tt_wert(Wert, digits = 2),
                           " &deg;C")
        ) %>%
        select(-tt_typ)
    dp
}


#' Tooltips Netto-CO2, Zielpfad und HGT-Korrigiert
#' @noRd
.prep_tt_netto_co2_zielpfad_hgtkorr <- function(daten) {

    dp <- daten %>%
        mutate(tt_typ = as.character(Typ)) %>%
        mutate(tt_typ = replace(tt_typ, tt_typ == "Netto-CO2 Emissionen nach Abzug", "Netto-CO<sub>2</sub>-Emissionen")) %>%
        mutate(tt_typ = replace(tt_typ, tt_typ == "Zielpfad", "Zielpfad")) %>%
        mutate(tt_typ = replace(tt_typ, tt_typ == "Netto-CO2 Emissionen, witterungskorrigiert",
                                                    "Netto-CO<sub>2</sub>-Emissionen, HGT-korr.")) %>%
        mutate(TT = paste0(tt_typ, ", ", Jahr, ":\n",
                           .formatiere_tt_wert(Wert, digits = 2),
                           " Mio.t CO<sub>2-eq</sub>")
        ) %>%
        select(-tt_typ) %>%
        mutate(Typ = factor(Typ, levels = c("Zielpfad",
                                            "Netto-CO2 Emissionen nach Abzug",
                                            "Netto-CO2 Emissionen, witterungskorrigiert")))

    dp
}


#' Tooltips Netto-CO2, Zielpfad und Szenarien 1, 2
#' @noRd
.prep_tt_szenarien <- function(daten) {

    dp <- daten %>%
        mutate(tt_typ = as.character(Typ)) %>%
        mutate(tt_typ = replace(tt_typ, tt_typ == "Historische Daten", "Netto-CO<sub>2</sub>-Emissionen")) %>%
        # Zielpfad und Szenarien sind schon ok
        mutate(TT = paste0(tt_typ, ", ", Jahr, ":\n",
                           .formatiere_tt_wert(Wert, digits = 2),
                           " Mio.t CO<sub>2-eq</sub>")
        ) %>%
        select(-tt_typ)
    dp
}

