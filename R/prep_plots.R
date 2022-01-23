


#' Daten für Zeitreihen präparieren
#'
#' @param daten Die (vollständigen) Daten
#' @param spalten Ein Vector mit den Spaltennamen
#' @param max_jahr Bis zu diesem Jahr werden die Daten gezeigt, dh
#'    im Allgemeinen wohl das Bezugsjahr. Es kann aber auch früher oder später sein.
#' @param runder (2) Auf wieviele Stellen soll gerundet werden?
#'
#' @import dplyr
#'
#' @return Ein langer `data_frame` mit den Spalten `Typ`, `Jahr`, `Wert` und `TT` (Tooltip)
#'
#' @export
#'
prep_daten_zr <- function(daten, spalten, max_jahr, runder = 2) {
    dat <- daten %>%
        dplyr::filter(Was %in% spalten, Jahr <= max_jahr) %>%
        dplyr::arrange(Was, Jahr) %>%
        dplyr::rename(Typ = Was) %>%
        dplyr::mutate(Typ = factor(Typ, levels = spalten )) %>%
        dplyr::mutate(Wert = round(Wert, runder)) %>%
        dplyr::mutate(TT = NA)
    dat
}



#' Daten für den Vergleich mit Basisjahr aufbereiten
#'
#' @param daten Die aufbereiteten Daten aller Anlagen, dh `dat$alle`
#' @param spalte Der Name der Variablen, die verglichen werden soll
#' @param bezugsjahr Das aktuelle Bezugsjahr, zB `dat$bez`
#'
#' @return Ein `data_frame` mit den drei Spalten `Kurz`, `Jahr`, `Wert`
#'
#' @import dplyr
#'
#' @export
#'
prep_daten_vgl_vorjahr_alle <- function(daten, spalte, bezugsjahr) {

    basisjahr <- gib_basisjahr()
    jahre <- c(basisjahr, bezugsjahr)

    anzahl_anlagen <- length(unique(daten$Kurz))
    fac_levels <- rev(daten$Kurz[1:anzahl_anlagen])

    dat <- daten %>%
        dplyr::filter(Jahr %in% jahre, Was == spalte) %>%
        dplyr::mutate(Kurz = factor(Kurz, levels = fac_levels)) %>%
        dplyr::mutate(Jahr = factor(Jahr, levels = c(bezugsjahr, basisjahr))) %>%
        dplyr::select(-Anlage, -Was)
    ohne <- which(dat$Kurz == "CH")

    dat <- dat[-ohne, ]

}


#' Daten Wasserfall-Chart Schweiz aufbereiten
#'
#' @param daten Die (vollständigen) Daten, sie werden hier gefiltert
#' @param bezugsjahr Das aktuelle Bezugsjahr
#'
#' @return Ein `data.frame` mit den zwei Spalten `values` und `labels`,
#'     wie er von `waterfalls` erwartet wird.
#'
#' @import dplyr
#' @import tidyr
#'
#' @export
#'
prep_daten_waterfall_ch <- function(daten, bezugsjahr) {

    vorjahr <- bezugsjahr - 1

    spalten <- c("Verbrannte Menge",
                 "Importe",
                 "davon Klärschlamm (Feucht)")

    # spalten_neu <- c("Klärschlamm", "Importe", "Total")


    dat <- daten %>%
        dplyr::filter(Jahr %in% c(vorjahr, bezugsjahr),
                      Was %in% spalten) %>%
        tidyr::spread(key = Jahr, value = Wert)

    # dat$Was <- spalten_neu

    delta <- dat[, 3] - dat[, 2]
    names(delta) <- "delta"

    dat <- cbind(dat, delta)

    tot_vorjahr    <- dplyr::filter(daten, Was == spalten[1], Jahr == vorjahr)$Wert
    tot_bezugsjahr <- dplyr::filter(daten, Was == spalten[1], Jahr == bezugsjahr)$Wert

    netto_vorjahr    <- tot_vorjahr - dat[1, 2] - dat[2, 2]
    netto_bezugsjahr <- tot_bezugsjahr - dat[1, 3] - dat[2, 3]
    delta_ch         <- netto_bezugsjahr - netto_vorjahr # das gibt die Differenz CH

    labels <- c(paste("Total", vorjahr),
                "Delta Klärschlamm",
                "Delta Importe",
                "Delta CH",
                paste("Total", bezugsjahr))

    values <- c(tot_vorjahr, dat[1, "delta"], dat[2, "delta"], delta_ch, tot_bezugsjahr)


    dat <- data.frame(
        values = round(values, 0),
        labels = labels,
        stringsAsFactors = FALSE
    )
    dat
}










#' Daten Wasserfall-Chart aller Anlagen aufbereiten
#'
#' @param daten Die Daten, sie werden hier gefiltert
#' @param spalte Der Name der Variablen, die geplottet wird
#' @param bezugsjahr Das aktuelle Bezugsjahr
#'
#' @return Ein `data.frame` mit den zwei Spalten `values` und `labels`,
#'     wie er von `waterfalls` erwartet wird.
#'
#' @import dplyr
#' @import tidyr
#'
#' @export
#'
prep_daten_waterfall_alle <- function(daten, spalte, bezugsjahr) {

    vorjahr <- bezugsjahr - 1
    vj <- as.character(vorjahr)
    bj <- as.character(bezugsjahr)

    anz_anlagen <- length(unique(daten$Anlage))


    dat <- daten %>%
        dplyr::filter(Jahr %in% c(vorjahr, bezugsjahr),
                      Was == spalte) %>%
        dplyr::mutate(Anlage = factor(Anlage, levels = Anlage[1:anz_anlagen])) %>%
        tidyr::spread(key = Jahr, value = Wert) %>%
        dplyr::select(-Was) %>%
        dplyr::filter(Kurz != "LU_1")

    spalte_bezugsjahr <- which(colnames(dat) == bezugsjahr)
    spalte_vorjahr    <- spalte_bezugsjahr - 1

    dat$Delta <- dat[[spalte_bezugsjahr]]  - dat[[spalte_vorjahr]]

    startwert <- subset(dat, Kurz == "CH")[[spalte_vorjahr]]
    names(startwert) <- paste("CH", vorjahr)

    deltas <- dat[["Delta"]]
    names(deltas) <- dat[['Kurz']]

    deltas <- deltas[-length(deltas)]
    deltas <- c(startwert, deltas)
    deltas <- round(deltas, 0)

    dat <- data.frame(
        values = unname(deltas),
        labels = names(deltas),
        stringsAsFactors = FALSE
    )

    dat
}


#' Daten für die HGT-Korrektur aufbereiten
#'
#' @param daten Die Daten der Schweiz (dat$ch)
#' @param bezugsjahr Das aktuelle Bezugsjahr
#'
#' @import dplyr
#' @importFrom tidyr spread
#'
#' @export
#'
prep_daten_hgt <- function(daten, bezugsjahr) {

    basisjahr <- gib_basisjahr()

    farben    <- .gib_farben_hgt()

    dat <- prep_daten_zr(daten,
                         spalten = c("HGT",
                                     "Wärmeabsatz HGT-Unabhängig",
                                     "Wärmeabsatz total",
                                     "korr. Wärmeabsatz total"),
                         bezugsjahr) %>%
        tidyr::spread(Typ, Wert) %>%
        mutate(HGT_NULL = 0) %>%
        mutate(Size  = ifelse(Jahr %in% c(basisjahr,  bezugsjahr), 5, 2)) %>%
        mutate(Color = ifelse(Jahr == bezugsjahr, farben["bezugsjahr"], farben["vorjahre"])) %>%
        mutate(Color = replace(Color, Jahr == basisjahr, farben["basisjahr"]))

    colnames(dat) <- c("Jahr",
                       "TT",
                       "HGT",
                       "Abgabe_unabh",
                       "Abgabe_total",
                       "Abgabe_korr",
                       "HGT_NULL",
                       "Size",
                       "Color")
    # print(dat)

    dat
}



#' Die Daten für den Vergleich der CO2-Emissionen brutto und Boni
#'
#' @param daten Die Daten der Schweiz
#' @param bezugsjahr Das aktuelle Bezugsjahr
#'
#' @return Ein `data_frame` wie für die Zeitreihen.
#'
#' @export
#'
prep_daten_co2_alle_boni <- function(daten, bezugsjahr) {
    spalten <- c("CO2-Emissionen Fossil (brutto)",
                 "Bonus CO2 Wärme negativ",
                 "Bonus CO2 Strom negativ" ,
                 "Bonus CO2 Metalle negativ",
                 "Malus",
                 "Netto-CO2 Emissionen nach Malus"
    )

    dat <- prep_daten_zr(daten, spalten, bezugsjahr)
    dat
}




#' Die Temperaturdaten Meteo-Schweiz aufbereiten
#'
#' Sie können anschliessend mit `plot_zr` geplottet werden
#'
#' @param daten Die eingelesenen Roh-Daten (ohne Header)
#' @param von (Jahr) Ab welchem Jahr soll geplottet werden?
#'
#' @return Ein `data.frame` mit `Jahr`, `Typ`, `Wert`
#'
#' @import dplyr
#' @importFrom tidyr gather
#'
#' @export
#'
prep_dat_temp <- function(daten, von) {
    dat <- daten  %>%
        select(time, winter, year) %>%
        filter(time >= von) %>%
        tidyr::gather(Typ, Wert, -time) %>%
        rename(Jahr = time) %>%
        mutate(Typ = factor(Typ))
}




#' Die spezifischen Daten aufbereiten
#'
#' Wir zeigen Wärme/Strom und Fe/NE bezüglich der verbrannten Abfallmenge,
#' in kWh pro Tonne Abfall bzw. kg Metall pro Tonne Abfall
#'
#' @param daten Die Daten der Schweiz
#'
#' @return Ein `data_frame` für den enstsprechenden Plot
#'
#' @import dplyr
#' @importFrom tidyr gather
#' @importFrom forcats as_factor
#'
#' @export
#'
prep_dat_spezifisch <- function(daten) {

    skalier <- 1000
    res <- daten %>%
        filter(Was %in% c("Verbrannte Menge",
                          "Wärmeabgabe MWh",
                          "Stromabgabe MWh",
                          "Fe",
                          "NE als Summe zur Vergleichbarkeit")) %>%
        pivot_wider(names_from = Was, values_from = Wert) %>%
        rename(Waerme = 'Wärmeabgabe MWh',
               Strom  = 'Stromabgabe MWh',
               NE     = "NE als Summe zur Vergleichbarkeit",
               Abfall = 'Verbrannte Menge') %>%
        mutate(spez_waerme = Waerme / Abfall * skalier,
               spez_strom  = Strom / Abfall * skalier,
               spez_ne = NE / Abfall * skalier,
               spez_fe = Fe / Abfall * skalier) %>%
        select(-Abfall, -Waerme, -Strom, -Fe, -NE) %>%
        tidyr::gather(key = "Spezifisch", value = "Wert", -Jahr) %>%
        mutate(Jahr = as.integer(Jahr)) %>%
        filter(Jahr %in% c(BASISJAHR, BEZUGSJAHR))   %>%
        arrange(Jahr, Wert) %>%
        mutate(Spezifisch = as_factor(Spezifisch),
               Jahr = as_factor(Jahr))
    res
}











#' Die Daten der weiteren Emissionsreduktionen aufbereiten
#'
#' Die totalen Emissionsreduktionen, die im Rahmen von Kompensationsprojekten
#' geltend gemacht werden, in t CO2-eq. pro Jahr.
#'
#' @param daten Die Daten aller Anlagen
#' @param max_jahr = 2020,  Bis zu welchem Jahr wollen wir die Säulen zeigen?
#'
#' @return Ein `data_frame` für den entsprechenden Plot
#'
#' @import dplyr
#' @importFrom tidyr complete
#' @importFrom forcats as_factor
#' @importFrom forcats fct_drop
#'
#' @export
#'
prep_dat_weitere_emissionsreduktionen <- function(daten, max_jahr = 2020) {

    res <- daten %>%
        filter(Was == "Weitere") %>%
        mutate(Jahr = as_factor(Jahr),
               Kurz = as_factor(Kurz)) %>%
        filter(Wert > 0) %>%
        tidyr::complete(Jahr) %>%
        mutate(Jahr = as.integer(as.character(Jahr))) %>%
        filter(Jahr <= max_jahr) %>%
        mutate(Jahr = as_factor(Jahr),
               Jahr = fct_drop(Jahr)) %>%
        select(-Was)
    res
}

