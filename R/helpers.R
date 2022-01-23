
#' Schreibe das Bezugsjahr in die .tex-Dateien
#'
#' Wir schreiben eine tex-Variable mit dem aktuellen Bezugsjahr. Diese wird fuer den Report eingelesen
#'
#' @param jahr Das Bezugsjahr als `integer`
#' @param out_dir Der Ordner, wo die .tex-Datei gespeichert wird
#' @param file_name Default: `bezugsjahr.tex`, kann hier angepasst werden
#'
#' @return Die Datei wird geschrieben.
#'
#' @export
#'
schreibe_das_bezugsjahr <- function(jahr, out_dir, file_name = "bezugsjahr.tex") {
    inhalt <- "% Diese Datei wird automatisch erstellt. Hier nichts ändern.\n"
    inhalt <- paste0(inhalt, "\\newcommand\\bezugsjahr{", jahr, "}")

    cat(inhalt, file = file_name)
}


#' Das Basisjahr
#'
#' @export
#'
gib_basisjahr <- function() {
    2010
}


#' Gib alle Spaltennamen, dh alle Variablen
#'
#' @param daten Die aufbereiteten Daten, `dat$ch` oder `dat$alle`, dh mit einer Spalte `Was`
#'
#' @export
#'
#'
gib_spaltennamen <- function(daten) {
    unique(daten$Was)
}
# gib_spaltennamen(dat$ch)



#' Gib den Wert einer Groesse zu einem Jahr
#'
#' @param daten Die aufbereiteten Daten, wir erwarten eine Spalte `Was`
#' @param was Die Bezeichnung des Wertes (`character`)
#' @param jahr Der Wert des Jahres `jahr`
#' @param kt (CH) Wenn aus den Daten aller Anlagen gesuch werden soll,
#'     muss hier das Kuerzel der Anlage angegeben werden, zB `AG_1`
#' @param runder (optional, default = 0) Die Daten werden auf 0 Kommastellen gerundet.
#'
#' @importFrom dplyr filter
#'
#' @export
#'
gib_wert <- function(daten, was, jahr, kt = "CH", runder = 0) {
    if (jahr < 2010) stop("Das Jahr darf nicht kleiner als 2010 sein.")
    if (jahr > 2025) stop("Das Jahr darf nicht kleiner als 2010 sein.")

    if (kt == "CH") {
        res <- dplyr::filter(daten, Was == was, Jahr == jahr)$Wert
    } else {
        res <- dplyr::filter(daten, Was == was, Jahr == jahr, Kurz == kt)$Wert
    }
    res <- round(res, runder)
    res
}
# gib_wert_von(dat$ch, "Verbrannte Menge", dat$bez)





#' Gib einen formatierten Wert
#'
#' @param daten Die aufbereiteten Daten, wir erwarten eine Spalte `Was`
#' @param was Die Bezeichnung des Wertes (`character`)
#' @param jahr Der Wert des Jahres `jahr`
#' @param kt (CH) Wenn aus den Daten aller Anlagen gesuch werden soll,
#'     muss hier das Kuerzel der Anlage angegeben werden, zB `AG_1`
#' @param runder (optional, default = 0) Die Daten werden auf 0 Kommastellen gerundet.
#'
#' @export
#'
gib_formatierten_wert <- function(daten, was, jahr, kt = "CH", runder = 0) {
    prettyNum(gib_wert(daten, was, jahr, kt, runder), big.mark = "'")
}


#' Gib die Hoehen für die interaktiven Grafiken
#'
#' @export
#'
hoehen <- function() {
    res <- c("Import" =  2.2,
             "Punkte" =  1.3,
             "Linien" =  1.84,
             "Temp"   =  2.2,
             "Vgl_bj" =  5.25,
             "Bars"   =  1.84,
             "Bilanz" =  2.5,
             "Szen"   =  1.84,
             "Waterf_ch"   = 2.5,
             "Waterf_alle" = 5.0
             )
    res
}

#' Gib eine bestimmte Hoehe
#'
#' @param name Der Name der Hoehe
#'
#' @export
#'
gib_hoehe <- function(name) {
    unname(hoehen()[name])
}




#' Formatiere die Werte für die Tooltips
#' @noRd
.formatiere_tt_wert <- function(x, digits = 0) {
    formatC(x,
            digits   = digits,
            big.mark = "`",
            format   = "f",
            drop0trailing = FALSE)
}







