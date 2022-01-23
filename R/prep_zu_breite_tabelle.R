
#' Eine zu breite Tabelle auf weniger Spalten reduzieren
#'
#' @param tab Die orignale aufbereitete Tabelle
#' @param anz_restliche Wieviele restliche/letzte Spalten sollen gezeigt werden, default = 3
#'
#' @return Die angepasste Tabelle (falls sie effektiv zu breit war)
#'
#' @export
#'
prep_zu_breite_tabelle <- function(tab, anz_restliche = 3) {
    anz_spalten <- ncol(tab)

    if (anz_spalten <= anz_restliche + 3) return(tab) # da ändern wir nichts


    von <- anz_spalten - anz_restliche + 1

    erste <- tab[, c(1, 2)]
    erste$'[...]' <- " "

    letzte <- tab[, c(von:anz_spalten)]

    res <- cbind(erste, letzte)
    res

}


# tab <- prep_tab_waerme_hgt_zr(roh$witt, BEZUGSJAHR)
# tab <- prep_zu_breite_tabelle(tab)
# print(tab)