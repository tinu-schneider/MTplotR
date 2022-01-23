


#' Helper: Die Tooltips für die Bars erstellen
#'
#' @param daten Die aufbereiteten Daten (mit Spalte TT)
#' @return Die ergänzten Daten (Spalte TT)
#' @noRd
.erstelle_tooltips_vgl <- function(daten, y_label) {

    dat <- daten
    dat$TT <- NA

    if (is.expression(y_label)) {
        y_label <- "t CO<sub>2-eq"
    }

    dp <-
        dat %>%
        mutate(TT = paste0(Kurz, ", ", Jahr, ": &nbsp;", .formatiere_tt_wert(Wert), " ", y_label)
        )
    dp

}

