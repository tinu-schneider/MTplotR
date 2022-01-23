

library(testthat)
library(MTplotR)

context("Charts plotten")

# devtools::install_github("lionel-/vdiffr")
# library(vdiffr)

## run to init the charts
# vdiffr::collect_cases()
# vdiffr::validate_cases()
# vdiffr::manage_cases() # to diff


# setup
pfad <- "../Monitoringtool_2020_dummy.xlsm"
roh  <- alle_daten_einlesen(pfad)
dat  <- alle_daten_aufbereiten(roh)



# test_that("Die Waterfalls sind ok", {
#     dat_wf      <- prep_daten_waterfall_ch(dat$ch, dat$bez)
#     dat_wf_alle <- prep_daten_waterfall_alle(dat$alle, "Verbrannte Menge", dat$bez)
#     # fails with CMD Check, ok with Ctrl+Shift+T
#     vdiffr::expect_doppelganger("Waterfall CH",
#                                 plot_waterfall_ch(dat_wf))
#     vdiffr::expect_doppelganger("Waterfall Alle",
#                                 plot_waterfall_alle(dat_wf_alle, dat$bez,
#                                                     y_min = 4.00e6,
#                                                     y_max = 4.30e6))
# })


test_that("Die Importe sind ok", {
    dp <- prep_daten_zr(daten    = dat$ch,
                        spalten  = c("Import D", "Import I", "Import Ö", "Import F"),
                        max_jahr = dat$bez)
    p <- plot_zr(dp,
                 farben  = col_vbsa(c("dunkelrot", "orange", "gruen", "blau")),
                 typ     = "bars",
                 y_label = "Tonnen",
                 y_max   = 350000,
                 legend_lab = paste0(" ", c("DE", "IT", "OE", "FR"), "   ")
    )
    vdiffr::expect_doppelganger("Importe",   p)
})

test_that("Die Punkte sind ok", {
    dp <- prep_daten_zr(daten    = dat$ch,
                        spalten  = "Verbrannte Menge",
                        max_jahr = dat$bez) %>%
        dplyr::mutate(Wert = round(Wert / 1e6, 2)) # Mio. Tonnen
    p <- plot_zr(dp,
                 farben  = col_vbsa("blau"),
                 typ     = "punkte",
                 y_label = "Mio. Tonnen",
                 y_min   = 3.5,
                 y_max   = 4.2)
    vdiffr::expect_doppelganger("Punkte",   p)
})

test_that("Die Linien sind ok", {
    dp <- prep_daten_zr(daten    = dat$ch,
                        spalten  = c("dAbfall", "BIP"),
                        max_jahr = dat$bez)
    p <- plot_zr(dp,
                 farben  = col_vbsa(c("blau", "dunkelrot")),
                 typ     = "linien",
                 y_label = "Änderung in Prozent gegenüber Vorjahr",
                 y_min   = -3,
                 y_max   =  5,
                 legend_lab = c(" Verwertete Abfallmenge   ", " BIP"))
    vdiffr::expect_doppelganger("Linien",   p)
})


# Vergleich mit Vorjahr
test_that("Die Vergleiche sind ok", {
    dp <- prep_daten_vgl_vorjahr_alle(daten  = dat$alle,
                                      spalte = "Verbrannte Menge",
                                      bezugsjahr = dat$bez)
    p <- plot_vgl_basisjahr(dp,
                            farben  = col_vbsa(c("gruen", "hellgruen")),
                            y_label = "Tonnen")
    vdiffr::expect_doppelganger("Vergleich",   p)
})




# Alle Boni
# test_that("Die Grafik 'Alle Boni' ist ok", {
#     dp <- prep_daten_co2_alle_boni(dat$ch, dat$bez)
#     p  <- plot_co2_boni(dp)
#     vdiffr::expect_doppelganger("Alle_Boni",   p)
# })


## Die Spaltennamen haben UMLAUTE,
# vdiffr lädt intern ( devtools::load_all(".") ) und das funzt nicht mit Umlauten.
# Darum gibt es bei den zwei Grafiken der HGT-Korreketur Fehler
# Es hat dann zuwenig Daten, weil die Spalten mit Umlauten ignoriert werden.

# HGT-Korrektur Basisjahr
# test_that("Die Grafik 'HGT_Basisjahr' ist ok", {
#     dp <- prep_daten_hgt(dat$ch, dat$bez)
#     p  <- plot_hgt_basis(dp, dat$bez, 4.2e6)
#     vdiffr::expect_doppelganger("hgt-basisjahr",   p)
# })


# # HGT-Korrektur 2017
# test_that("Die Grafik 'HGT_Bezugsjahr' ist ok", {
#     dp <- prep_daten_hgt(dat$ch, dat$bez)
#     p  <- plot_hgt_bezugsjahr(dp, dat$bez, 4.2e6)
#     vdiffr::expect_doppelganger("hgt-bezugsjahr",   p)
# })



# aufräumen
rm(pfad, roh, dat)