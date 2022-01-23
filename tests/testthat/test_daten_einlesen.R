

# library(testthat)
# library(MTplotR)

context("Daten einlesen")

test_that("Die richten Daten werden eingelesen", {

    pfad <- "../Monitoringtool_TEST.xlsm"
    roh  <- alle_daten_einlesen(pfad)

    expect_true( is.list(roh) )
    expect_that( length(roh), equals(7) )
    expect_that( names(roh), equals(c("ch", "alle", "witt", "div", "bez", "vers", "sz")) )
})

