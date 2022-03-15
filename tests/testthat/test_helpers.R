


context("Helpers")

# setup
pfad <- "../Monitoringtool_TEST.xlsm"
roh  <- alle_daten_einlesen(pfad)
dat  <- alle_daten_aufbereiten(roh)


test_that("'gib_wert() liefert für dat$ch richtig'", {

    expect_equal(gib_wert(dat$ch, "Verbrannte Menge", 2010), 3732303)
    expect_equal(gib_wert(dat$ch, "Netto-CO2 Emissionen, witterungskorrigiert",
                            2017, runder = -2),  1016000)
    expect_equal(gib_wert(dat$ch, "Wärmeabgabe, indexiert (2010 = 100%)",
                            2017, runder = 2), 121.53)

    expect_error(gib_wert(dat$ch, "Verbrannte Menge", 2009))
    expect_error(gib_wert(dat$ch, "Verbrannte Menge", 2036))
    }
)

test_that("'gib_wert() liefert für dat$alle richtig'", {
    expect_equal(gib_wert(dat$alle, "Verbrannte Menge", 2010, "AG_1"), 117947)
    expect_equal(gib_wert(dat$alle, "CO2-Emissionen Fossil (brutto)", 2017, "ZH_6"), 108027)
    }
)


# fix #1, Berechnung kumuliertes BIP
test_that("BIP wird richtig kumuliert", {
    bip_kumuliert_bj     <- gib_wert(dat$ch, "BIP_kumuliert", 2017)
    d_bip_seit_2010_proz <- round( bip_kumuliert_bj - 100, 1)

    expect_equal(d_bip_seit_2010_proz,  12.0 )

    }
)

