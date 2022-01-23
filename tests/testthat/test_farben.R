
context("Farben")

test_that("Farben passen", {

    # Aktuell haben wir 11 Farben
    expect_equal(nrow(gib_farb_palette()), 11)

    # Falscher Name
    expect_error(col_vbsa("hellrott"))

    # Richtige Namen, 1. und letzte
    expect_equal(col_vbsa("dunkelgrau"), '#5F5F5F')
    expect_equal(col_vbsa("dunkelrot"), '#B83131')

    # Zwei Farben
    expect_equal(col_vbsa(c("dunkelrot", "blau")), c('#B83131','#3E6fA7'))

    # Drei Farben, eine davon falsch
    expect_error(col_vbsa(c("dunkelrot", "rot", "blau")))

    # Vier Farben, Reihenfolge korrekt
    vf <- col_vbsa(c("dunkelrot", "orange", "gruen", "blau"))
    expect_equal(vf, c('#B83131', '#F08D2C', '#7AAA3C', '#3E6fA7') )


})



