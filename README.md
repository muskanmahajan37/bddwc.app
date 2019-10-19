# bdDwC.app

Darwinazing biodiversity data in R.

"Darwin Core is a standard maintained by the Darwin Core maintenance group. It includes a glossary of terms intended to facilitate the sharing of information about biological diversity by providing identifiers, labels, and definitions." [Darwin Core](https://github.com/tdwg/dwc)

`bddwc.app` is an shiny UI extension of bdDwC package that user can use to Darwinize given dataset. This package is part of [`bdverse`](https://bdverse.org) and has a colsole interface implemented in [`bdDwC`](https://github.com/bd-R/bdDwC).


---

Install `bddwc.app` with: 

    devtools::install_github("bd-R/bddwc.app")

---

To use `bddwc.app`, load `bdDwC` package with:

    library(bddwc.app)

and to run the shiny app with:

    bddwc_app()