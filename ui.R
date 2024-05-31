# packages
library(bslib)
library(shiny)
library(sass)
library(fontawesome)
library(shiny.router)
library(shiny.i18n)

#pages
source("pages/va_page__intro.R")
source("pages/va_page__news.R")
source("pages/va_page__detailer.R")
source("pages/va_page__kaarten.R")
source("pages/va_page__kaartekatalog.R")
source("pages/va_page__impressum.R")


# ui
ui <- router_ui(
        route("/", va_page__intro),
        route("news", va_page__news),
        route("detailer", va_page__detailer),
        route("kaarten", va_page__kaarten),
        route("kaartekatalog", va_page__kaartekatalog),
        route("impressum", va_page__impressum)
  )



