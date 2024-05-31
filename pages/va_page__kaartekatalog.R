
#packages
library(bslib)
library(shiny)
library(sass)
library(fontawesome)
library(shiny.router)


# components
source("R/unilu_theme.R")
source("R/unilu_page.R")
source("R/unilu_grid.R")
source("R/unilu_header.R")
source("R/unilu_navbar.R")
source("R/unilu_navpage.R")
source("R/unilu_text_blocks.R")
source("R/unilu_cards.R")
source("R/unilu_content.R")
source("R/unilu_footer.R")


# News Page
va_page__kaartekatalog <- page_fluid(
    tags$head(
        tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
        tags$script(src = "js/menu_show.js")

    ),
    title = "Variatiounsatlas | Kaartekatalog | uni.lu",
    style = "max-width: 95.5rem;",
    theme = unilu_theme,
    
    #* unilu_grid(6),
    unilu_header(img = "img/variatiounsatlas_logo.png"),

    unilu_navbar(
            brand = "Variatiounsatlas",
            brand_logo_url = fa("book-atlas", fill = "#f6f5ea"),
            unilu_navpage("Intro", href = route_link("/")),
            unilu_navpage("News", href = route_link("news")),
            unilu_navpage("Detailer", href = route_link("detailer")),
            unilu_navpage("Kaarten", href = route_link("kaarten")),
            unilu_navpage("Kaartekatalog", href = route_link("kaartekatalog"), is_active = TRUE),
            unilu_navpage("Impressum", href = route_link("impressum")),
            unilu_navpage("Disabled", is_disabled = TRUE)
        ),
    
    unilu_empty_block("3rem"),

        # news
    div(class = "row",
        div(class = "col-12 text-start",
            unilu_title_small(i18n$t("Kaartekatalog")),
            unilu_text("Iwwersiicht iwwer all Kaart am Atlas. Mam Link vun der Variable kann een direkt op d’Kaart sprangen.")
        )
    ),

    unilu_card(i18n$t("Lëscht mat Variabelen"),  DTOutput("katalog", height = "auto")),



    unilu_footer("Copyright © Université du Luxembourg, Department of Humanities, 2021-2023. All rights reserved.")


)