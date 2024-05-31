
#packages
library(bslib)
library(shiny)
library(shiny.i18n)
library(sass)
library(fontawesome)
library(shiny.router)
library(htmltools)



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


# Intro Page
va_page__intro <- page_fluid(
    # for internationalization
    shiny.i18n::usei18n(i18n),
    
    tags$head(
        tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
        tags$script(src = "js/menu_show.js")
    ),
    title = "Variatiounsatlas | uni.lu",
    style = "max-width: 95.5rem;",
    theme = unilu_theme,
    
    #* unilu_grid(6),
    unilu_header(img = "img/variatiounsatlas_logo.png"),

    unilu_navbar(
            brand = "Variatiounsatlas",
            brand_logo_url = fa("book-atlas", fill = "#f6f5ea"),
            unilu_navpage("Intro", href = route_link("/"), is_active = TRUE),
            unilu_navpage("News", href = route_link("news")),
            unilu_navpage("Detailer", href = route_link("detailer")),
            unilu_navpage("Kaarten", href = route_link("kaarten")),
            unilu_navpage("Kaartekatalog", href = route_link("kaartekatalog")),
            unilu_navpage("Impressum", href = route_link("impressum")),
            
            # language selector
            tags$div(
              class = 'inline-form uni-topnav-link border-start px-4 py-0 fw-normal fs-5',
              selectInput(
                inputId='selected_language',
                #label=unilu_navpage('Sprooch', is_active = FALSE),
                label = NULL,
                #choices = i18n$get_languages(),
                choices = c("Lëtzebuergesch" = "lb", "English" = "en"),
                selected = i18n$get_key_translation(),
                width = '130px'  # Set a specific width
              )
            ),
            unilu_navpage("Disabled", is_disabled = TRUE)
        ),
    
    unilu_empty_block("3rem"),

    # intro
    div(class = "row",
        div(class = "col-xl-6 text-start",
            unilu_title_small("Wëllkomm"),
            unilu_title_large("Variatiounsatlas vum Lëtzebuergeschen"),
            #unilu_text_md("content/intro.md"),
            unilu_text("Dësen Atlas dokumentéiert an analyséiert d'Variatioun vum Lëtzebuergesche fir iwwer 700 ausgewielte linguistesch Variabelen an hir sëlleg Varianten, déi aus de sproochlechen Domäner 'Foneetik', 'Lautkontakt', 'Wuertschatz & Sproochkontakt', 'Grammatik', 'Pragmatik' an 'Nimmgrammatik' kommen. D'Date si mat Hëllef vun eiser Schnëssen-App an der Zäit tëschent 2018 an 2020 erhuewe ginn. Pro Variabel sinn d'Opname vu bis zu 2000 Leit an am Ganze méi wéi 250.000 Observatioune verschafft ginn. Dës breet Datebasis erlaabt en eenzegaartegen an zouverlässegen Iwwerbléck iwwert déi haiteg Variatioun am Lëtzebuergeschen, souwuel aus regionaler wéi och aus sozialer Perspektiv. Doriwwer eraus kann een un de Kaarten a Visualiséierungen ofliesen, wéi staark d'Lëtzebuergescht am Moment amgaang ass sech ze veränneren."),
            unilu_button("Kaarten", route_link("kaarten"))
        ),
        unilu_img_map(src="img/map_1.png", i18n$t("Vergläichskaart aus dem “Luxemburgischer Sprachatlas”, LSA (1963), Kaart 025")), 
        unilu_img_map(src="img/map_2.png", i18n$t("D'Faarf korrespondéiert mat der heefegster Variant pro Polygon. D'Intensitéit weist d'Heefegkeet vun där Variant.1157 Participanten | Klengst Polygoner: Gemeng. ©Uni Lëtzbuerg | Generéiert Mon Feb 27 11:18:47 2023"))
    ),

    # features
    div(class = "row text-start ps-3",
        unilu_title_small("Funktionalitéiten")
    ),
    
    div(class = "row text-start gy-4 mb-5",
        unilu_card_feature("Kaarten", content[[1]]),
        unilu_card_feature("Korrelation mat Sozialdaten", content[[2]]),
        unilu_card_feature("Statistesch Analysen", content[[3]]),
        unilu_card_feature("Korrelatioun mat sozio-demografesche Facteure vun der Gemeng", markdown(content[[4]])),
        unilu_card_feature("Audio", content[[5]]),
        unilu_card_feature("Kaartekatalog", content[[6]]),
  ),

    # bilbliografie
    unilu_title_small("Bibliografie"),
    unilu_text_md(content[[7]]),
    unilu_text_md(content[[8]]),
    unilu_title_small("De Variatiounsatlas zitéieren"),
    unilu_text_md(content[[9]]),

    unilu_footer("Copyright © Université du Luxembourg, Department of Humanities, 2021-2023. All rights reserved.")


)
