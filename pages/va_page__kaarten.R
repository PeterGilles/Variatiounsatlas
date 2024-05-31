
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
va_page__kaarten <- page_fluid(
  
    tags$head(
        tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
        tags$script(src = "js/menu_show.js")
    ),
    title = "Variatiounsatlas | Kaarten | uni.lu",
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
            unilu_navpage("Kaarten", href = route_link("kaarten"), is_active = TRUE),
            unilu_navpage("Kaartekatalog", href = route_link("kaartekatalog")),
            unilu_navpage("Impressum", href = route_link("impressum")),
            unilu_navpage("Disabled", is_disabled = TRUE)
        ),
    
    # unilu_empty_block("3rem"),

    div(class = "d-flex flex-column flex-lg-row justify-content-end align-items-end", 
        div(class = "uni_kaart_select", 
            selectInput("variable", "", 
                choices = make_choices(), 
                selected = NULL, 
                multiple = FALSE, 
                selectize = TRUE, 
                width = "auto", 
                size = NULL
            )
        ),
        bookmarkButton(class = "uni_bookmark", icon = shiny::icon("bookmark", lib = "font-awesome"))
        # div(class = "uni_bookmark ms-3 mb-3",
        #     bookmarkButton(icon = shiny::icon("bookmark", lib = "font-awesome"))
        # )
    ),
    
    # sidebar
    # layout_sidebar(
    #     class = "p-0 ps-5",
    #     border = FALSE,
    #     border_radius = FALSE,

    #     sidebar = sidebar(
    #         width = 400,
    #         class = "uni_kaart_select border border-1 border-danger rounded-0 bg-secondary px-3 py-0",
    #         selectInput(
    #             "variable",
    #             "Variable",
    #             choices = make_choices(),
    #             selected = NULL,
    #             multiple = FALSE,
    #             selectize = TRUE,
    #             width = NULL,
    #             size = NULL
    #         )
    #     ),


        # Card 1 --------------------------------------------------------------
        uiOutput("iwwersiicht_variabel"),

        # Card 2 --------------------------------------------------------------
        unilu_card("Varianteverdeelung",  plotOutput("plotFreqVariants", height="100px")),

        # Card 3 --------------------------------------------------------------
        navset_card_tab(
            id = "uni_card_tab_1",
            title = tags$b(i18n$t("Kartografie")),
            #full_screen = TRUE,
            nav_panel(i18n$t("Iwwerbléckskaart"), withSpinner(girafeOutput("Iwwerbléckskaart"))),
            nav_panel(i18n$t("Kaarten nom Alter"), withSpinner(plotOutput("Iwwerbléckskaart_no_alter"))),
            #tabPanel(i18n$t("Altersanimatioun"), withSpinner(plotOutput("Iwwerbléckskaart_dynamic", height = "700px")))
            nav_panel(i18n$t("Variantekaarten"), withSpinner(uiOutput("plot.ui")),
                          downloadButton("downloadData", "")),
            footer = tags$div(tags$em(i18n$t("D'Faarf korrespondéiert mat der heefegster Variant pro Polygon an d'Verbreedung vun där Variant gëtt besser visibel. D'Intensitéit vun der Faarf weist déi relativ Heefegkeet vun där Variant. Wat méi intensiv, wat d'Heefegkeet méi héich ass. Wann d'Faarf méi hell ass, dann ass déi Gemeng/dee Kanton duerch Mëschung vu verschiddene Variante charakteriséiert. D'Beweegung mat der Maus liwwert weider Infoe iwwert d'Varianteverdeelung. Fir wäiss Polygoner leie keng Date vir.")))
        ),

        # Card 4 --------------------------------------------------------------
        navset_card_tab(
            id = "uni_card_tab_2",
            title = tags$b(i18n$t("Sozialdaten")),
            nav_panel(i18n$t("Alter"), plotOutput("plotAlter")),
            nav_panel(i18n$t("Geschlecht"), plotOutput("plotGeschlecht")),
            nav_panel(i18n$t("Dialektgebiet"), plotOutput("plotDialektgebiet")),
            # deaktivéiert, 23.1.2023
            # nav_panel("Sprooch- & Educatiounsindex", plotOutput("plotLangEduIndex")),
            nav_panel(i18n$t("Ausbildung"), plotOutput("plotAusbildung")),
            nav_panel(i18n$t("Kompetenz Däitsch"), plotOutput("plotKompetenzD")),
            nav_panel(i18n$t("Kompetenz Franséisch"), plotOutput("plotKompetenzF")),
            tabPanel(i18n$t("Sproochekompetenz"), plotOutput("plotSproochekompetenz")),
            nav_panel(i18n$t("Mammesprooch"), plotOutput("plotMammesprooch")),
            nav_panel(i18n$t("Sproochlech Aflëss"), plotOutput("plotAfloss"))
        ),

        # Card 5 --------------------------------------------------------------
        navset_card_tab(
            id = "uni_card_tab_3",
            title = tags$b(i18n$t("Statistesch Analysen")),
            # print text + plot output of various social factors reactive 
            nav_panel(i18n$t("Logistesch Regressioun"), plotOutput("plotLModel"), htmlOutput("textLModel")),
            nav_panel(i18n$t("Variable Importance"), plotOutput("plotVariableImportance")),
            # Plot Random Forest tree
            nav_panel(i18n$t("Random Forest tree"), plotOutput("plotTree"))
        ),

        # Card 6 --------------------------------------------------------------
        navset_card_tab(
            title = tags$b(i18n$t("Sozio-demografie vun der Gemeng")),
            id = "uni_card_tab_4",
            nav_panel(i18n$t("Urbanisatioun"), plotOutput("plotUrbanisatioun")),
            nav_panel(i18n$t("Awunner_km2"), plotOutput("plotAwunner")),
            nav_panel(i18n$t("Indice socio-économique"), plotOutput("plotIndex")),
            nav_panel(i18n$t("Immigratioun"), plotOutput("plotImmigratioun"))
        ),
    
        # Card 7 --------------------------------------------------------------
        unilu_card(i18n$t("Audioopnamen"),  DTOutput("plotAudio", height = "600px")),

    # ),


    unilu_footer("Copyright © Université du Luxembourg, Department of Humanities, 2021-2023. All rights reserved.")


)