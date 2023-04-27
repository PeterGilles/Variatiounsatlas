#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("global.R")

## UI
ui <- function(request) {
  
  autoWaiter()
  
  dashboardPage(
    # shinydashboardPlus skins
    skin = "black-light",
    preloader = preloader,
    
    header = dashboardHeader(
      tags$li(class = "dropdown",
             tags$a(href="https://uni.lu", target="_blank",
                    tags$img(height = "20px", alt="Logo", src="https://dico.uni.lu/images/unilu.png")
             )
      ),
      title = "Variatiounsatlas vum Lëtzebuergeschen",
      titleWidth = 360),
    
    sidebar = dashboardSidebar(
      tags$style(HTML(".sidebar-menu li a { font-size: 18px!important; 
                      font-family: 'Roboto';}")),
      sidebarMenu(
        id = "sidebarid",
        menuItem("Intro", tabName = "intro"),
        menuItem("Neiegkeeten", tabName = "news"),    
        
        # page Datebasis
        # lädt nicht!
        #menuItem("Datebasis", tabName = "struktur"),
        
        menuItem("Kaarten", tabName = "kaartekomplexer",
                 startExpanded = TRUE,
                 expandedName = "kaartekomplexer",
                 selected = FALSE
        ),
        conditionalPanel(
          # inputSelect wird ausgeklappt, wenn MenuItem 'Kaarten' angewählt wird
          'input.sidebarid == "kaartekomplexer"',
          # two selectInputs dependent on each other
          # see observeEvent() belwp
          # selectInput(inputId="map_category",
          #             label="Wielt eng Kategorie",
          #             choices=unique(variables$map_category),
          #             selected= unique(variables$map_category)[1]
          # ),
          selectInput(inputId="variable",
                      label = "",
                      selectize = FALSE,
                      multiple = FALSE,
                      size= 20,
                      choices = make_choices()
                      #choices=unique(variables$variable)
                      # selected klappt net
                      #selected= unique(variables$variable)[2]
          ),
          bookmarkButton(icon = shiny::icon("bookmark", lib = "font-awesome"))
        ),
        
        menuItem("Kaartekatalog", tabName = "kaartekatalog"),

        menuItem("Merci", tabName = "outro")
      )
    ),
    body = dashboardBody(
      tags$head(
        #tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css"),
        tags$link(rel = "shortcut icon", href = "favicon.png")
      ),
      tabItems(
        # page Intro ----
        tabItem(tabName = "intro", includeMarkdown("intro.md")),
        
        # page Kaartekomplexer ----
        tabItem(
          tabName = "kaartekomplexer",
          # fluidRow(
          #   box(
          #     selectInput(inputId="variable",
          #                 label = "Wielt eng Kaart ",
          #                 #selectize = TRUE,
          #                 #multiple = FALSE,
          #                 #size= 20,
          #                 choices=make_choices()
          #                 # wenn map_category auch als input select existiert
          #                 #choices=unique(variables$variable)
          #     )
          #   )
          # ),
          fluidRow(
              valueBoxOutput("variable", width = 12)),
          
          fluidRow(
            box(
              plotOutput("plotFreqVariants", height="100px"),
              title= "Varianteverdeelung",
              width = 12,
              status = "primary",
              collapsible = T,
              solidHeader = T)
          ),
          
          fluidRow(
            box(
              title = "Kartografie",
              footer = "D'Faarf korrespondéiert mat der heefegster Variant pro Polygon an d'Verbreedung vun där Variant gëtt besser visibel. D'Intensitéit vun der Faarf weist déi relativ Heefegkeet vun där Variant. Wat méi intensiv, wat d'Heefegkeet méi héich ass. Wann d'Faarf méi hell ass, dann ass déi Gemeng/dee Kanton duerch Mëschung vu verschiddene Variante charakteriséiert. D'Beweegung mat der Maus liwwert weider Infoe iwwert d'Varianteverdeelung. Fir wäiss Polygoner leie keng Date vir.",
              solidHeader = T,
              width = 12,
              collapsible = T,
              status = "primary",
              tabBox(
                id = "kaarten",
                width = 12,
                type = "tabs",
                #side="right",
                tabPanel("Iwwerbléckskaart", withSpinner(girafeOutput("Iwwerbléckskaart", height = "700px"))),
                tabPanel("Kaarten nom Alter", withSpinner(plotOutput("Iwwerbléckskaart_no_alter", height = "600px"))),
                #tabPanel("Altersanimatioun", withSpinner(plotOutput("Iwwerbléckskaart_dynamic", height = "700px")))
              )
            )
          ),
          
          fluidRow(
            box(
              title = "Variantekaarten",
              footer ="Dës Variantekaarte weisen d'quantitativ Verdeelung separat pro Variant. Esou gëtt d'Stabilitéit, Ausbreedung an d'Zeréckgoe vun enger Variant visualiséiert.",
              solidHeader = T,
              width = 12,
              collapsible = T,
              status = "primary",
              uiOutput("plot.ui"),
              downloadButton("downloadData", "")
            )
          ),
          
          fluidRow(
            box(title = "Sozialdaten",
                footer = "D'Zuel an de faarwege Segmenter steet fir d'Unzuel vu Participantë pro Kategorie.\nD'Prozenter pro Variant loosse sech iwwert d'Gréisst vum Segment op der y-Achs ofliesen.",
                width = 12,
                status = "primary",
                collapsible = T,
                solidHeader = T,
                tabBox(
                  #title = "Sozialdaten",
                  id = "sozialdaten",
                  width = 12,
                  type = "tabs",
                  #side="right",
                  tabPanel("Alter", plotOutput("plotAlter")),
                  tabPanel("Geschlecht", plotOutput("plotGeschlecht")),
                  tabPanel("Dialektgebiet", plotOutput("plotDialektgebiet")),
                  tabPanel("Ausbildung", plotOutput("plotAusbildung")),
                  tabPanel("Kompetenz Däitsch", plotOutput("plotKompetenzD")),
                  tabPanel("Kompetenz Franséisch", plotOutput("plotKompetenzF")),
                  tabPanel("Mammesprooch", plotOutput("plotMammesprooch")),
                  tabPanel("Sproochlech Aflëss", plotOutput("plotAfloss"))
                )
            )
          ),
          
          fluidRow(
            box(title = "Sozio-demografesch Facteure vun der Gemeng",
                footer = "D'Zuel an de faarwege Segmenter steet fir d'Unzuel vu Participantë pro Kategorie.\nD'Prozenter pro Variant loosse sech iwwert d'Gréisst vum Segment op der y-Achs ofliesen.",
                width = 12,
                collapsible = T,
                solidHeader = T,
                status = "primary",
                tabBox(
                  id = "demographie",
                  width = 12,
                  type = "tabs",
                  #side="right",
                  tabPanel("Urbanisatioun", plotOutput("plotUrbanisatioun")),
                  tabPanel("Awunner_km2", plotOutput("plotAwunner")),
                  tabPanel("Indice socio-économique", plotOutput("plotIndex")),
                  tabPanel("Immigratioun", plotOutput("plotImmigratioun"))
                )
            )
          ),
          # deactivated, replace with regression analysis
          # fluidRow(
          #   box(
          #     title = "Inference decision tree",
          #     solidHeader = T,
          #     width = 12,
          #     collapsible = T,
          #     p("(fir Varianten, déi op d'mannst mat 20 % virkommen)"),
          #     plotOutput("plotTree", height = "600px")
          #   )
          # ),
          # fluidRow(
          #   box(
          #     title = "Variable importance (Random Forest)",
          #     solidHeader = T,
          #     width = 12,
          #     collapsible = T,
          #     plotOutput("plotVariableImportance", height = "400px")
          #   )
          # ),
          
          fluidRow(
            box(
              title = "Audioopnamen",
              solidHeader = T,
              status = "primary",
              width = 12,
              height = 600,
              collapsible = T,
              DTOutput("plotAudio", height = "600px")
            )
          )
        ),
        
        # page Kaartekatalog
        tabItem(tabName = "kaartekatalog",
                includeMarkdown(paste("kaartekatalog.md")),
                DTOutput("katalog")),
        
        # page News
        tabItem(tabName = "news", includeMarkdown("news.md")),
        
        # page Outro
        tabItem(tabName = "outro", includeMarkdown("outro.md"))
      )
    ), # dashboardPage
    footer = dashboardFooter(
      left = "Peter Gilles",
      right = "Universitéit Lëtzebuerg, 2022"
    )
  )
}

# Server
server <- function(input, output, session) {
  
  # observe function which updates the second selectInput when the 
  #  first selectInput is changed
  # observe({updateSelectInput(session,
  #                            inputId="variable", 
  #                            choices=make_choices(input$map_category))
  # })
  
  setBookmarkExclude(c("katalog_rows_current", "plotAudio_rows_all", "sozialdaten", "plotAudio_rows_current", "plotAudio_cell_clicked", "plotAudio_state"))
  
  # map_category <- reactive({
  #   req(input$map_category)
  #   filter(variables, map_category == input$map_category)
  # })
  # input_choice <- reactive({
  #   req(input$input_choice)
  #   filter(map_category(), input_choice == input$input_choice)
  # })
  
  # Reactive function for the variable
  variable <- reactive({
    req(input$variable)
    input$variable
  })
  
  # Reactive function for the variable_name
  variable_name <- reactive({
    variables %>%
      dplyr::filter(variable == variable()) %>%
      dplyr::select(input_choice)
  })
  
  # Reactive function for the selection
  selection <- reactive({
    selection <- variables %>%
      filter(variable == variable()) %>%
      pull(selection) %>%
      as.character() %>%
      as.vector()
    selection <- strsplit(selection, " ")[[1]]
    return(selection)
  })
  
  
  # Reactive function to load the appropriate Google table based on the selected variable
  google_df <- reactive({
    # Obtain the Google table name associated with the selected variable
    google_df_name <- variables %>%
      filter(variable == variable()) %>% pull(google_df)
    
    # Load and return the QS file associated with the table name
    qread(paste0(google_df_name, ".qs"))
  })
  
  # Reactive function to pivot the Google table to a longer format and filter by selected variants
  longer_google_df <- reactive({
    google_df() %>%
      # filter only data from Luxembourg
      filter(Kanton != "") %>%
      # Pivot the table to a longer format
      pivot_longer(cols = variable(),
                   names_to = "variable",
                   values_to = "variants") %>%
      # Filter rows based on selected variants
      filter(variants %in% selection())
  })
  
  # Reactive function to prepare data for maps
  prepare_data <- reactive({
    
    # Get the longer_google_df
    longer_google_df <- longer_google_df()
    
    # Determine the base polygon (canton or commune) based on the number of participants per item
    geo_type <- if (nrow(longer_google_df) <= 2200) "Kanton" else "Gemeng"
    
    # Pivot the table and add a 'geo_type' column
    longer_google_df <- longer_google_df %>%
      pivot_longer(cols = geo_type,
                   names_to = "geo_type",
                   values_to = "geo_name") %>%
      mutate(variants = factor(variants, ordered = TRUE))
    
    # Calculate the total and frequency of each variant
    variants_total <- longer_google_df %>%
      filter(geo_type == geo_type) %>%
      group_by(geo_name, variable(), variants) %>%
      count() %>%
      group_by(geo_name, variable()) %>%
      mutate(total = sum(n)) %>%
      mutate(freq = n / total)
    
    # Attach the total population per commune
    Gemengen_Statistiken <- qread("Gemengen_Statistiken.qs")
    awunner <- Gemengen_Statistiken %>%
      dplyr::distinct(Gemeng, Awunnerzuel)
    
    variants_total <- left_join(variants_total, awunner, by = c("geo_name" = "Gemeng"))
    
    # Add a weighted average column
    variants_total <- variants_total %>%
      mutate(weighted_freq = freq * (Awunnerzuel / 601400))
    
    # Add the color column based on the selection
    color <- color_palette
    color_column <- tribble(~variants, ~color,
                            selection()[1], color[1],
                            selection()[2], color[2],
                            selection()[3], color[3],
                            selection()[4], color[4],
                            selection()[5], color[5],
                            selection()[6], color[6],
                            selection()[7], color[7],
                            selection()[8], color[8])
    
    variants_total <- left_join(variants_total, color_column, copy = TRUE)
    
    # Join the data with the corresponding geographical data (cantons_df or communes_df)
    df <- if (geo_type == "Kanton") {
      inner_join(cantons_df, variants_total, by = c("id" = "geo_name"), multiple = "all")
    } else {
      inner_join(communes_df, variants_total, by = c("id" = "geo_name"), multiple = "all")
    }
    
    return(list("dataset" = df, "geo_type" = geo_type))
  })
  
  
  # Reactive function to prepare data for maps with age information
  prepare_data_age <- reactive({
    
    # Get the longer_google_df
    longer_google_df <- longer_google_df()
    
    # Determine the base polygon (canton or commune) based on the number of participants per item
    geo_type <- if (nrow(longer_google_df) <= 2200) "Kanton" else "Gemeng"
    
    # Recode age, pivot the table, and add a 'geo_type' column
    longer_google_df <- longer_google_df %>%
      mutate(Alter = case_match(Alter,
                                c("≤ 24", "25 bis 34") ~ "jonk",
                                c("35 bis 44", "45 bis 54") ~ "mëttel-al",
                                c("55 bis 64", "65+") ~ "eeler")
      ) %>%
      mutate(Alter = factor(Alter, levels = c("eeler", "mëttel-al", "jonk"))) %>%
      pivot_longer(cols = geo_type,
                   names_to = "geo_type",
                   values_to = "geo_name") %>%
      mutate(variants = factor(variants, ordered = TRUE))
    
    # Calculate the total and frequency of each variant, grouped by age
    variants_total <- longer_google_df %>%
      filter(geo_type == geo_type) %>%
      group_by(Alter, geo_name, variable(), variants) %>%
      count() %>%
      group_by(Alter, geo_name, variable()) %>%
      mutate(total = sum(n)) %>%
      mutate(freq = n / total)
    
    #saveRDS(variants_total, "test.rds")
    
    # Add the color column based on the selection
    color <- color_palette
    color_column <- tribble(~variants, ~color,
                            selection()[1], color[1],
                            selection()[2], color[2],
                            selection()[3], color[3],
                            selection()[4], color[4],
                            selection()[5], color[5],
                            selection()[6], color[6],
                            selection()[7], color[7],
                            selection()[8], color[8])
    
    variants_total <- left_join(variants_total, color_column, copy = TRUE)
    
    # Join the data with the corresponding geographical data (cantons_df or communes_df)
    df <- if (geo_type == "Kanton") {
      inner_join(cantons_df, variants_total, by = c("id" = "geo_name"), multiple = "all")
    } else {
      inner_join(communes_df, variants_total, by = c("id" = "geo_name"), multiple = "all")
    }
    
    return(list("dataset" = df, "geo_type" = geo_type))
  })
  
  
### infoBox for overview of variable
  output$variable <- renderInfoBox({
    infoBox(
      title = paste0("Phenomeen aus dem Beräich '", variables %>% filter(variable == variable()) %>% dplyr::select(map_category), "'"),
      value = paste0("Variabel: ", variable_name()),
      # for formatting see: https://stackoverflow.com/questions/34234107/r-shiny-dashboard-infobox-over-two-lines
      subtitle = HTML(paste0("Offrokontext: ", variables %>%
                               filter(variable == variable()) %>% 
                               dplyr::select(item_text), 
                             br(),
                             "Analyséiert Varianten: ",
                             toString(as.vector(as_tibble(selection()))), # TODO: nach net ganz schéin!
                             br(),
                             "Schnëssen-Item: ", variables %>% 
                               filter(variable == variable()) %>% 
                               dplyr::select(item_number),
                             br(),
                             "Ausgewäert Realiséierungen: ",
                             nrow(longer_google_df())
      )
      ),
      icon = icon(name="ice-lolly-tasted", lib = "glyphicon"),
      color = "light-blue",
      width = 12
    )
  })
  
  # Plot overall distribution of variants #
  output$plotFreqVariants <- renderPlot({
    plot_freq_variants(data = google_df(), variable = variable(), selection = selection())
    
  }) 
  
  ### Iwwerbléckskaart #

  output$Iwwerbléckskaart <- renderGirafe({
    
    lsa_map_number <- variables %>% filter(variable == variable()) %>% pull(lsa_map_number) %>% as.character()
    
    # pull item number from tribble
    item_number <- variables %>% filter(variable == variable()) %>% pull(item_number) %>% as.character()
    
    # pull item text from tribble
    item_text <- variables %>% filter(variable == variable()) %>% pull(item_text) %>% as.character()
    
    if(file.exists(paste0("Iwwerbleckskaart_", variable(), ".qs"))) {
      qread(paste0("Iwwerbleckskaart_", variable(), ".qs"))
    }
    else {
      print(paste(variable(),": Iwwerbléckskaart gëtt generéiert"))
      make_summary_plot(data = prepare_data()[["dataset"]], lsa_map_number = lsa_map_number, 
                        selection= selection(), color_num = length(selection()), map_title = variable(), item_number, 
                        item_text, geo_type = prepare_data()[["geo_type"]])
    }
  })
  

  ### Iwwerbléckskaarte per age #
  
  output$Iwwerbléckskaart_no_alter <- renderPlot({
    lsa_map_number <- variables %>% filter(variable == variable()) %>% pull(lsa_map_number) %>% as.character()
    
    # pull item number from tribble
    item_number <- variables %>% filter(variable == variable()) %>% pull(item_number) %>% as.character()
    
    # pull item text from tribble
    item_text <- variables %>% filter(variable == variable()) %>% pull(item_text) %>% as.character()
    
    # if(file.exists(paste0("Iwwerbleckskaarten_Alter_", variable(), ".qs"))) {
    #   qread(paste0("Iwwerbleckskaarten_Alter_", variable(), ".qs"))
    # }
    # else {
      print(paste(variable(),": Eenzel Alterskaaarten gi generéiert"))
      make_summary_plot_age(data = prepare_data_age()[["dataset"]], lsa_map_number = lsa_map_number, 
                            selection= selection(), color_num = length(selection()), map_title = variable(), item_number, 
                            item_text, geo_type = prepare_data()[["geo_type"]])
    # }
  })
  
  ### Iwwerbléckskaart animation with age #
  
  output$Iwwerbléckskaart_dynamic <- renderImage({
    lsa_map_number <- variables %>% filter(variable == variable()) %>% pull(lsa_map_number) %>% as.character()
    
    # pull item number from tribble
    item_number <- variables %>% filter(variable == variable()) %>% pull(item_number) %>% as.character()
    
    # pull item text from tribble
    item_text <- variables %>% filter(variable == variable()) %>% pull(item_text) %>% as.character()
    
    # if(file.exists(paste0("Iwwerbleckskaart_age_animation_", variable(), ".gif"))) {
    #   # get a list containing the filename
    #   # this can be displayed then with imageOutput
    #   list(src = paste0("Iwwerbleckskaart_age_animation_", variable(), ".gif"), contentType = "image/gif")
    # }
    # else {
      print(paste(variable(),": GIF gëtt generéiert"))
      make_summary_plot_age_dynamic(data = prepare_data_age()[["dataset"]], lsa_map_number = lsa_map_number, 
                                    selection= selection(), color_num = length(selection()), map_title = variable(), item_number, 
                                    item_text, geo_type = prepare_data()[["geo_type"]])
    # }
  },
  deleteFile = TRUE)
  
  #####################
  ### Variantekaarten #
  #####################
  
  # separate plot function for download link
  myplot <- reactive({
    # function to generalte a df containing the individual maps for the selected variants
    # make_plot() is the actual drawing function to create one map
    # each map is stored in plots$plots
    all_maps <- function(data) {
      plots <- data %>%
        group_by(variants, color)   %>%
        nest()  %>%
        drop_na() %>%
        mutate(plots = pmap(list(variable = variants,
                                 dataset = data,
                                 color = color),
                            make_plot))
      # cowplot is used to arrange these maps in a grid - too large gg objects
      #p <- cowplot::plot_grid(plotlist = plots$plots, ncol = 2)
      # using now patchwork - smaller
      p <- wrap_plots(plots$plots, ncol = 3)
      rm(plots, data)
      
      # save map as qs
      #print("saving Variantekaarten")
      #qsave(p, file = paste0("Variantekaarten_", variable(), ".qs"))
      
      #ggsave(plot = p, filename = paste0("variantekaart_", ".pdf"), units = "cm", width = 22)
      return(p)
    }
    
    # if qs file already exist, display it (faster); if not, create the map (slower)
    # if(file.exists(paste0("Variantekaarten_", variable(), ".qs"))) {
    #   print("lokal Variantekaarten")
    #   qread(paste0("Variantekaarten_", variable(), ".qs"))
    # }
    # else {
      print(paste(variable(),": Varianteaarte gi generéiert"))
      all_maps(data = prepare_data()$dataset)
    # }
  })
  
  # plot function
  output$Variantekaarten <- renderPlot({
    myplot()
  })
  
  # determine height by number of variants in plot
  plotHeight <- reactive((700 + 20) * (ceiling(length(selection()) / 3 )) - 20 - 200)

  # render plot as UI (not plot!) to maintain height attribute
  output$plot.ui <- renderUI({
    plotOutput("Variantekaarten", height = plotHeight())
  })
  
  # create download button for Variantekaarten
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Variantekaarten_", variable(), Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      # TODO: size attributes not optimal
      png(file=file, height = plotHeight(), width = 960)
      # call separate plot function from above
      plot(myplot())
      dev.off()
    }
  )
  
  output$plotAlter <- renderPlot({
    
    # variableFonInput <- input$kaart
    # variable <- variableFonInput
    # 
    # # pull variant selection list from tribble
    # selection <- variables %>% filter(variable == variableFonInput) %>% pull(selection) %>% as.character() %>% as.vector()
    # selection <- strsplit(selection, " ")[[1]]
    
    caption = ""
    
    plot_social_categories(data = google_df(), Alter, variable = variable(), selection = selection(), caption = caption)
    
  }) 
  
  # plot function Geschlecht
  output$plotGeschlecht <- renderPlot({
    
    plot_social_categories(data = google_df(), Geschlecht, variable = variable(), selection = selection())
    
  }) 
  
  # plot function Dialektgebiet
  output$plotDialektgebiet <- renderPlot({
    
    caption = ""
    
    plot_social_categories(data = google_df(), Dialektgebiet, variable = variable(), selection = selection(), caption = caption)
    
  }) 
  
  # plot Ausbildung
  output$plotAusbildung <- renderPlot({
    
    caption = ""
    
    plot_social_categories(data = google_df(), Ausbildung, variable = variable(), selection = selection(), caption = caption)
    
  })
  
  # plot function Kompetenz D
  output$plotKompetenzD <- renderPlot({
    
    caption = "Baséiert op Selbstaschätzung. 1 = bal guer keng Kompetenz, 7 = perfekt Kompetenz. \nFir d'Interpretatioun si just d'Stufe vu 5 bis 7 reliabel."
    
    plot_social_categories(data = google_df(), `Kompetenz am Däitschen`, variable = variable(), selection = selection(), caption = caption)
    
  })
  
  # plot function Kompetenz F
  output$plotKompetenzF <- renderPlot({
    
    caption = "Baséiert op Selbstaschätzung. 1 = bal guer keng Kompetenz, 7 = perfekt Kompetenz. \nFir d'Interpretatioun si just d'Stufe vun 3 bis 7 reliabel."
    
    plot_social_categories(data = google_df(), `Kompetenz am Franséischen`, variable = variable(), selection = selection(), caption = caption)
    
  })
  
  # plot function Mammesprooch
  output$plotMammesprooch <- renderPlot({
    
    caption = ""
    
    plot_social_categories(data = google_df(), `Mammesprooch`, variable = variable(), selection = selection(), caption = caption)
    
  })
  # plot function Aflëss
  output$plotAfloss <- renderPlot({
    
    caption = "Baséiert op Selbstaschätzung."
    
    plot_social_categories(data = google_df(), `Sproochlech Aflëss`, variable = variable(), selection = selection(), caption = caption)
    
  })
  
  # plot function Urbanisatioun
  output$plotUrbanisatioun <- renderPlot({
    
    caption = "Baséiert op dem 'Degree of Urbanisation' (DEGRUBA).\n1 = staark urbaniséiert (= d'Stad Lëtzebuerg), 3 = wéineg urbaniséiert\nSource: STATEC (https://statistiques.public.lu/en/themes/population-emploi.html)"
    
    plot_social_categories(data = google_df(), Urbanisatioun, variable = variable(), selection = selection(), caption = caption)
    
  })
  
  # plot function Awunner_km2
  output$plotAwunner <- renderPlot({
    
    caption = ""
    
    plot_social_categories(data = google_df(), Awunner_km2, variable = variable(), selection = selection(), caption = caption)
    
  })
  # plot function Indice socio-économique
  output$plotIndex <- renderPlot({
    
    caption = "Baséiert op dem 'Indice socio-économique' pro Gemeng.\n\nSource: STATEC (https://statistiques.public.lu/dam-assets/catalogue-publications/bulletin-Statec/2017/bulletin-2-17.pdf)"
    
    plot_social_categories(data = google_df(), `Indice socio-économique`, variable = variable(), selection = selection(), caption = caption)
    
  })
  
  # plot function Immigratioun
  output$plotImmigratioun <- renderPlot({
    
    caption = "Prozentzuel vun Net-Lëtzebuerger pro Gemeng\nSource: STATEC: https://data.public.lu/en/datasets/population-de-residence-habituelle-par-commune-et-nationalite-au-1er-fevrier-2011/"
    
    plot_social_categories(data = google_df(), Immigratioun, variable = variable(), selection = selection(), caption = caption)
    
  })
  
  
  # plot inference decision tree
  output$plotTree <- renderPlot({
    
    plot_decision_tree(data = google_df(), variable = variable(), selection = selection())
  })
  
  # # plot variable importance from random forest
  # output$plotVariableImportance <- renderPlot({
  #   
  #   if(file.exists(paste0(variable(), "_VariableImportance.qs"))) {
  #     print("lokal VariableImportance")
  #     qread(paste0(variable(), "_VariableImportance.qs"))
  #   }
  #   else {
  #     print("VariableImportance gëtt generéiert")
  #     plot_VariableImportance(data = google_df(), variable = variable(), selection = selection())
  #  }
  # })
  
  # plot function Audio
  output$plotAudio <- renderDT({
    
    plot_datatable(data = google_df(), variable = variable())
  })
  
  ## Plot Kaartekatalog
  output$katalog <- renderDT({
    datatable(variables %>%
                dplyr::select(Variabel = input_choice, Varianten = selection, Kategorie = map_category, Offrokontext = item_text, 
                              variable, Item = item_number, `LSA-Kaart` = lsa_map_number) %>%
                # Links for bookmarks
                # shinyapps
                mutate(Variabel = paste0("<a href=", SERVER, "?_inputs_&sidebarid=%22kaartekomplexer%22&variable=%22", variable, "%22>", Variabel, "</a>")) %>%
                # engelmann
                # mutate(Variabel = paste0("<a href=", SERVER_local, "?_inputs_&sidebarid=%22kaartekomplexer%22&variable=%22", variable, "%22>", Variabel, "</a>")) %>%
                dplyr::select(-variable),  
              escape = FALSE,
              #height = 600,
              extensions = 'Scroller',
              filter = 'top', options = list(
                deferRender = TRUE,
                scrollY = 600,
                scroller = TRUE,
                autoWidth = TRUE)
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")
