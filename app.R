source("global.R")

## UI
ui <- function(request) {
  
  autoWaiter()

  fluidPage(
    lang = "de",
    theme = bs_theme(version = 5,
                     bootswatch = "cerulean",
                     base_font = font_google("Lato"),
                     heading_font = font_google("Montserrat"),
                     #"navbar_bg" = "#FBFF90",
                     #"nav-link-font-size" = "20px",
                     ),
                            
    tags$head(
      tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon.png"),
      #tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),

    titlePanel(div(h1(img(src = "uni-logo.png", width="4%"), style="margin-left:15px;margin-top:10px", "Variatiounsatlas vum Lëtzebuergeschen")), windowTitle =  "Variatiounsatlas vum Lëtzebuergeschen"),
    
  page_navbar(
    id = "nav",
    
    # Create Right Side Logo/Image with Link       
    title = tags$script(HTML("var header = $('.navbar > .container-fluid'); 
                     header.append('<div style=\"float:left\"><a href=\"#\"><img src=\"logo_schnessen_transparent.png\" alt=\"alt\" style=\"float:left;width:38px;margin-left:-\"> </a></div>');
    console.log(header)")
    ),

    sidebar = sidebar(
      id = "sidebar",
      open = FALSE,
      
      conditionalPanel(
        # inputSelect wird ausgeklappt, wenn MenuItem 'Kaarten' angewählt wird
        'input.nav == "Kaarten"',

        # two selectInputs dependent on each other
        # see observe function below in server section!
        # selectInput(inputId="map_category",
        #             label="Wielt eng Kategorie",
        #             choices = unique(variables$map_category),
        #             selected = unique(variables$map_category)[1]
        # ),
        # selectInput(inputId="variable",
        #             label = "Wielt eng Kaart",
        #             selectize = FALSE,
        #             multiple = FALSE,
        #             size= 20,
        #             choices=unique(variables$variable)
        # ),
        selectInput(inputId="variable",
                    label = "",
                    selectize = FALSE,
                    multiple = FALSE,
                    size= 20,
                    choices = make_choices()
        ),
        bookmarkButton(icon = shiny::icon("bookmark", lib = "font-awesome"))
      )
    ),

    nav_panel("Intro", includeMarkdown("intro.md")),
    nav_panel("News", includeMarkdown("news.md")),
    nav_panel("Detailer", includeMarkdown("detailer.md")),
    nav_panel("Kaarten",
              
              # Iwwersiicht iwwer Variabel
              uiOutput("iwwersiicht_variabel"),
              
              # Varianteverdeelung
              card(
                card_header(
                  tags$b("Varianteverdeelung")
                  ),
                card_body(
                  plotOutput("plotFreqVariants", height="100px")
                  )
                  ),
              
              navset_card_tab(
                title = tags$b("Kartografie"),
                #full_screen = TRUE,
                nav_panel("Iwwerbléckskaart", withSpinner(girafeOutput("Iwwerbléckskaart"))),
                nav_panel("Kaarten nom Alter", withSpinner(plotOutput("Iwwerbléckskaart_no_alter"))),
                #tabPanel("Altersanimatioun", withSpinner(plotOutput("Iwwerbléckskaart_dynamic", height = "700px")))
                nav_panel("Variantekaarten", withSpinner(uiOutput("plot.ui")),
                          downloadButton("downloadData", "")),
                footer = tags$div(tags$em("D'Faarf korrespondéiert mat der heefegster Variant pro Polygon an d'Verbreedung vun där Variant gëtt besser visibel. D'Intensitéit vun der Faarf weist déi relativ Heefegkeet vun där Variant. Wat méi intensiv, wat d'Heefegkeet méi héich ass. Wann d'Faarf méi hell ass, dann ass déi Gemeng/dee Kanton duerch Mëschung vu verschiddene Variante charakteriséiert. D'Beweegung mat der Maus liwwert weider Infoe iwwert d'Varianteverdeelung. Fir wäiss Polygoner leie keng Date vir."))
    ),
              navset_card_tab(
                title = tags$b("Korrelatioun mat Sozialdaten"),

                nav_panel("Alter", plotOutput("plotAlter")),
                nav_panel("Geschlecht", plotOutput("plotGeschlecht")),
                nav_panel("Dialektgebiet", plotOutput("plotDialektgebiet")),
                nav_panel("Sprooch- & Educatiounsindex", plotOutput("plotLangEduIndex")),
                nav_panel("Ausbildung", plotOutput("plotAusbildung")),
                nav_panel("Kompetenz Däitsch", plotOutput("plotKompetenzD")),
                nav_panel("Kompetenz Franséisch", plotOutput("plotKompetenzF")),
                #tabPanel("Sproochekompetenz", plotOutput("plotSproochekompetenz")),
                nav_panel("Mammesprooch", plotOutput("plotMammesprooch")),
                nav_panel("Sproochlech Aflëss", plotOutput("plotAfloss"))
              ),
    
              navset_card_tab(
                title = tags$b("Statistesch Analysen"),
                
                # print text + plot output of various social factors reactive 
                nav_panel("Logistesch Regressioun", 
                          # build the selectInput for the variable
                          # choices should also contain the interaction terms
                          selectInput("selectedVariable", "Select a variable:", choices = c("langEduIndex_raw", "`Kompetenz am Franséischen`", "Alter", "Geschlecht", "Dialektgebiet")),
                          htmlOutput("textLModel"),
                          plotOutput("plotLModel")),
                nav_panel("Variable Importance", plotOutput("plotVariableImportance")),
                # Plot Random Forest tree
                nav_panel("Random Forest tree", plotOutput("plotTree"))
              ),
    
              navset_card_tab(
                title = tags$b("Korrelatioun mat sozio-demografesche Facteure vun der Gemeng"),

                nav_panel("Urbanisatioun", plotOutput("plotUrbanisatioun")),
                nav_panel("Awunner_km2", plotOutput("plotAwunner")),
                nav_panel("Indice socio-économique", plotOutput("plotIndex")),
                nav_panel("Immigratioun", plotOutput("plotImmigratioun"))
              ),
    
              card(
                card_header(tags$b("Audioopnamen")),
                card_body(DTOutput("plotAudio", height = "600px"))
              )
    ),
    
    nav_panel("Kaartekatalog", includeMarkdown(paste("kaartekatalog.md")),
                            DTOutput("katalog")),
    nav_panel("Impressum", includeMarkdown("outro.md")),
    
    footer = tags$div("Copyright © Université du Luxembourg, Department of Humanities, 2021-2023. All rights reserved.", class="navbar navbar-expand-lg bg-light")
  )
  )
}

# Server
server <- function(input, output, session) {
  
  # show sidebar only for 'Kaarten'
  observe({
    sidebar_toggle(
      id = "sidebar",
      open = input$nav == "Kaarten"
    )
  })
  # observe function which updates the second selectInput when the 
  # first selectInput is changed
  # observe({updateSelectInput(session,
  #                            inputId="variable",
  #                            choices=make_choices(input$map_category),
  #                            selected=unique(variables$variable))
  # })
  
  setBookmarkExclude(c("katalog_rows_current", "plotAudio_rows_all", "sozialdaten", "plotAudio_rows_current", "plotAudio_cell_clicked", "plotAudio_state"))
  
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
    qread(paste0("./raw_tables/", google_df_name, ".qs"))
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
  
  # Define the reactive function to determine the most frequent variant
  most_frequent_variant <- reactive({
    # Get the counts of each variant
    input_data <- google_df() %>%
      dplyr::select(variable()) %>%
      dplyr::filter_at(vars(variable()), ~ . != "FALSE")
    counts <- table(input_data)
    # Get the name of the variant with the highest count
    names(counts)[which.max(counts)]
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
  
  
  # Reactive function to prepare data for detailed variant maps
  prepare_data_variant_maps <- reactive({
    
    # Get the longer_google_df
    longer_google_df <- longer_google_df()
    
    # Determine the base polygon (canton or commune) based on the number of participants per item
    geo_type <- if (nrow(longer_google_df) <= 200) "Kanton" else "Gemeng"
    
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
  
  # reactive function to get the card for the info on a variable
  output$iwwersiicht_variabel<- renderUI({
      card(
      card_header(
        fa("map"),
        #bsicons::bs_icon("body-text"),
          HTML(paste0(tags$b("Phenomeen aus dem Beräich '", variables %>% filter(variable == variable()) %>% dplyr::select(map_category), "'")))
      ),
      card_body(
        HTML(paste0(tags$div(tags$b("Variabel: "), variable_name())),
        
        paste0(tags$div(tags$b("Offrokontext: "), variables %>%
                          filter(variable == variable()) %>% 
                          dplyr::select(item_text)), 
               
               tags$div(tags$b("Analyséiert Varianten: "),
                        paste(selection(), collapse = ", ")),

               tags$div(tags$b("Heefegst Variant: "), em(most_frequent_variant())),
               
               tags$div(tags$b("Schnëssen-Item: "), variables %>% 
                      filter(variable == variable()) %>% 
                      dplyr::select(item_number)),
                    
               tags$div(tags$b("Ausgewäert Realiséierungen: "),
                    nrow(longer_google_df()))
      )
    ) # HTML
      )
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
    
    if(file.exists(paste0("./overview_maps/Iwwerbleckskaart_", variable(), ".qs"))) {
      qread(paste0("./overview_maps/Iwwerbleckskaart_", variable(), ".qs"))
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
    
    # if(file.exists(paste0("./overview_maps/Iwwerbleckskaarten_Alter_", variable(), ".qs"))) {
    #   qread(paste0("./overview_maps/Iwwerbleckskaarten_Alter_", variable(), ".qs"))
    # }
    # else {
      print(paste(variable(),": Eenzel Alterskaaarten gi generéiert"))
      make_summary_plot_age(data = prepare_data_age()[["dataset"]], lsa_map_number = lsa_map_number, 
                            selection= selection(), color_num = length(selection()), map_title = variable(), item_number, 
                            item_text, geo_type = prepare_data()[["geo_type"]])
    # }
  })
  
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
      all_maps(data = prepare_data_variant_maps()$dataset)
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
  
  # reactive function for google data for mixed model
  data_regression <- reactive({
    max_var <- most_frequent_variant()
    title = paste("Heefegst Variant/Referenzvariant:", max_var)
    var <- variable()
    data <- google_df() %>%
      filter(!!sym(var) %in% selection()) %>%
      # add a new variable temp2 which is 1 for the most frequent variant of the reactive 'variable()', 0 otherwise
      mutate(temp_variable = ifelse(!!sym(var) == max_var, 1, 0))
    #str(data)

    return(list(data = data, title = title))
  })
  
  # plot function Sprooch & Educatiouns-Index
  output$plotLangEduIndex <- renderPlot({
    
    caption = paste("Dësen Index kombinéiert dat sproochlecht mat dem Ausbildungskapital:\n
                    Kompetenz am Däitschen (20 %), Kompetenz am Franséischen (40 %), Ausbildung (40 %).\n
                    Den Index rangéiert tëschent 0 an 1 (= maximaalt Kapital, i.e. béid Sproochkompetenzen maximal, Ausbildung: Universitéit/Fachhéichschoul)")
    
    plot_social_categories(data = google_df() %>%
                             # only Mammesproochler
                             filter(Mammesprooch == "Jo"),
                             `Sprooch & Educatioun-Index`, variable = variable(), selection = selection(), caption = caption)
    
  })
  
  # run a logistic regression
  lmodel <- reactive({
    glm(as.formula(paste("temp_variable ~", input$selectedVariable)), family = "binomial", data = data_regression()$data)
  })
  
  # Output the LM
  output$textLModel <- renderUI({
    HTML(sjPlot::tab_model(lmodel(), 
                           p.style = "scientific_stars",
                           title = data_regression()$title)$knitr)
  })
  
  output$plotLModel <- renderPlot({
    # Predict y-values
    predicted <- predict(lmodel(), type = "response")
    
    # Find x-value where y is closest to 0.5
    closest_point <- which.min(abs(predicted - 0.5))
    x_value <- data_regression()$data[[input$selectedVariable]][closest_point]
    
    p <- ggplot(data_regression()$data, aes_string(x = input$selectedVariable, y = "temp_variable")) +
      geom_jitter(height = 0.1, alpha = 0.5) +
      geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "red") +
      geom_point(aes(x = x_value, y = 0.5), color = "blue", size = 3) +  # Add point
      labs(x = input$selectedVariable, y = "Warscheinlechkeet, datt d'Variant gesot gëtt") +
      theme_bw() +
      ggtitle(data_regression()$title) +
      labs(caption = "Baséiert op enger logistischer Regressioun mat der Referenzvariant als Referenzkategorie") +
      annotate("text", x = 0.5, y = 0.5, label = paste("p =", round(summary(lmodel())$coefficients[2,4], 3))) +
      annotate("text", x = 0.5, y = 0.1, label = paste("n =", nrow(data_regression()$data))) +
      theme(legend.position = "none")
    
    p
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
  
  # plot function Sproochekompetenz
  output$plotSproochekompetenz <- renderPlot({
    
    caption = "Gemëttelt aus Kompetenz D + F."
    
    plot_social_categories(data = google_df(), `Sproochekompetenz`, variable = variable(), selection = selection(), caption = caption)
    
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
  
  # plot variable importance from random forest
  output$plotVariableImportance <- renderPlot({

    if(file.exists(paste0(variable(), "_VariableImportance.qs"))) {
      print("lokal VariableImportance")
      qread(paste0(variable(), "_VariableImportance.qs"))
    }
    else {
      print("VariableImportance gëtt generéiert")
      plot_VariableImportance(data = google_df(), variable = variable(), selection = selection())
   }
  })
  
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
                mutate(Variabel = paste0("<a href=", SERVER, "?_inputs_&sidebar=true&nav=%22Kaarten%22&variable=%22", variable, "%22>", Variabel, "</a>")) %>%
                # engelmann
                #mutate(Variabel = paste0("<a href=", SERVER_local, "?_inputs_&sidebarid=%22kaartekomplexer%22&variable=%22", variable, "%22>", Variabel, "</a>")) %>%
                dplyr::select(-variable),  
              escape = FALSE,
              #height = 600,
              extensions = 'Scroller',
              filter = 'top', options = list(
                deferRender = TRUE,
                scrollY = 900,
                scroller = TRUE,
                autoWidth = TRUE)
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")
