
server <- function(input, output, session) {

    observeEvent(input$selected_language, {
    update_lang(input$selected_language, session)
  })

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
    #geo_type <- input$selected_geo_type
    
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
  card_header_title <- reactive({
    variables %>% 
      filter(variable == variable()) %>% 
      dplyr::pull(map_category) %>% 
      first()
    })

  output$iwwersiicht_variabel <- renderUI({

    unilu_card(
      paste0(i18n$t("Phenomeen aus dem Beräich '"), i18n$t(card_header_title()), "'"),
      HTML(
        paste0(
        tags$ul(class="list-unstyled",
          tags$li(tags$span(i18n$t("Variabel: "), class="fw-bold"), variable_name()),
          tags$li(tags$span(i18n$t("Offrokontext: "), class="fw-bold"),variables %>% filter(variable == variable()) %>% dplyr::select(item_text)),
          tags$li(tags$span(i18n$t("Analyséiert Varianten: "), class="fw-bold"), paste(selection(), collapse = ", ")),
          tags$li(tags$span(i18n$t("Heefegst Variant: "), class="fw-bold"), em(most_frequent_variant())),
          tags$li(tags$span(i18n$t("Schnëssen-Item: "), class="fw-bold"), variables %>% filter(variable == variable()) %>% dplyr::select(item_number)),
          tags$li(tags$span(i18n$t("Ausgewäert Realiséierungen: "), class="fw-bold"), nrow(longer_google_df()))
        )
       )
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
      mutate(temp_variable = ifelse(!!sym(var) == max_var, 1, 0)) %>%
      # add a new variable kompetenz_däitsch, based on `Kompetenz am Däitschen` rescaled from 0 to 1 using scales::rescale()
      mutate(`Kompetenz am Däitschen` = scales::rescale(as.numeric(`Kompetenz am Däitschen`), to = c(0, 1))) %>%
      mutate(`Kompetenz am Franséischen` = scales::rescale(as.numeric(`Kompetenz am Franséischen`), to = c(0, 1))) %>%
      # relevel factor Dialektgebiet to "Zentrum" (reference level)
      mutate(Dialektgebiet = relevel(Dialektgebiet, ref = "Zentrum"))
    
    #str(data)

    return(list(data = data, title = title))
  })
  
  # plot function Sprooch & Educatiouns-Index
  output$plotLangEduIndex <- renderPlot({
    
    caption = i18n$t(paste("Dësen Index kombinéiert dat sproochlecht mat dem Ausbildungskapital:\n
                    Kompetenz am Däitschen (25%), Kompetenz am Franséischen (40%), Ausbildung (35%).\n
                    Den Index rangéiert tëschent 0 an 1 (= maximaalt Kapital, i.e. béid Sproochkompetenzen maximal, Ausbildung: Universitéit/Fachhéichschoul)"))
    
    plot_social_categories(data = google_df() %>%
                             # only Mammesproochler
                             filter(Mammesprooch == "Jo"),
                             `Sprooch & Educatioun-Index`, variable = variable(), selection = selection(), caption = caption)
    
  })
  
  # run a logistic regression
  lmodel <- reactive({
    #glm(as.formula(paste("temp_variable ~", input$selectedVariable)), family = "binomial", data = data_regression()$data)
    glm(as.formula(paste("temp_variable ~ `Kompetenz am Däitschen` + `Kompetenz am Franséischen` + Alter + Geschlecht")),
        family = "binomial",
        data = data_regression()$data)
    # für multinomial regression
    # nnet::multinom(as.formula(paste(variable(), " ~ langEduIndex_raw + Alter + Geschlecht + Dialektgebiet")), 
    #                data = data_regression()$data)
    # data <-  data_regression()$data
    # model <- glmulti::glmulti(temp_variable ~ `Kompetenz am Däitschen` + `Kompetenz am Franséischen` + Alter + Geschlecht + Dialektgebiet, 
    #                  data =  data, 
    #                  level = 1, 
    #                  method = "d", 
    #                  crit = "aicc",
    #                  family = "binomial",
    #                  fitfunction = glm,
    #                  confsetsize = 0.95, 
    #                  plotty = FALSE)
    # 
    # model@objects[[1]]
    # print(summary(model@objects[[1]]))
  })
  
  coeffs <- reactive({
    broom::tidy(lmodel()) %>%
      dplyr::filter(!grepl("Alter.Q|Alter.C|Alter\\^4|Alter\\^5|Ausbildung.Q|Ausbildung.C|Ausbildung\\^4|Ausbildung\\^5", term)) %>%
      dplyr::select(parameter = term, est = estimate, se = std.error, z = statistic, p = p.value) %>%
      # für multinomial regression
      #dplyr::select(parameter = term, est = estimate, se = std.error, z = statistic, p = p.value, Response = y.level) %>%
      # Create pretty.parameter and signif_labels columns
      mutate(parameter = fct_reorder(parameter, est)) %>%
      mutate(signif_labels = ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", ""))))
      
  })
  
  # Output the LM
  output$textLModel <- renderUI({
    HTML(sjPlot::tab_model(lmodel(),
                           rm.terms = c("Alter.C", "Alter.Q", "Alter^4", "Alter^5", "Ausbildung.C", "Ausbildung.Q", "Ausbildung^4", "Ausbildung^5"),
                           p.style = "scientific_stars",
                           title = data_regression()$title,
                           dv.labels = most_frequent_variant()
                           )$knitr)
  })
  
  output$plotLModel <- renderPlot({
    
    # see: https://keikcaw.github.io/visualizing-logistic-regression/Intro.html#Introduction:_Just_tell_me_%E2%80%9Cthe%E2%80%9D_effect

    # Define a named vector for colors
    color_mapping <- c("Positiv" = "#0571B0", 
                       "Net signifikant" = "gray", 
                       "Negativ" = "#CA0020")
    
    # Specify Roboto as the font family
    theme_set(theme_minimal() +
                theme(
                  axis.title.x = element_text(size = 14, family = "Lato"), # X-axis title font
                  axis.title.y = element_text(size = 14, family = "Lato"), # Y-axis title font
                  axis.text.x = element_text(size = 14, family = "Lato"),  # X-axis labels font
                  axis.text.y = element_text(size = 14, family = "Lato"),  # Y-axis labels font
                  plot.title = element_text(size = 14, family = "Lato")    # Plot title font
                ))
    
    coeffs() %>%
      filter(parameter != "(Intercept)") %>%
      mutate(
        pretty.parameter = fct_reorder(parameter, est),
        lower.95 = est + (qnorm(0.025) * se),
        lower.50 = est + (qnorm(0.25) * se),
        upper.50 = est + (qnorm(0.75) * se),
        upper.95 = est + (qnorm(0.975) * se),
        signif = case_when(p > 0.05 ~ i18n$t("Net signifikant"),
                           est > 0 ~ i18n$t("Positiv"),
                           est < 0 ~ i18n$t("Negativ")),
        signif = fct_relevel(signif, i18n$t("Positiv"),
                                            i18n$t("Net signifikant"),
                                                   i18n$t("Negativ"))) %>%
      ggplot(aes(x = pretty.parameter, color = signif)) +
      geom_linerange(aes(ymin = lower.95, ymax = upper.95), size = 1) +
      geom_linerange(aes(ymin = lower.50, ymax = upper.50), size = 2) +
      geom_point(aes(y = est), size = 4) +
      geom_hline(yintercept = 0) +
      scale_y_continuous(
        breaks = c(-1, 0, 1), #<<
        labels = c(i18n$t("← Manner"), #<<
                   i18n$t("Gläich"), #<<
                   i18n$t("Méi →")) #<<
      ) +
      scale_color_manual(
        "",
        values = color_mapping
      ) +
      geom_text(aes(x = pretty.parameter, y = upper.95, label = signif_labels), vjust = -0.5, size = 6) +
      labs(x = "", y = i18n$t("Warscheinlechkeet vum Gebrauch\nvun der Variant"),
           title = paste0(data_regression()$title, i18n$t("\n\nGeschate Relatioun tëscht soziale Facteuren an der\nWarscheinlechkeet fir de Gebrauch vun der Variant"))) +
      coord_flip(clip = "off")
    
  })
  
  # plot Ausbildung
  output$plotAusbildung <- renderPlot({
    
    caption = ""
    
    plot_social_categories(data = google_df(), Ausbildung, variable = variable(), selection = selection(), caption = caption)
    
  })
  
  # plot function Kompetenz D
  output$plotKompetenzD <- renderPlot({
    
    caption = i18n$t("Baséiert op Selbstaschätzung. 1 = bal guer keng Kompetenz, 7 = perfekt Kompetenz. \nFir d'Interpretatioun si just d'Stufe vu 5 bis 7 reliabel.")
    
    plot_social_categories(data = google_df(), `Kompetenz am Däitschen`, variable = variable(), selection = selection(), caption = caption)
    
  })
  
  # plot function Kompetenz F
  output$plotKompetenzF <- renderPlot({
    
    caption = i18n$t("Baséiert op Selbstaschätzung. 1 = bal guer keng Kompetenz, 7 = perfekt Kompetenz. \nFir d'Interpretatioun si just d'Stufe vun 3 bis 7 reliabel.")
    
    plot_social_categories(data = google_df(), `Kompetenz am Franséischen`, variable = variable(), selection = selection(), caption = caption)
    
  })
  
  # plot function Sproochekompetenz
  output$plotSproochekompetenz <- renderPlot({
    
    caption = i18n$t("Gemëttelt aus Kompetenz D + F.")
    
    plot_social_categories(data = google_df(), `Sproochekompetenz`, variable = variable(), selection = selection(), caption = caption)
    
  })
  
  # plot function Mammesprooch
  output$plotMammesprooch <- renderPlot({
    
    caption = ""
    
    plot_social_categories(data = google_df(), `Mammesprooch`, variable = variable(), selection = selection(), caption = caption)
    
  })
  # plot function Aflëss
  output$plotAfloss <- renderPlot({
    
    caption = i18n$t("Baséiert op Selbstaschätzung.")
    
    plot_social_categories(data = google_df(), `Sproochlech Aflëss`, variable = variable(), selection = selection(), caption = caption)
    
  })
  
  # plot function Urbanisatioun
  output$plotUrbanisatioun <- renderPlot({
    
    caption = i18n$t("Baséiert op dem 'Degree of Urbanisation' (DEGRUBA).\n1 = staark urbaniséiert (= d'Stad Lëtzebuerg), 3 = wéineg urbaniséiert\nSource: STATEC (https://statistiques.public.lu/en/themes/population-emploi.html)")
    
    plot_social_categories(data = google_df(), Urbanisatioun, variable = variable(), selection = selection(), caption = caption)
    
  })
  
  # plot function Awunner_km2
  output$plotAwunner <- renderPlot({
    
    caption = ""
    
    plot_social_categories(data = google_df(), Awunner_km2, variable = variable(), selection = selection(), caption = caption)
    
  })
  # plot function Indice socio-économique
  output$plotIndex <- renderPlot({
    
    caption = i18n$t("Baséiert op dem 'Indice socio-économique' pro Gemeng.\n\nSource: STATEC (https://statistiques.public.lu/dam-assets/catalogue-publications/bulletin-Statec/2017/bulletin-2-17.pdf)")
    
    plot_social_categories(data = google_df(), `Indice socio-économique`, variable = variable(), selection = selection(), caption = caption)
    
  })
  
  # plot function Immigratioun
  output$plotImmigratioun <- renderPlot({
    
    caption = i18n$t("Prozentzuel vun Net-Lëtzebuerger pro Gemeng\nSource: STATEC: https://data.public.lu/en/datasets/population-de-residence-habituelle-par-commune-et-nationalite-au-1er-fevrier-2011/")
    
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
      print(i18n$t("VariableImportance gëtt generéiert"))
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
  





 # stop App ----------------------------------------  

    # session$onSessionEnded(function() {
    #   stopApp()
    # })

 # shiny.route ----------------------------------------  
  router_server()
  
}

