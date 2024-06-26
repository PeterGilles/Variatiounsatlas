library(dplyr)
library(ggplot2)
library(purrr)
library(magrittr)
library(tidyr)
library(stringr)
library(forcats)
library(DT)
library(waiter)
library(qs)
library(bslib) # is also loaded in ui.R ?!
library(shinycssloaders)
library(markdown)
library(ggiraph)
library(mapproj)
library(patchwork) # smaller objects
library(shiny.i18n)

# activate these for the creation of maps
#library(gfonts)
#library(raster)
#options("sp_evolution_status"=2)
#library(sp)
#library(sf)
#library(cowplot) # creates too large objects! used for ggdraw
#library(jpeg)
#library(magick) # for LSA maps
#library(scales)

# on Ubuntu/engelmann, install packages with:
# sudo su - -c "R -e \"install.packages('party', repos='http://cran.rstudio.com/')\""

enableBookmarking(store = "url")

 
## Some settings
# 1. load paths externally
paths <- rio::import("paths/paths.csv")
SERVER_local <- filter(paths, name == "SERVER_local") %>%
  dplyr::select(path) %>%
  as.character()

SERVER <- filter(paths, name == "SERVER") %>%
  dplyr::select(path) %>%
  as.character()

## Font stuff
#get_all_fonts()

# setup_font(
#   id = "open-sans",
#   output_dir = "fonts",
#   variants = c("regular", "italic", "700", "700italic"),
#   prefer_local_source = FALSE)
#
# use_font("open-sans", "fonts/css/open-sans.css", selector = ".dummy-selector")

# setup_font(
#   id = "roboto",
#   output_dir = "fonts",
#   variants = c("regular", "italic", "700", "700italic"),
#   prefer_local_source = FALSE)

# setup_font(
#   id = "domine",
#   output_dir = "./Variatiounsatlas/fonts",
#   variants = c("regular", "italic", "700", "700italic"),
#   prefer_local_source = FALSE)

#use_font("domine", "fonts/css/domine.css")
#use_font("roboto", "fonts/css/roboto.css")
#validated_fonts(list(sans = "roboto", serif = "roboto"))

preloader <- list(html = tagList(spin_1(), "De Variatiounsatlas gëtt gelueden ..."), color = "#343a40")

## Prepare stuff
# Polygoner fir Kanonten a Gemengen

# Create new polygones for Gemengen
#library(rgdal)     # R wrapper around GDAL/OGR
#library(ggplot2)   # for general plotting
#library(ggmaps)    # for fortifying shapefiles

# First read in the shapefile, using the path to the shapefile and the shapefile name minus the
# extension as arguments
#shapefile <- readOGR("/Users/peter.gilles/Downloads/gadm36_LUX_shp/gadm36_LUX_3.shp")
# Then modify polygons in QGIS, save
# Next the shapefile has to be converted to a dataframe for use in ggplot2
#shapefile_df <- fortify(shapefile, region = "NAME_3")
# Save as RDS
#saveRDS(shapefile_df, "communes_df.RDS")

# load prepared polygon data for cantons and communes
cantons_df <- readRDS("seed/cantons_df.RDS")
communes_df <- readRDS("seed/communes_df.RDS")
Gemengen_Statistiken <- qread("seed/Gemengen_Statistiken.qs")

# Weider Elementer fir d'Kaartéierung
# rivers <- readRDS("river.RDS")
# rivers <- rivers[["osm_lines"]] %>%
#   filter(name %in% c("Alzette", "Sûre", "Sauer", "Sauer - Sûre", "Mosel", "Moselle"))

#bbox_lux_2500.sf <-readRDS("bbox_lux_2500.sf.rds")
#elevation_raster <- readRDS("elevation_raster.rds")

# OSM base map
#osm_map <- qread("osm_map.qs")

color_palette <- c('#56cc9d','#beaed4','#fdc086','#ffff99','#386cb0','#f0027f','#bf5b17','#666666')

# Internationalisation
i18n <- Translator$new(translation_json_path='translations/translation.json')
i18n$set_translation_language('lb')

# update Kaartesettings from Google Docs  
# library(googlesheets4)
# gs4_deauth()
# variables <- range_read(ss = "1IDtvxHgccWg2JMu-dqJyqsCpwsurpYLOgm6rfE0mMGU", sheet = "kaartesettings", col_names=T, col_types = "c") %>%
#  filter(active == "yes") %>%
#  arrange(map_category, input_choice) %>%
#   write_csv(file = "./Variatiounsatlas/kaartesettings.csv")

# load Kaartesettings from local csv
variables <- read.csv("seed/kaartesettings.csv") %>%
  filter(active == "yes") %>%
  arrange(map_category, input_choice, .locale = "de")

# Functions
make_choices <- function(category) {
  choices <- variables %>%
    #dplyr::filter(map_category == {{category}}) %>%
    dplyr::select(variable, input_choice) %>%
    arrange(tolower(input_choice))
  choices <- set_names(choices$variable, choices$input_choice)
  #choices
  # selectInput(inputId = category, label = h3(category),
  #             choices = choices,
  #             selectize = FALSE,
  #             size = 25)
}

# make_choices <- function(category) {
#   choices <- variables %>%
#     dplyr::filter(map_category == {{category}}) %>%
#     dplyr::select(variable, input_choice) %>%
#     arrange(input_choice)
#   choices <- set_names(choices$variable, choices$input_choice)
#   #choices
#   # selectInput(inputId = category, label = h3(category),
#   #             choices = choices,
#   #             selectize = FALSE,
#   #             size = 25)
# }

# Function frequencies of variants
plot_freq_variants <- function(data, variable, selection, caption = "") {
  
  color <- color_palette[1:length(selection)]
  
  bar_df <- data %>%
    dplyr::select(variable = {{variable}}) %>%         # Filter the desired variable
    filter(variable %in% selection) %>%                # Only include variants from the selection
    drop_na() %>%
    mutate(Varianten = "Varianten",                    # Dummy variable for stacking
           variable = factor(variable, levels = selection)) 
  
  # Simple bar chart
  bar <- ggplot(data = bar_df %>% 
                  count(Varianten, variable) %>%
                  mutate(total = sum(n), 
                         label = paste0(round(n/total, 2)*100, "%\n", n)),
                aes(x = Varianten, y = n, fill = variable, label = label)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = color, breaks = selection) +
    geom_text(position = position_stack(vjust = .5), size = 5, alpha = 0.9, color = "antiquewhite") +
    coord_flip() +
    theme(axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 13),
          legend.box = "horizontal",
          text = element_text(size = 13),
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank())
  
  return(bar)
}

#############################
# Function Iwwerbléckskaart #

make_summary_plot <- function(dataset, lsa_map_number, selection, color_num, map_title, item_number ="", item_text = "", geo_type) {
  
  print(geo_type)
  color <- color_palette[1:color_num]
  
  # get count of all observations
  variant_count <- dataset %>% distinct(id, total)
  variant_count <- sum(variant_count$total)
  
  dataset <- dataset %>%
    group_by(id) %>%
    mutate(prozent = n/total) %>%
    filter(n == max(n)) %>%
    mutate(max_variant = variants) 
  
  p <-  ggplot() +
  # variant data
  geom_polygon_interactive(data = dataset, aes(x = long, y = lat, fill=max_variant, group = id,
                                               tooltip = paste0(i18n$t("Lokalitéit: "), id, 
                                                                       i18n$t("\nHeefegst Variant: "), max_variant,
                                                                       i18n$t("\nRelativ Heefegkeet: "), round(freq, 2)*100,
                                                                              i18n$t(" %\nParticipanten: "), n)),
                           # alpha per polygon steered by percentage, if not useful, set alpha back to 0.8
                           linewidth=0, alpha = dataset$prozent) +
    # add Rivers
    #geom_sf(data = rivers, color = "#46b4e7") +
    # borders of cantons
    geom_polygon(data = cantons_df, aes(x = long, y = lat, group = id),
                 linewidth= .1, colour = "#a9a9a9", fill = NA) +
    coord_map() +
    # coord_sf(xlim = c(5.715637, 6.54680),
    #          ylim = c(49.393100, 50.192726),
    #          expand = FALSE) +
    scale_colour_identity() +
    scale_fill_manual(values = color[1:color_num],
                      breaks = selection) +
    labs(title = paste(i18n$t("Variabel: "), word(map_title, 2, sep="_")),
         fill = paste0(i18n$t("Haaptvariant\npro "), word(geo_type, 1, sep = "_")),
         x = "", y = "",
         caption = paste0(variant_count, i18n$t(" Participanten | Klengst Polygoner: "), word(geo_type, 1, sep = "_"), i18n$t("\n© Uni Lëtzebuerg | generéiert "), date())
    ) +
    #theme_void(base_family = "sans") +
    theme_void() +
    theme(plot.title = element_text(size=38, hjust = 0.5, face="bold"),
          plot.caption = element_text(size=26),
          legend.position =  c(0.8, 0.9),
          legend.text = element_text(size=34),
          legend.title = element_text(size=35, face = "bold"))
  
  # plot with or without LSA
  # with LSA
  if(lsa_map_number != "NO") {
    # prepare LSA map into grid
    # für Docker
    #lsa <- readJPEG(source =  paste0("atlas/Kaarten-LSA_small/", lsa_map_number, "_lux.jpg"))
    lsa <- readJPEG(source = paste0("./Kaarten-LSA_small/", lsa_map_number, "_lux.jpg"))
    lsa <- ggdraw() + draw_image(lsa) +
      labs(caption =paste0(i18n$t("Vergläichskaart aus dem 'Luxemburgischer Sprachatlas', LSA (1963), Kaart "), lsa_map_number)) +
      theme_void() +
      theme(plot.caption = element_text(size=26))
    
    plot_row <- wrap_plots(lsa, p, nrow = 1, ncol = 2)
    
    plot_row <- girafe(code = print(plot_row),
                       width_svg = 27, height_svg = 18,
                       #fonts = list(sans = "roboto"),
                       options = list(
                         opts_hover(css = "fill:#FF3333;stroke:black;cursor:pointer;", reactive = TRUE),
                         opts_selection(
                           type = "multiple", css = "fill:#FF3333;stroke:black;")))
  } else {
  #without LSA map
    plot_row <- girafe(code = print(p),
                       width_svg = 20, height_svg = 24,
                       #fonts = list(sans = "Sans"),
                       options = list(
                         opts_hover(css = "fill:#FF3333;stroke:black;cursor:pointer;", reactive = TRUE),
                         opts_selection(
                           type = "multiple", css = "fill:#FF3333;stroke:black;")))
  }
  
  # to save map as pdf and png
  #ggsave(plot = plot_row, filename = paste0(map_title, ".png"), units = "cm", width = 22)
  #ggsave(plot = p, filename = paste0(map_title, ".pdf"), units = "cm", width = 22)
  #ggsave(plot = plot_row, filename = paste0(map_title, "_mat_LSA.pdf"), device="pdf", dpi=400, units = "cm", width = 28)
  
  qsave(plot_row, file = paste0("./overview_maps/Iwwerbleckskaart_", map_title, ".qs"))
  return(plot_row)
}

########################################
# Function Iwwerbléckskaarte per Alter #

make_summary_plot_age <- function(dataset, lsa_map_number, selection, color_num, map_title, item_number ="", item_text = "", geo_type) {
  
  color <- color_palette[1:color_num]
  # TODO remove '_' from selection
  #selection <- str_replace(selection, "_", "")
  
  # get count of all observations
  variant_count1 <- dataset %>% distinct(id, Alter, total)
  variant_count <- sum(variant_count1$total)
  
  freq_alter <- variant_count1 %>%
    group_by(Alter) %>%
    summarise(freq_alter = sum(total))

  # define a function to create the labels
  my_labeller <- function(value) {
    freq_alter2 <- unique(freq_alter$freq_alter[freq_alter$Alter == value])
    label <- paste0(value, "\n(N = ", freq_alter2, ")")
    return(label)
  }
  
  dataset <- dataset %>%
    group_by(id, Alter) %>%
    mutate(prozent = n/total) %>%
    filter(n == max(n)) %>%
    mutate(max_variant = variants) 
  
  # create maps
  p <-  ggplot() +
    
    # variant data
    geom_polygon(data = dataset, aes(x = long, y = lat, fill=max_variant, group = id),
                 # alpha per polygon steered by percentage, if not useful, set alpha back to 0.8
                 linewidth=0, alpha = dataset$prozent) +
    # borders of cantons
    geom_polygon(data = cantons_df, aes(x = long, y = lat, group = id),
                 linewidth= .1, colour = "#a9a9a9", fill = NA) +
    coord_map() +
    scale_colour_identity() +
    scale_fill_manual(values = color[1:color_num],
                      breaks = selection) +
    labs(title = paste("Variabel '", word(map_title, 2, sep="_"), "' no Alter"),
         fill = paste0("Haaptvariant\npro ", word(geo_type, 1, sep = "_")),
         x = "", y = "",
         caption = paste0(variant_count, " Participanten | Klengst Polygoner: Kanton\n© Uni Lëtzebuerg | generéiert ", date())
    ) +
    theme_void(base_family = "Roboto") +
    # potential error in connection with randomForest for 'margin', hence ggplot2::margin
    # see: https://github.com/tidyverse/ggplot2/issues/2150
    theme(plot.title = element_text(size=18, hjust = 0.5, face="bold", margin = ggplot2::margin(0, 0, 20, 0)),
          plot.caption = element_text(size=12),
          legend.position =  "bottom",
          legend.text = element_text(size=12),
          legend.title = element_text(size=12, face = "bold"),
          strip.text = element_text(size = 13, face = "bold"), # set strip.text to bold and size 12
          ) +
  
    facet_wrap(. ~ Alter, labeller = as_labeller(my_labeller), scales = "fixed"
    )
  
  # Save maps
  #qsave(p, file = paste0("./overview_maps/Iwwerbleckskaarten_Alter_", map_title, ".qs"))
  
  # return maps
  return(p)
}

### Function Variantekaarten #
make_plot <- function(dataset, variable, color) {
  
  # Get count of all observations
  variant_count <- dataset %>% 
    distinct(id, n) %>% 
    summarize(sum(n))
  
  # Create ggplot
  plot <- ggplot() +
    geom_polygon(data = dataset, aes(x = long, y = lat, fill = freq, 
                                     group = id), linewidth = 0, alpha = 1, colour = "lightgrey") +
    geom_polygon(data = cantons_df, aes(x = long, y = lat, group = id),
                 linewidth = .1, colour = "#a9a9a9", fill = NA) +
    coord_map() +
    scale_fill_gradient(guide = guide_legend(), low = "white", high = color,
                        name = paste(variant_count, "Participanten"), na.value = "white", limits = 0:1, labels = scales::percent(0.25 * 0:4)) +
    labs(title = stringr::str_replace({{variable}}, "_", " "), x = "", y = "") +
    theme_void() +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          legend.position = "bottom",
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 12),
          plot.margin = unit(c(0.2, 0, 0.2, 0), "cm")) +
    guides(fill = guide_colourbar(barwidth = 7, barheight = .7, ticks = FALSE, title.position = "bottom", title.hjust = 0.5))
  
  return(plot)
}

# Function to plot Sozialdaten #

plot_social_categories <- function(data, social_category, variable, selection, caption = "") {
  
  color <- color_palette[1:length(selection)]
  
  bar_df <- data %>%
    # Einschränkung auf gewünschte Kategorie und Variable
    dplyr::select({{social_category}}, variable = {{variable}}) %>%
    # Nur Varianten aus der Selection
    filter(variable %in% selection) %>%
    drop_na() %>%
    # Prozente berechnen
    group_by({{social_category}}, variable) %>%
    count() %>%
    group_by({{social_category}}) %>%
    mutate(total = sum(n)) %>%
    mutate(Prozent = n/total) %>%
    dplyr::rename(count_variant = n) %>%
    group_by({{social_category}}) %>%
    mutate(total_alter = sum(count_variant)) %>%
    mutate(Prozent = count_variant/total_alter) %>%
    ungroup() %>%
    mutate(total_variable = sum(count_variant)) %>%
    mutate(Prozent_variable = count_variant/total_variable)  %>%
    dplyr::select(-total_alter, -total_variable) %>%
    group_by({{social_category}}) %>%
    mutate(Prozent_social_category = sum(Prozent_variable))
  
  #bar_df$w <- cumsum(bar_df$total)
  #bar_df$wm <- bar_df$w - bar_df$total
  #bar_df$wt <- with(bar_df, wm + (w - wm)/2)
  
  # # Groups:   Alter [6]
  #    Alter     variable count_variant total Prozent
  #    <chr>     <chr>            <int> <int>   <dbl>
  #  1 ≤ 24      lo[g]ie             40   224  0.179 
  #  2 ≤ 24      lo[ʒ]ie            184   224  0.821 
  #  3 25 bis 34 lo[g]ie             45   305  0.148 
  #  4 25 bis 34 lo[ʒ]ie            260   305  0.852 
  #  5 35 bis 44 lo[g]ie             11   179  0.0615
  #  6 35 bis 44 lo[ʒ]ie            168   179  0.939 
  #  7 45 bis 54 lo[g]ie             20   183  0.109 
  #  8 45 bis 54 lo[ʒ]ie            163   183  0.891 
  #  9 55 bis 64 lo[g]ie              8   123  0.0650
  # 10 55 bis 64 lo[ʒ]ie            115   123  0.935 
  # 11 65+       lo[g]ie              2    40  0.05  
  # 12 65+       lo[ʒ]ie             38    40  0.95  
  # # A tibble: 4 x 5
  # # Groups:   Geschlecht [2]
  #   Geschlecht variable count_variant total Prozent
  #   <chr>      <chr>            <int> <int>   <dbl>
  # 1 Männlech   lo[g]ie             31   328  0.0945
  # 2 Männlech   lo[ʒ]ie            297   328  0.905 
  # 3 Weiblech   lo[g]ie             95   726  0.131 
  # 4 Weiblech   lo[ʒ]ie            631   726  0.869 
  
  xlabel <- as.character(ensym(social_category))
  #print(xlabel)
  
  # Stacked barplot with multiple groups
  bar <- ggplot(data=bar_df, aes(x= {{social_category}}, y = Prozent, fill = variable, label = count_variant), alpha = 0.9) +
    geom_col(position = position_stack()) +
    # with proportional bar widths
    #  geom_col(position = position_stack(), aes(width = Prozent_social_category * 2)) +
    #geom_rect(position = position_stack(), aes(xmin = wm, xmax = w,
    #   ymax = Prozent, fill = variable)) +
    geom_text(position = position_stack(vjust = .5), size = 5, alpha = 0.9, color = "antiquewhite") +
    labs(fill = word(variable, 2, sep="_"),
         caption = caption) +
    # to displace labels, when too crowded
    #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_manual(values = color[1:length(selection)], breaks = selection) +
    theme_minimal() +
    labs(x = i18n$t(xlabel)) +
    theme(legend.text = element_text(size=14),
          legend.position="bottom",
          legend.title = element_blank(),
          plot.caption = element_text(size = 11),
          axis.title = element_text(size=15),
          axis.text = element_text(size=14))
  
  return(bar)
}

# Function for decision tree
plot_decision_tree <- function(data, variable, selection) {

  cond_df <- data %>%
    dplyr::filter(Mammesprooch != "Neen") %>%
    dplyr::select(variable = {{variable}}, Alter, Geschlecht, Dialektgebiet, Däitsch = `Kompetenz am Däitschen`, Franséisch = `Kompetenz am Franséischen`, Ausbildung) %>%
    dplyr::filter((variable) %in% selection) %>%
    na.omit() %>%
    # filter variants above a certain frequency level
    group_by(variable) %>%
    dplyr::filter(n()/nrow(data) >= 0.04) %>%
    ungroup()
  
  cond_df[sapply(cond_df, is.character)] <- lapply(cond_df[sapply(cond_df, is.character)], as.factor)
  
  # age as ordered factor
  ageorder <- c("≤ 24", "25 bis 34", "35 bis 44", "45 bis 54", "55 bis 64", "65+")
  cond_df$Alter <- factor(cond_df$Alter,
                            ordered = is.ordered(ageorder))
  
  set.seed(1234)
  
  cond_tree <- party::ctree(formula = variable ~ Alter + Geschlecht + Dialektgebiet + Däitsch + Franséisch + Ausbildung,
                     data = cond_df,
                     control = party::ctree_control(testtype = "Univariate", minbucket = 20))
  
  # plot tree
  plot(cond_tree)
  
}

# Function for RF variable importance
plot_VariableImportance <- function(data, variable, selection) {

  forest_df <-  data %>%
    filter(Mammesprooch != "Neen") %>%
    dplyr::select(variable = {{variable}}, Alter, Geschlecht, Dialektgebiet, Däitsch = `Kompetenz am Däitschen`, Franséisch = `Kompetenz am Franséischen`, Ausbildung) %>%
    dplyr::filter((variable) %in% selection) %>%
    na.omit() %>%
    # filter variants above a certain frequency level
    group_by(variable) %>%
    filter(n()/nrow(data) >= 0.04) %>%
    ungroup() 
        
  forest_df[sapply(forest_df, is.character)] <- lapply(forest_df[sapply(forest_df, is.character)], as.factor)

  # age as ordered factor
  ageorder <- c("≤ 24", "25 bis 34", "35 bis 44", "45 bis 54", "55 bis 64", "65+")
  forest_df$Alter <- factor(forest_df$Alter,
                            ordered = is.ordered(ageorder))
  
  # # mutate Index in forest_df as ordered factor
  # indexorder <- c("<= 0.4", "0.6", "0.8", "1")
  # forest_df$Index <- factor(forest_df$Index,
  #                           ordered = is.ordered(indexorder))
  
  # Random forest model
  rf <- randomForest::randomForest(variable ~ Alter + Geschlecht + Dialektgebiet + Däitsch + Franséisch + Ausbildung, 
                     data=forest_df, importance=TRUE, ntree=2000, keep.forest=FALSE)
  
  # plot Variable Importance
  randomForest::varImpPlot(rf, main = "Variable Importance", type = 1) # type = 1: mean decrease in accuracy
  
  # save
  #qsave(plot, file = paste0(variable, "_VariableImportance.qs"))
  
  return(plot)
  
}

# Function for data table
plot_datatable <- function(data, variable) {
  datatable(data %>%
              dplyr::select(Gemeng, Variant = as.name({{variable}}), recordingURL, Kanton, Dialektgebitt = Dialektgebiet) %>%
              dplyr::filter(Variant != "FALSE") %>%
              mutate(Lauschtert = paste0("<audio controls preload=\"none\" type=\"audio/wav\" src=\"", recordingURL, "\"> </audio>")) %>%
              dplyr::select(-recordingURL) %>%
              dplyr::select(Variant, Lauschtert, Dialektgebitt, Gemeng, Kanton),
            escape = FALSE,
            #height = 600,
            extensions = 'Scroller',
            filter = 'top', options = list(
              deferRender = TRUE,
              scrollY = 600,
              scroller = TRUE,
              autoWidth = TRUE
            ))
}
