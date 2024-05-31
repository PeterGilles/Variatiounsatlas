unilu_theme <- bs_theme(

        primary = "#319fd4",
        secondary = "#f6f5ea",
        danger = "#bf4d4d", # was: #da2827
        "enable-rounded" = FALSE,
    )

unilu_theme <- bs_add_variables(

    unilu_theme, 
    "navbar-light-brand-hover-color" = "$white", 
    "navbar-light-active-color" = "$danger",
    "font-size-sm" = "0.65 * $spacer",
    "box-shadow-sm" = "0px 1px 3px 0 rgba(0, 0, 0, .1)",
    .where = "declarations"

    )

unilu_theme <- bs_add_rules(

    unilu_theme, 
    sass::sass_file("www/css/unilu_global.scss")
    
    )
        