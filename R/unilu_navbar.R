
unilu_navbar <- function(brand = NULL, brand_logo_url = NULL, ...) {

    # unilu_navbar_deps <- function() {
    #     htmlDependency(
    #     name = "unilu_navbar",
    #     version = "0.0.1",
    #     src = c(href = "css/"),
    #     stylesheet = "unilu_navbar.css"
    #     )
    # }

    
    # # Create the brand image tag if a logo URL is provided
    # brand_img_tag <- if (!is.null(brand_logo_url)) {
    #     tags$img(src = brand_logo_url, alt = brand, width = "30", height = "30", class = "d-inline-block align-text-center me-2")
    # }

    # Create the brand image tag if a logo URL is provided
    brand_img_tag <- if (!is.null(brand_logo_url)) {
        div(brand_logo_url, alt = brand, width = "30", height = "30", class = "d-inline-block align-text-center me-2")
    }
    
    # Create the brand link tag if a brand name is provided
    brand_link_tag <- if (!is.null(brand)) {
        tags$a(class = "uni-topnav-brand fw-normal ps-3", href = route_link("/"), style = "min-width: 200px;",
               if (!is.null(brand_img_tag)) tagList(brand_img_tag, brand) else brand)
    }
    
    navbar <- tags$nav(class = "navbar navbar-light bg-primary navbar-expand-lg p-0 mt-4 sticky-top",
        tags$div(class = "container-fluid",
            if (!is.null(brand)) brand_link_tag,  # Include the brand link tag conditionally
            #tags$span(class="badge rounded-pill fs-6 bg-warning me-5 shadow-sm", "v3103"),
            tags$button(class = "navbar-toggler", type = "button", 
                `data-bs-toggle` = "collapse", 
                `data-bs-target` = "#navbarNavAltMarkup", 
                `aria-controls` = "navbarNavAltMarkup", 
                `aria-expanded` = "false", 
                `aria-label` = "Toggle navigation",
                tags$span(class = "navbar-toggler-icon")
            ),
            tags$div(class = "collapse navbar-collapse", id = "navbarNavAltMarkup",
                tags$div(class = "uni-topnav-nav", ...)
            )
        )
    )

    #// tagList(unilu_navbar_deps(), navbar)
    tagList(navbar)
}