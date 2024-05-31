unilu_page <- function(title, ...) {

    library(htmltools)
    library(shiny)
    library(bslib)

    # unilu_page_deps <- function() {
    #     htmlDependency(
    #     name = "unilu_page",
    #     version = "0.0.1",
    #     src = c(href = "css/"),
    #     stylesheet = "unilu_page.css"
    #     )
    # }

    # bs5_deps <- function() {
    #     htmlDependency(
    #     name = "Bootstrap",
    #     version = "5.3.2",
    #     src = c(href = "https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/"),
    #     script = "js/bootstrap.bundle.min.js",
    #     stylesheet = "css/bootstrap.min.css"
    #     )
    # }


    head <- tags$head(
        tags$meta(charset = "utf-8"),
        tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
        tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon.png"),
        tags$title(title)
    )

    body <- tags$body(class = "bg-white",
        tags$div(class = "container-fluid p-0 text-center", style = "max-width: 94rem;", ...)
    )

    # tagList(bs5_deps(), unilu_page_deps(), head, body)
    tagList(head, body)


}


