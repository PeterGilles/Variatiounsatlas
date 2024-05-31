

unilu_tabset <- function(...) {

    bs5_deps <- function() {
        htmlDependency(
        name = "Bootstrap",
        version = "5.3.2",
        src = c(href = "https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/"),
        script = "js/bootstrap.bundle.min.js",
        stylesheet = "css/bootstrap.min.css"
        )
    }

    tabs <- list(...)
  
    items <- lapply(tabs, function(tab) tab$item)
    bodies <- lapply(tabs, function(tab) tab$body)
  
    navigation <- tags$div(class = "card",
        tags$div(class = "card-header",
            tags$ul(class = "nav nav-tabs card-header-tabs", role = "tablist", items)
        ),
        tags$div(bodies)
    )
  
    tagList(
        bs5_deps(),
        navigation
    )

}