
unilu_tab <- function(title, body) {
    item <- tags$li(
        class = "nav-item",
        tags$a(class = "nav-link", "aria-current" = "true", href = "#", title)
    )

    body <- tags$div(
        tags$div(class = "card-body", tags$p(class = "card-text", body))
    )

  list(item = item, body = body)
}

