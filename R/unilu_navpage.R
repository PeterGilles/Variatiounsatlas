unilu_navpage <- function(page_name, href = "#", is_active = FALSE, is_disabled = FALSE) {
  
    # internationalization
    page_name <- i18n$t(page_name)
    
    classes <- "uni-topnav-link border-start px-4 py-0 fw-normal fs-5"
    if (is_active) {
        classes <- paste(classes, "active")
    }
    if (is_disabled) {
        classes <- paste(classes, "disabled")
    }
    
    tags$a(class = classes, href = href, page_name)
}
