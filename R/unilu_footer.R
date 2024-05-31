

unilu_footer  <- function(text) {
    
    footer <-  div(class = "d-flex flex-row justify-content-center align-items-center border-1 border-top border-primary bg-secondary px-3 mt-5", style = "height: 80px;",
        div(class = "text-dark", text)
    )

    tagList(footer)

}
