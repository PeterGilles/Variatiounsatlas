
unilu_header <- function(img = img) {

    header <- 
        div(class = "d-flex bg-white align-items-center position-relative mx-auto", style = "height: 112px; box-shadow: 0px 1px 3px 0 rgba(0,0,0,.1); z-index: 1000;",
            div(class = "ps-3 ms-3 pe-3 me-3", img(src="img/unilu_logo.svg", class = "img", style = "height: 78px;")),
            div(class = "border-start border-dark ps-4", img(src = img, class = "img ms-2", style = "height: 60px;"))
        )

    tagList(header)

}