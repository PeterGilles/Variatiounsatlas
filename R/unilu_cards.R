
unilu_card_feature <- function(title, body) {
    # for internationalization
    title <- i18n$t(title)
    #body <- i18n$t(body)
    
    div(class = "col-xl-4", 
        div(class = "uni-card-feature", 
            div(class = "uni-card-feature-header py-3", title),
            div(class = "uni-card-feature-body", 
                p(class = "uni-card-feature-text", body)
            )
        )
    
    )
}



unilu_card <- function(title, body) {
        
        div(class = "uni-card mb-3", 
            div(class = "uni-card-header py-3", title),
            div(class = "uni-card-body", body
                # p(class = "uni-card-text", body)
            )
        )
    
}

