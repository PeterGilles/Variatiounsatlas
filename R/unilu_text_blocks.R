unilu_img_map <- function(src, caption) {
    imageTag <- tags$div(class = "col-xl-3", tags$image(class = "img-fluid", src = src), p(class = "caption p-2 mt-2 border-top", caption))
    tagList(imageTag)
}

unilu_title_small <- function(title) {
    title <- i18n$t(title)
    title <- p(id = "uni_title_small", class = "h5 py-3 px-0 text-start", title)
    tagList(title)
}

unilu_title_large <- function(title) {
    title <- i18n$t(title)
    title <- p(id = "uni_title_large", class = "h1 lh-1 text-start", title)
    tagList(title)
}

unilu_text_md <- function(md_file) {
    body <- p(class = "pt-0", includeMarkdown(md_file))
    tagList(body)
}

unilu_text <- function(text) {
    text <- i18n$t(text)
    body <- p(class = "pt-0", text)
    tagList(body)
}

unilu_empty_block <- function(height) {
    block <- div(class = "bg-white", style= paste0("height:", height, ";"))
    tagList(block)
}

unilu_button <- function(label, link) {
    label <- i18n$t(label)
    button <- tags$a(href = link, class = "btn btn-outline-danger rounded-0 border-3 py-3 px-4 my-5", role="button", label)
    tagList(button)
}
