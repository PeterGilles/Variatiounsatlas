unilu_grid <- function(num_cols) {

    if (num_cols < 1 || num_cols > 12) {
        stop("Number of columns must be between 1 and 12.")
    }

    cols <- vector("list", num_cols)

    for (i in 1:num_cols) {
        cols[[i]] <- tags$div(class = "col bg-primary bg-opacity-25 border border-primary text-white fs-6 fw-light", "column")
    }

    # Create the grid with the specified number of columns
    grid <- tags$div(class = "row", cols)

    tagList(grid)
}