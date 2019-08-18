#' Run Shiny app.
#'
#' @return Web App
#'
#'
#' @export
run_mlr <- function(){
    app_path <- system.file("webapp/MLPlanneR", package = "automlr")
    return(shiny::runApp(app_path, launch.browser = TRUE))
}
