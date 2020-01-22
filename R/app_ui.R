#' @import shiny
app_ui <- function() {
  
  ui <- tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    fluidPage(
      title = "Phoning",
      theme = shinythemes::shinytheme("cerulean"),
      absolutePanel(
        width = "96%",
        fluidRow(
          column(
            width = 7, style='padding-left: 30px; padding-right: 50px;',
            mod_groups_panel_ui("groups_panel_ui"),
            fluidRow(div(style = "height: 20px;", "")),
            mod_participants_panel_ui("participants_panel_ui")
          ),
          column(
            width = 5,
            mod_contacts_panel_ui("contacts_panel_ui")
          )
        )
      )
    )
  )
  
  ui <- shinymanager::secure_app(ui)
  
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'survey.phoning')
  )
 
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    # Add here all the external resources
    shinyalert::useShinyalert(),
    rclipboard::rclipboardSetup(),
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
