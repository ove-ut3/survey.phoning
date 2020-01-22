# Module UI
  
#' @title   mod_participants_panel_ui and mod_participants_panel_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_participants_panel
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_participants_panel_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      div(
        style = "font-size:90%",
        DT::dataTableOutput(ns("dt_participants"))
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_participants_panel
#' @export
#' @keywords internal
    
mod_participants_panel_server <- function(input, output, session, rv){
  ns <- session$ns
  
  rv$df_participants_selected <- reactive({
    
    req(rv$input_group[["dt_groups_rows_selected"]])
    
    data <- rv$df_participants_user() %>% 
      df_participants_events() %>% 
      dplyr::filter(!completed, !optout)
    
    if (!is.null(rv$attributes_groups)) {
      data <- data %>% 
        dplyr::semi_join(
          dplyr::filter(rv$df_groups, dplyr::row_number() == rv$input_group[["dt_groups_rows_selected"]]),
          by = rv$attributes_groups
        )
    }
    
    data
    
  })
  
  output$dt_participants <- DT::renderDT({

    if (length(rv$attributes_participants) == 0) rv$attributes_participants <- NULL
    
    rv$df_participants_selected() %>% 
      dplyr::select_at(c("Prénom" = "firstname", "Nom" = "lastname", rv$attributes_participants, "Avancement" = "lastpage_rate", "Suivis" = "n_events", "Date" = "last_event_date")) %>% 
      DT::datatable(
        selection = list(mode = 'single', selected = 1),
        rownames = FALSE,
        options = list(
          ordering = FALSE,
          scrollY = '42vh',
          dom = 'Brtip',
          language = list(paginate = list(previous = "Précédent", `next` = 'Suivant'))
        )
      ) %>%
      DT::formatPercentage("Avancement", digits = 1) %>%
      DT::formatDate("Date", method = "toLocaleDateString", params = list("fr-FR"))
    
  })
  
  rv$dt_participants_proxy <- DT::dataTableProxy("dt_participants")
  
  rv$input_participant <- input
  
}
