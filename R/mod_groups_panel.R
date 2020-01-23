# Module UI
  
#' @title   mod_groups_panel_ui and mod_groups_panel_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_groups_panel
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_groups_panel_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      div(
        style = "font-size:90%",
        DT::dataTableOutput(ns("dt_groups"))
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_groups_panel
#' @export
#' @keywords internal
    
mod_groups_panel_server <- function(input, output, session, rv){
  ns <- session$ns

  output$dt_groups <- DT::renderDataTable({
    
    req(rv$attributes_groups)
    
    rv$df_groups <- rv$df_participants_user() %>% 
      df_groups(rv$attributes_groups, user = rv$user$user)
    
    select <- c(rv$attributes_groups, "Participants" = "participants", "Complétés" = "completed", "Taux de réponse" = "response_rate", "Refus" = "optout", "A contacter" = "to_contact", "Suivis" = "n_events", "Date" = "last_event_date")
    
    if (rv$user$user != "admin") {
      dom <- 'rt'
      scrollY <- '40vh'
      
    } else {
      dom <- 'rft'
      scrollY <- '36vh'
      select <- c(select, "Vacataire" = "user")
    }
    
    rv$df_groups %>% 
      dplyr::select(select) %>% 
      DT::datatable(
        selection = list(mode = 'single', selected = 1),
        rownames = FALSE,
        options = list(
          scrollY = scrollY,
          pageLength = -1,
          dom = dom,
          autoWidth = TRUE,
          language = list(search = "Recherche")
        )
      ) %>%
      DT::formatPercentage("Taux de réponse", digits = 1) %>% #, dec.mark = ","
      DT::formatStyle(
        "Taux de réponse",
        target = "row",
        backgroundColor = DT::styleInterval(c(0.499999, 0.649999, 0.749999), c("rgb(251, 145, 131)", "rgb(255, 210, 128)", "rgb(191, 255, 128)", "rgb(0, 179, 0)"))
      ) %>%
      DT::formatDate("Date", method = "toLocaleDateString", params = list("fr-FR"))
    
  })
  
  rv$dt_groups_proxy <- DT::dataTableProxy("dt_groups")
  
  observeEvent(input$dt_groups_search, ignoreInit = TRUE, {

    req(input$dt_groups_rows_current)

    DT::selectRows(rv$dt_groups_proxy, input$dt_groups_rows_current[1])

  })

  rv$input_group <- input
  
}
