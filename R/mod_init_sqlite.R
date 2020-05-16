# Module UI
  
#' @title   mod_init_sqlite_ui and mod_init_sqlite_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_init_sqlite
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_init_sqlite_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_init_sqlite
#' @export
#' @keywords internal
    
mod_init_sqlite_server <- function(input, output, session, rv){
  ns <- session$ns
  
  rv$df_config <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "config")
  
  rv$df_participants <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "participants") %>% 
    dplyr::rename_all(stringr::str_replace_all, "\\.", " ")
  
  rv$df_participants_contacts <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "participants_contacts")
  rv$df_participants_attributes <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "participants_attributes")
  
  rv$df_phoning_team <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "phoning_team")
  rv$df_phoning_team_group <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "phoning_team_group") %>% 
    dplyr::rename_all(stringr::str_replace_all, "\\.", " ")
  
  rv$df_phoning_team_events <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "phoning_team_events")
  
}
