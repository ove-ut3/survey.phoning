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
  
  if (! "phoning_participants_contacts" %in% impexp::sqlite_list_tables(golem::get_golem_options("sqlite_base"))) {
    
    impexp::sqlite_export(
      golem::get_golem_options("sqlite_base"), 
      impexp::sqlite_import(
        golem::get_golem_options("sqlite_base"),
        "participants_contacts"
      ),
      "phoning_participants_contacts"
    )
  }
  
  rv$df_phoning_participants_contacts <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "phoning_participants_contacts")
  
  rv$df_phoning_team <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "phoning_team")
  rv$df_phoning_team_group <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "phoning_team_group") %>% 
    dplyr::rename_all(stringr::str_replace_all, "\\.", " ")
  
  if (! "phoning_team_events" %in% impexp::sqlite_list_tables(golem::get_golem_options("sqlite_base"))) {
    
    impexp::sqlite_export(
      golem::get_golem_options("sqlite_base"), 
      dplyr::tibble(
        token = character(0),
        type = character(0),
        comment = character(0),
        date = character(0),
        datetime = character(0),
        user = character(0),
      ),
      "phoning_team_events"
    )
  }
  
  if (! "phoning_crowdsourcing_log" %in% impexp::sqlite_list_tables(golem::get_golem_options("sqlite_base"))) {
    
    impexp::sqlite_export(
      golem::get_golem_options("sqlite_base"), 
      dplyr::tibble(
        token = character(0),
        key = character(0),
        new_value = character(0),
        old_value = character(0),
        user = character(0),
        date = character(0),
        status = character(0)
      ),
      "phoning_crowdsourcing_log"
    )
  }
  
}
