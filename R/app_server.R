#' @import shiny
app_server <- function(input, output, session) {
  # List the first level callModules here
  
  rv <- shiny::reactiveValues()
  
  callModule(mod_init_sqlite_server, "init_sqlite_ui", rv)

  config_limesurvey <- impexp::sqlite_import(
    golem::get_golem_options("sqlite_base"),
    "config"
  ) %>% 
    dplyr::filter(stringr::str_detect(.data$key, "^lime_")) %>% 
    split(x = .$value, f = .$key)
  
  options(lime_api = config_limesurvey$lime_api)
  options(lime_username = config_limesurvey$lime_username)
  options(lime_password = config_limesurvey$lime_password)
  
  rv$attributes_groups <- impexp::sqlite_import(
    golem::get_golem_options("sqlite_base"),
    "config"
  ) %>%
    dplyr::filter(.data$key == "phoning_attributes_groups") %>%
    tidyr::separate_rows(.data$value, sep = ";") %>%
    dplyr::pull(.data$value) %>% 
    dplyr::na_if("") %>% 
    stats::na.omit()
  
  rv$attributes_participants <- impexp::sqlite_import(
    golem::get_golem_options("sqlite_base"),
    "config"
  ) %>% 
    dplyr::filter(.data$key == "phoning_attributes_participants") %>% 
    tidyr::separate_rows(.data$value, sep = ";") %>% 
    dplyr::pull(.data$value) %>% 
    dplyr::na_if("") %>% 
    stats::na.omit()
  
  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(
      impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "phoning_team"),
    ),
    timeout = 0
  )

  rv$df_participants_user <- reactive({

    rv$user <- reactiveValuesToList(res_auth)
    
    data <- rv$df_participants %>% 
      dplyr::left_join(
        impexp::r_import(golem::get_golem_options("cron_responses")),
        by = c("survey_id", "token")
      ) %>% 
      dplyr::arrange_at(c(rv$attributes_groups, "lastname", "firstname"))
    
    if (rv$user$user != "admin") {
      
      data <- data %>% 
        dplyr::inner_join(
          rv$df_phoning_team_group %>% 
            dplyr::filter(.data$user == rv$user$user),
          by = rv$attributes_groups
        )
      
    } else {

      data <- data %>%
        dplyr::inner_join(
          rv$df_phoning_team_group,
          by = rv$attributes_groups
        )

    }
    
    data

  })

  callModule(mod_groups_panel_server, "groups_panel_ui", rv)
  
  callModule(mod_participants_panel_server, "participants_panel_ui", rv)
  
  callModule(mod_contacts_panel_server, "contacts_panel_ui", rv)
  
}
