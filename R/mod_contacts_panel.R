# Module UI
  
#' @title   mod_contacts_panel_ui and mod_contacts_panel_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_contacts_panel
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_contacts_panel_ui <- function(id){
  ns <- NS(id)
  
  uiOutput(ns("ui"))
  
}
    
# Module Server
    
#' @rdname mod_contacts_panel
#' @export
#' @keywords internal
    
mod_contacts_panel_server <- function(input, output, session, rv){
  ns <- session$ns
  
  output$ui <- renderUI({
    
    req(rv$df_participant_selected())
    
    tagList(
      uiOutput(ns("ui_phoning_events")),
      tabsetPanel(
        type = "tabs",
        tabPanel(
          title = "Contacts",
          uiOutput(ns("ui_contacts_valid")),
          div(br(), style = "font-size: 20px"),
          uiOutput(ns("ui_launch_survey")),
          div(br(), style = "font-size: 20px"),
          uiOutput(ns("ui_contacts_invalid"))
        ),
        tabPanel(
          title = "Suivi",
          uiOutput(ns("ui_phone_suivi")),
          div(br(), style = "font-size: 40px"),
          uiOutput(ns("ui_take_appointment")),
          div(br(), style = "font-size: 20px"),
          uiOutput(ns("ui_mailing_sms")),
          div(br(), style = "font-size: 20px"),
          column(
            width = 8,
            fluidRow(
              uiOutput(ns("ui_linkedin"))
            )
          ),
          column(
            width = 4,
            fluidRow(
              uiOutput(ns("ui_optout"))
            )
          )
        ),
        tabPanel(
          title = "Appels manqu\u00e9s",
          div(br(), style = "font-size: 20px"),
          DT::DTOutput(ns("phone_missed_calls"))
        ),
        tabPanel(
          title = "Tous mes rendez-vous",
          div(br(), style = "font-size: 10px;"),
          fullcalendar::fullcalendarOutput(ns("phone_appointments"), height = "80%")
        ),
        tabPanel(
          title = "Aide-m\u00e9moire",
          htmlOutput(ns("ui_help_text"))
        )
      )
    )
    
  })
  
  rv$df_participant_selected <- reactive({

    req(rv$input_participant[["dt_participants_rows_selected"]])

    rv$df_participants_selected() %>%
      dplyr::filter(dplyr::row_number() == rv$input_participant[["dt_participants_rows_selected"]])

  })
  
  rv$df_participant_selected_contacts <- reactive({
    
    rv$df_participants_contacts %>% 
      dplyr::semi_join(
        rv$df_participant_selected(),
        by = "token"
      )
    
  })

  output$ui_phoning_events <- renderUI({
    
    req(rv$df_participant_selected())
    
    data <- rv$df_phoning_team_events %>% 
      dplyr::semi_join(
        rv$df_participant_selected(),
        by = "token"
      ) %>% 
      dplyr::select(.data$type, .data$comment, .data$date) %>% 
      tidyr::drop_na(.data$type) %>% 
      dplyr::add_row(date = as.character(lubridate::today()))
    
    height <- 30 + (nrow(data) * 35) + 20
    height <- paste0(height, "px")
    
    tagList(
      h3("Historique de suivi"),
      rhandsontable::rHandsontableOutput(ns("hot_phoning_events"), height = height)
    )
    
  })
  
  output$hot_phoning_events <- rhandsontable::renderRHandsontable({
    
    rv$df_phoning_team_events %>% 
      dplyr::semi_join(
        rv$df_participant_selected(),
        by = "token"
      ) %>% 
      dplyr::select(.data$type, .data$comment, .data$date, .data$token) %>% 
      tidyr::drop_na(.data$type) %>% 
      dplyr::add_row(
        date = as.character(lubridate::today()),
        token = rv$df_participant_selected()$token
        ) %>% 
      rhandsontable::rhandsontable(rowHeaders = NULL) %>% 
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") %>%
      rhandsontable::hot_rows(rowHeights = 35) %>%
      rhandsontable::hot_cols(colWidths = c(150, 280, 70, 1)) %>% 
      rhandsontable::hot_col(col = "date", type = "date", dateFormat = "YYYY-MM-DD", default = as.character(lubridate::today()))
    
  })
  
  observeEvent(input$hot_phoning_events, {
    
    req(input$hot_phoning_events)
    
    changes <- input$hot_phoning_events$changes
    
    req(!is.null(changes[["ind"]]) | !is.null(changes[["changes"]]))
    
    hot_update <- input$hot_phoning_events %>% 
      rhandsontable::hot_to_r() %>% 
      dplyr::as_tibble() %>% 
      dplyr::mutate(
        datetime = as.character(lubridate::now()),
        user = rv$user$user
      ) %>% 
      tidyr::drop_na(.data$type) %>% 
      dplyr::select(.data$token, .data$type, .data$comment, .data$date, .data$datetime, .data$user)
    
    if (nrow(hot_update) == 0) {
      token_sqlite <- rv$df_participant_selected()$token
    } else {
      token_sqlite <- hot_update$token[1]
    }
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("DELETE FROM phoning_team_events WHERE token = \"{token_sqlite}\";")
    )
    
    impexp::sqlite_append_rows(
      golem::get_golem_options("sqlite_base"),
      hot_update,
      "phoning_team_events"
    )
    
    rv$df_phoning_team_events <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "phoning_team_events"
    )
    
  })
  
  data_proxy <- reactive({
    
    data_proxy <- rv$df_phoning_team_events %>% 
      tidyr::drop_na(.data$type) %>% 
      dplyr::group_by(.data$token) %>% 
      dplyr::summarise(
        hot_n_events = dplyr::n_distinct(.data$date),
        hot_last_event_date = suppressWarnings(max(.data$date))
      ) %>% 
      dplyr::ungroup()
    
  })
  
  observe({

    data_participants_proxy <- rv$df_participants_selected() %>% 
      dplyr::select_at(c("token", "firstname", "lastname", rv$attributes_participants, "lastpage_rate", "n_events", "last_event_date")) %>% 
      dplyr::left_join(
        data_proxy(),
        by = "token"
      ) %>% 
      dplyr::mutate(
        n_events = .data$hot_n_events,
        last_event_date = .data$hot_last_event_date
      ) %>% 
      dplyr::select_at(c("firstname", "lastname", rv$attributes_participants, "lastpage_rate", "n_events", "last_event_date"))
    
    DT::replaceData(
      proxy = rv$dt_participants_proxy,
      data = data_participants_proxy,
      rownames = FALSE,
      resetPaging = FALSE,
      clearSelection = "none"
    )
    
  })
  
  observe({
    
    req(rv$df_groups)
    
    data_groups_proxy <- rv$df_groups %>%
      dplyr::left_join(
        data_proxy() %>%
          dplyr::right_join(
            rv$df_participants_user() %>%
              dplyr::select(c("token", rv$attributes_groups, .data$completed, .data$optout)),
            by = "token"
          ) %>% 
          tidyr::replace_na(list(hot_n_events = 0L)) %>%
          dplyr::mutate(
            hot_n_events = dplyr::if_else(.data$completed | .data$optout, NA_integer_, .data$hot_n_events),
            hot_last_event_date = dplyr::if_else(.data$completed | .data$optout, NA_character_, .data$hot_last_event_date)
          ) %>% 
          dplyr::group_by_at(rv$attributes_groups) %>%
          dplyr::summarise(
            hot_n_events = suppressWarnings(as.integer(min(.data$hot_n_events, na.rm = TRUE))),
            hot_last_event_date = suppressWarnings(min(.data$hot_last_event_date, na.rm = TRUE)),
            hot_n_events = dplyr::if_else(is.na(.data$hot_last_event_date), NA_integer_, .data$hot_n_events)
          ) %>%
          dplyr::ungroup(),
        by = rv$attributes_groups
      ) %>%
      dplyr::mutate(
        n_events = dplyr::if_else(!is.na(.data$hot_n_events), .data$hot_n_events, .data$n_events),
        last_event_date = dplyr::if_else(!is.na(.data$hot_last_event_date), .data$hot_last_event_date, .data$last_event_date)
      ) %>%
      dplyr::select_at(names(rv$df_groups))

    if (rv$user$user != "admin") {
      data_groups_proxy <- dplyr::select(data_groups_proxy, -.data$user)
    }
    
    DT::replaceData(
      proxy = rv$dt_groups_proxy,
      data = data_groups_proxy,
      rownames = FALSE,
      resetPaging = FALSE,
      clearSelection = "none"
    )
    
  })
  
  output$ui_contacts_valid <- renderUI({
    
    req(rv$df_participant_selected_contacts())
    
    tagList(
      h3("Coordonn\u00e9es"),
      rhandsontable::rHandsontableOutput(ns("hot_contacts_valid"))
    )
    
  })
  
  output$hot_contacts_valid <- rhandsontable::renderRHandsontable({
    
    df <- rv$df_participant_selected_contacts() %>% 
      dplyr::filter(!.data$status %in% "invalid") %>% 
      dplyr::filter(!.data$key %in% "tel_etudiant") %>% 
      dplyr::select(.data$key, .data$value, .data$token)
    
    if (nrow(df) == 0) {
      df <- dplyr::add_row(df, token = rv$df_participant_selected()$token)
    }
    
    df %>% 
      rhandsontable::rhandsontable(rowHeaders = NULL) %>% 
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") %>%
      rhandsontable::hot_rows(rowHeights = 35) %>%
      rhandsontable::hot_cols(valign = "htMiddle", colWidths = c(100, 100, 1))
    
  })
  
  observeEvent(input$hot_contacts_valid, {
    
    req(input$hot_contacts_valid)
    
    changes <- input$hot_contacts_valid$changes
    
    req(!is.null(changes[["ind"]]) | !is.null(changes[["changes"]]))
    
    hot_update <- input$hot_contacts_valid %>% 
      rhandsontable::hot_to_r() %>% 
      dplyr::as_tibble()
  
    if (nrow(hot_update) >= 1) {
      hot_update <- tidyr::replace_na(hot_update, list(token = stats::na.omit(hot_update$token)[1]))
    }
    
    hot_update <- hot_update %>% 
      dplyr::left_join(
        rv$df_participants_contacts,
        by = c("token", "key", "value")
      ) %>% 
      dplyr::mutate(status = "valid") %>% 
      dplyr::select(.data$token, .data$key, .data$value, .data$source, .data$date, .data$service, .data$status, .data$status_date) %>% 
      unique()
    
    key <- hot_update[changes$changes[[1]][[1]] + 1, ]$key
    new_value <- hot_update[changes$changes[[1]][[1]] + 1, ]$value
    
    if (isTRUE(!is.na(key) & !is.na(new_value) & new_value != "") | changes$event == "afterRemoveRow") {
      
      if (nrow(hot_update) == 0) {
        token_sqlite <- rv$df_participant_selected()$token
      } else {
        token_sqlite <- hot_update$token[1]
      }
      
      hot_update$source[changes$changes[[1]][[1]] + 1] <- "phoning"
      hot_update$date[changes$changes[[1]][[1]] + 1] <- as.character(lubridate::today())
      
      phoning_crowdsourcing_log <- hot_update %>% 
        survey.admin::df_participants_contacts_crowdsourcing() %>% 
        tidyr::gather("key", "value", -.data$token, na.rm = TRUE) %>% 
        dplyr::rename(new_value = .data$value) %>% 
        dplyr::full_join(
          impexp::sqlite_import(
            golem::get_golem_options("sqlite_base"),
            "participants_contacts"
          ) %>% 
            dplyr::filter(.data$token == token_sqlite, !.data$status %in% "invalid") %>% 
            survey.admin::df_participants_contacts_crowdsourcing() %>% 
            tidyr::gather("key", "value", -.data$token, na.rm = TRUE) %>% 
            dplyr::rename(old_value = .data$value),
          by = c("token", "key")
        ) %>% 
        dplyr::filter(purrr::map2_lgl(.data$old_value, .data$new_value, ~ !.x %in% .y | !.y %in% .x)) %>% 
        dplyr::mutate(
          user = rv$user$user,
          date = as.character(lubridate::today()),
          status = NA_character_
        )
      
      impexp::sqlite_append_rows(
        golem::get_golem_options("sqlite_base"),
        phoning_crowdsourcing_log,
        "phoning_crowdsourcing_log"
      )
      
      impexp::sqlite_execute_sql(
        golem::get_golem_options("sqlite_base"),
        glue::glue("DELETE FROM participants_contacts WHERE token = \"{token_sqlite}\" AND (status != \"invalid\" OR status IS NULL);")
      )
      
      impexp::sqlite_append_rows(
        golem::get_golem_options("sqlite_base"),
        hot_update,
        "participants_contacts"
      )
      
      rv$df_participants_contacts <- impexp::sqlite_import(
        golem::get_golem_options("sqlite_base"),
        "participants_contacts"
      )
      
    }
    
  })

  output$ui_launch_survey <- renderUI({
    
    tagList(
      h3("Lancer le questionnaire"),
      actionButton(
        ns("launch_survey"), 
        "Lancer le questionnaire", 
        icon = icon("pencil-square-o"), 
        onclick = paste0("window.open('", rv$df_participant_selected()$surveyurl, "', '_blank')")
      )
    )
    
  })
  
  observeEvent(input$launch_survey, {

    impexp::sqlite_append_rows(
      golem::get_golem_options("sqlite_base"),
      dplyr::tibble(
        token = rv$df_participant_selected()$token,
        type = "launch_questionnaire",
        comment = NA_character_,
        date = as.character(lubridate::today()),
        datetime = as.character(lubridate::now()),
        user = rv$user$user
      ),
      "phoning_team_events"
    )
    
    rv$df_phoning_team_events <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "phoning_team_events"
    )

  })
  
  output$ui_contacts_invalid <- renderUI({
    
    req(rv$df_participant_selected_contacts())
    
    tagList(
      h3("Coordonn\u00e9es invalides"),
      rhandsontable::rHandsontableOutput(ns("hot_contacts_invalid"))
    )
    
  })
  
  output$hot_contacts_invalid <- rhandsontable::renderRHandsontable({
    
    df <-  rv$df_participant_selected_contacts() %>% 
      dplyr::filter(.data$status == "invalid") %>% 
      dplyr::filter(.data$key != "tel_etudiant") %>% 
      dplyr::select(.data$key, .data$value, .data$token)
    
    if (nrow(df) == 0) {
      df <- dplyr::add_row(df, token = rv$df_participant_selected()$token)
    }
    
    df %>% 
      rhandsontable::rhandsontable(rowHeaders = NULL) %>% 
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") %>%
      rhandsontable::hot_rows(rowHeights = 35) %>%
      rhandsontable::hot_cols(valign = "htMiddle", colWidths = c(100, 100, 1))
    
  })
  
  observeEvent(input$hot_contacts_invalid, {
    
    req(input$hot_contacts_invalid)
    
    changes <- input$hot_contacts_invalid$changes
    
    req(!is.null(changes[["ind"]]) | !is.null(changes[["changes"]]))
    
    hot_update <- input$hot_contacts_invalid %>% 
      rhandsontable::hot_to_r() %>% 
      dplyr::as_tibble()
    
    if (nrow(hot_update) >= 1) {
      hot_update <- tidyr::replace_na(hot_update, list(token = stats::na.omit(hot_update$token)[1]))
    }
    
    hot_update <- hot_update %>% 
      dplyr::left_join(
        rv$df_participants_contacts,
        by = c("token", "key", "value")
      ) %>% 
      dplyr::mutate(status = "invalid") %>% 
      dplyr::select(.data$token, .data$key, .data$value, .data$source, .data$date, .data$service, .data$status, .data$status_date) %>% 
      unique()
    
    key <- hot_update[changes$changes[[1]][[1]] + 1, ]$key
    new_value <- hot_update[changes$changes[[1]][[1]] + 1, ]$value
    
    if (isTRUE(!is.na(key) & !is.na(new_value) & new_value != "") | changes$event == "afterRemoveRow") {
      
      if (nrow(hot_update) == 0) {
        token_sqlite <- rv$df_participant_selected()$token
      } else {
        token_sqlite <- hot_update$token[1]
      }
      
      hot_update$source[changes$changes[[1]][[1]] + 1] <- "phoning"
      hot_update$date[changes$changes[[1]][[1]] + 1] <- as.character(lubridate::today())
      
      phoning_crowdsourcing_log <- hot_update %>% 
        survey.admin::df_participants_contacts_crowdsourcing() %>% 
        tidyr::gather("key", "value", -.data$token, na.rm = TRUE) %>% 
        dplyr::rename(new_value =.data$ value) %>% 
        dplyr::full_join(
          impexp::sqlite_import(
            golem::get_golem_options("sqlite_base"),
            "participants_contacts"
          ) %>% 
            dplyr::filter(.data$token == token_sqlite, .data$status == "invalid") %>% 
            survey.admin::df_participants_contacts_crowdsourcing() %>% 
            tidyr::gather("key", "value", -.data$token, na.rm = TRUE) %>% 
            dplyr::rename(old_value = .data$value),
          by = c("token", "key")
        ) %>% 
        dplyr::filter(purrr::map2_lgl(.data$old_value, .data$new_value, ~ !.x %in% .y | !.y %in% .x)) %>% 
        dplyr::mutate(
          user = rv$user$user,
          date = as.character(lubridate::today()),
          status = NA_character_
        )
      
      impexp::sqlite_append_rows(
        golem::get_golem_options("sqlite_base"),
        phoning_crowdsourcing_log,
        "phoning_crowdsourcing_log"
      )
      
      impexp::sqlite_execute_sql(
        golem::get_golem_options("sqlite_base"),
        glue::glue("DELETE FROM participants_contacts WHERE token = \"{token_sqlite}\" AND status = \"invalid\";")
      )
      
      impexp::sqlite_append_rows(
        golem::get_golem_options("sqlite_base"),
        hot_update,
        "participants_contacts"
      )
      
      rv$df_participants_contacts <- impexp::sqlite_import(
        golem::get_golem_options("sqlite_base"),
        "participants_contacts"
      )
      
    }
    
  })
  
  output$ui_phone_suivi <- renderUI({
    
    tagList(
      h3("Suivi d'appel"),
      column(
        width = 4,
        actionButton(
          ns("message_laisse"),
          "Message laiss\u00e9"
        )
      ),
      column(
        width = 4,
        actionButton(
          ns("aucune_messagerie"),
          "Pas de messagerie"
        )
      ),
      column(
        width = 4,
        actionButton(
          ns("faux_numero"),
          "Faux num\u00e9ro"
        )
      )
    )
    
  })
  
  observeEvent(input$message_laisse, {
    
    impexp::sqlite_append_rows(
      golem::get_golem_options("sqlite_base"),
      dplyr::tibble(
        token = rv$df_participant_selected()$token,
        type = "Message laiss\u00e9",
        comment = "Num\u00e9ro:",
        date = as.character(lubridate::today()),
        datetime = as.character(lubridate::now()),
        user = rv$user$user
      ),
      "phoning_team_events"
    )
    
    rv$df_phoning_team_events <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "phoning_team_events"
    )
    
  })
  
  observeEvent(input$aucune_messagerie, {
    
    impexp::sqlite_append_rows(
      golem::get_golem_options("sqlite_base"),
      dplyr::tibble(
        token = rv$df_participant_selected()$token,
        type = "Pas de messagerie",
        comment = "Num\u00e9ro:",
        date = as.character(lubridate::today()),
        datetime = as.character(lubridate::now()),
        user = rv$user$user
      ),
      "phoning_team_events"
    )
    
    rv$df_phoning_team_events <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "phoning_team_events"
    )
    
  })
  
  observeEvent(input$faux_numero, {
    
    impexp::sqlite_append_rows(
      golem::get_golem_options("sqlite_base"),
      dplyr::tibble(
        token = rv$df_participant_selected()$token,
        type = "Faux num\u00e9ro",
        comment = "Num\u00e9ro:",
        date = as.character(lubridate::today()),
        datetime = as.character(lubridate::now()),
        user = rv$user$user
      ),
      "phoning_team_events"
    )
    
    rv$df_phoning_team_events <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "phoning_team_events"
    )
    
  })
  
  output$ui_take_appointment <- renderUI({
    
    req(rv$df_participant_selected_contacts())
    
    tagList(
      h3("Prendre un rendez-vous"),
      div(
        style = "display: inline-block; vertical-align: top;",
        shinyWidgets::airDatepickerInput(
          inputId = ns("appointment"),
          timepicker = TRUE,
          addon = "none",
          language = "fr",
          timepickerOpts = shinyWidgets::timepickerOptions(
            timeFormat = "hh:i",
            minHours = 17,
            maxHours = 21,
            minutesStep = 5
          )
        )
      ),
      div(
        style = "display: inline-block; vertical-align: top;",
        actionButton(ns("take_appointment"), "Enregistrer", icon = icon("calendar-alt"))
      )
    )
    
  })
  
  observeEvent(input$take_appointment, {
    
    req(input$appointment)
    
    if (rv$user$user == "admin") {
      
      user <- rv$df_participant_selected()$user
      
    } else {
      
      user <- rv$user$user
      
    }
    
    df <- dplyr::tibble(
      token = rv$df_participant_selected()$token,
      type = "Rendez-vous t\u00e9l\u00e9phonique",
      comment = format(input$appointment, "Le %d/%m/%Y \u00e0 %H:%M"),
      date = as.character(lubridate::today()),
      datetime = as.character(lubridate::now()),
      user = user,
    )
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      paste0('DELETE FROM phoning_team_events WHERE type = \"Rendez-vous t\u00e9l\u00e9phonique\" AND token = "', rv$df_participant_selected()$token, '";')
    )
    
    impexp::sqlite_append_rows(
      golem::get_golem_options("sqlite_base"),
      df,
      "phoning_team_events"
    )
    
    rv$df_phoning_team_events <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "phoning_team_events"
    )
    
  })
  
  next_appointment <- reactivePoll(
    60 * 5 * 1000,
    session,
    checkFunc = function() { Sys.time() },
    valueFunc = function() {

    req(rv$user$user)

      rv$df_phoning_team_events %>%
      dplyr::filter(
        .data$type == "Rendez-vous t\u00e9l\u00e9phonique",
        .data$user == rv$user$user
      ) %>%
      dplyr::mutate_at("comment", stringr::str_replace, "Le (.+?) \u00e0 (.+)", "\\1 \\2:00") %>%
      dplyr::mutate_at("comment", lubridate::dmy_hms) %>%
      dplyr::filter(.data$comment >= lubridate::now(tz = "UTC") + 60 * 60) %>%
      dplyr::arrange(.data$comment) %>%
      utils::head(1) %>% 
      dplyr::left_join(
        rv$df_participants %>% 
          dplyr::select(.data$token, .data$firstname, .data$lastname, rv$attributes_groups),
        by = "token"
      )

  })

  observe({
    
    req(nrow(next_appointment()) == 1)
    
    invalidateLater(60 * 5 * 1000)
    
    diff_time <- difftime(next_appointment()$comment, lubridate::now(tz = "UTC") + 60 * 60, units = "secs")
    
    if (diff_time >= 0 & diff_time <= (60 * 5)) {
      showNotification(HTML(glue::glue("Rendez-vous \u00e0 {format(next_appointment()$comment, '%H:%M')} :<br>{next_appointment()$firstname} {next_appointment()$lastname}<br>{next_appointment()[[rv$attributes_groups[1]]]}")), duration = NULL, type = "error")
    }

  })
  
  output$ui_mailing_sms <- renderUI({
    
    req(rv$df_participant_selected_contacts())
    
    emails_list <- rv$df_participant_selected_contacts() %>% 
      dplyr::filter(.data$key == "email") %>% 
      dplyr::filter(!.data$status %in% "invalid") %>% 
      dplyr::pull(.data$value)
    
    ui <- tagList(
      h3("Relance email")
    )
    
    if (length(emails_list) >= 1) {
      
      ui <- tagAppendChildren(
        ui,
        div(
          div(
            style = "display: inline-block; vertical-align: top;",
            selectInput(ns("select_email"), label = NULL, choices = emails_list)
          ),
          div(
            style = "display: inline-block; vertical-align: top;",
            actionButton(ns("confirm_mailing"), "Envoyer email", icon = icon("envelope-o"))
          )
        )
      )
      
    }

    ui
    
  })
  
  observeEvent(input$confirm_mailing, {
    
    shinyalert::shinyalert(inputId = "mailing", title = "Do you confirm mailing ?", type = "info", showCancelButton = TRUE, closeOnEsc = FALSE)
    
  })
  
  observeEvent(input$mailing, {
    
    req(input$mailing)
    
    participants <- rv$df_participant_selected() %>% 
      dplyr::mutate(email = input$select_email)
    
    participants_attributes <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "participants_attributes") %>% 
      tidyr::separate_rows(.data$survey_id, sep = ";") %>% 
      dplyr::filter(.data$survey_id == rv$df_participant_selected()$survey_id)
    
    mail_template <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "mail_template"
    ) %>% 
      dplyr::mutate_at("value", survey.admin::escape_space_glue, participants_attributes) %>% 
      dplyr::mutate_at("value", ~ purrr::map_chr(., ~ glue::glue_data(., .x = rv$df_participant_selected()))) %>% 
      dplyr::mutate_at("value", stringr::str_replace_all, "'+", "'") %>% 
      split(x = .$value, f = .$key)
    
    survey.admin::mailing(
      participants = participants,
      participants_attributes = rv$df_participants_attributes,
      from = list(
        "email" = mail_template$sender_email,
        "alias" = mail_template$sender_alias
      ),
      subject = mail_template$subject,
      body = mail_template$body,
      sleep = 0
    )
    
    impexp::sqlite_append_rows(
      golem::get_golem_options("sqlite_base"),
      dplyr::tibble(
        token = rv$df_participant_selected()$token,
        type = "email",
        comment = input$select_email,
        date = as.character(lubridate::today()),
        datetime = as.character(lubridate::now()),
        user = rv$user$user
      ),
      "phoning_team_events"
    )
    
    rv$df_phoning_team_events <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "phoning_team_events"
    )
    
  })
  
  output$ui_linkedin <- renderUI({
    
    req(rv$df_participant_selected_contacts())
    
    linkedin_search_suffix_text <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "linkedin"
    ) %>%
      dplyr::filter(.data$key == "search_text_input") %>% 
      dplyr::pull(.data$value)
    
    invitation_text_fr <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "linkedin") %>% 
      dplyr::filter(.data$key == "invitation_text_fr") %>% 
      dplyr::pull(.data$value) %>% 
      survey.admin::escape_space_glue(
        impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "participants_attributes") %>% 
          tidyr::separate_rows(.data$survey_id, sep = ";") %>% 
          dplyr::filter(.data$survey_id == rv$df_participant_selected()$survey_id)
      ) %>% 
      glue::glue_data(.x = rv$df_participant_selected()) %>% 
      glue::glue_data(.x = rv$df_participant_selected()) %>% 
      iconv(from = "UTF-8")
    
    invitation_text_en <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "linkedin") %>% 
      dplyr::filter(.data$key == "invitation_text_en") %>% 
      dplyr::pull(.data$value) %>% 
      survey.admin::escape_space_glue(
        impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "participants_attributes") %>% 
          tidyr::separate_rows(.data$survey_id, sep = ";") %>% 
          dplyr::filter(.data$survey_id == rv$df_participant_selected()$survey_id)
      ) %>% 
      glue::glue_data(.x = rv$df_participant_selected()) %>% 
      glue::glue_data(.x = rv$df_participant_selected()) %>% 
      iconv(from = "UTF-8")
    
    tagList(
      h3("Linkedin"),
      div(
        actionButton(
          ns("linkedin_button_search"),
          label = "Recherche", 
          icon = icon("search"), 
          onclick = paste0("window.open('", paste0("https://www.linkedin.com/search/results/all/?keywords=", rv$df_participant_selected()$firstname, "%20", rv$df_participant_selected()$lastname, "%20", linkedin_search_suffix_text), "', '_blank')")
        )
      ),
      br(),
      rclipboard::rclipButton(ns("linkedin_invitation_fr"), "Invitation (fr)", invitation_text_fr, icon("clipboard")),
      rclipboard::rclipButton(ns("linkedin_invitation_en"), "Invitation (en)", invitation_text_en, icon("clipboard"))
    )
    
  })
  
  observeEvent(input$linkedin_button_search, {
    
    impexp::sqlite_append_rows(
      golem::get_golem_options("sqlite_base"),
      dplyr::tibble(
        token = rv$df_participant_selected()$token,
        type = "Recherche linkedin",
        comment = NA_character_,
        date = as.character(lubridate::today()),
        datetime = as.character(lubridate::now()),
        user = rv$user$user
      ),
      "phoning_team_events"
    )
    
    rv$df_phoning_team_events <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "phoning_team_events"
    )
    
  })
  
  observeEvent(input$linkedin_invitation_fr, {
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      paste0('DELETE FROM phoning_team_events WHERE type = \"Recherche linkedin\" AND token = "', rv$df_participant_selected()$token, '";')
    )
    
    impexp::sqlite_append_rows(
      golem::get_golem_options("sqlite_base"),
      dplyr::tibble(
        token = rv$df_participant_selected()$token,
        type = "linkedin_invitation",
        comment = NA_character_,
        date = as.character(lubridate::today()),
        datetime = as.character(lubridate::now()),
        user = rv$user$user
      ),
      "phoning_team_events"
    )
    
    rv$df_phoning_team_events <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "phoning_team_events"
    )
    
  })
  
  observeEvent(input$linkedin_invitation_en, {
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      paste0('DELETE FROM phoning_team_events WHERE type = \"Recherche linkedin\" AND token = "', rv$df_participant_selected()$token, '";')
    )
    
    impexp::sqlite_append_rows(
      golem::get_golem_options("sqlite_base"),
      dplyr::tibble(
        token = rv$df_participant_selected()$token,
        type = "linkedin_invitation",
        comment = NA_character_,
        date = as.character(lubridate::today()),
        datetime = as.character(lubridate::now()),
        user = rv$user$user
      ),
      "phoning_team_events"
    )
    
    rv$df_phoning_team_events <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "phoning_team_events"
    )
    
  })
  
  output$ui_optout <- renderUI({
    
    req(rv$df_participant_selected_contacts())
    
    tagList(
      h3("Refus de r\u00e9pondre"),
      actionButton(
        ns("confirm_optout"),
        label = "Refus",
        icon = icon("frown"),
        style = "padding: 10px; font-size: 150%"
      ),
      div(br(), style = "font-size: 10px;"),
      actionButton(
        ns("optin"),
        label = "Annulation",
        icon = icon("smile"),
        style = "padding: 10px; font-size: 150%"
      )
    )
    
  })
  
  observeEvent(input$confirm_optout, {
    
    shinyalert::shinyalert(
      inputId = "optout",
      title = "Validation du refus de r\u00e9ponse",
      type = "info",
      showCancelButton = TRUE,
      closeOnEsc = FALSE
    )

  })
  
  observeEvent(input$optout, {
    
    req(input$optout)
    
    key <- limer::get_session_key()
    
    set_participant_properties <- limer::call_limer(
      method = "set_participant_properties", 
      params = list(
        "iSurveyID" = rv$df_participant_selected()$survey_id,
        "aTokenQueryProperties" = rv$df_participant_selected()$tid,
        "aTokenData" = list("emailstatus" = "OptOut")
      )
    )
    
    release <- limer::release_session_key()
    
    impexp::sqlite_append_rows(
      golem::get_golem_options("sqlite_base"),
      dplyr::tibble(
        token = rv$df_participant_selected()$token,
        type = "Refus de r\u00e9pondre",
        comment = NA_character_,
        date = as.character(lubridate::today()),
        datetime = as.character(lubridate::now()),
        user = rv$user$user
      ),
      "phoning_team_events"
    )
    
    rv$df_phoning_team_events <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "phoning_team_events"
    )

  })
  
  observeEvent(input$optin, {
    
    key <- limer::get_session_key()
    
    set_participant_properties <- limer::call_limer(
      method = "set_participant_properties", 
      params = list(
        "iSurveyID" = rv$df_participant_selected()$survey_id,
        "aTokenQueryProperties" = rv$df_participant_selected()$tid,
        "aTokenData" = list("emailstatus" = "OK")
      )
    )
    
    release <- limer::release_session_key()

    impexp::sqlite_append_rows(
      golem::get_golem_options("sqlite_base"),
      dplyr::tibble(
        token = rv$df_participant_selected()$token,
        type = "Annulation du refus",
        comment = NA_character_,
        date = as.character(lubridate::today()),
        datetime = as.character(lubridate::now()),
        user = rv$user$user
      ),
      "phoning_team_events"
    )
    
    rv$df_phoning_team_events <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "phoning_team_events"
    )
    
  })
  
  output$phone_missed_calls <- DT::renderDT({
    
    data <- rv$df_participants_contacts %>% 
      dplyr::filter(stringr::str_detect(.data$key, "^tel_")) %>% 
      dplyr::select(.data$token, .data$key, .data$value) %>% 
      dplyr::inner_join(
        rv$df_participants_user() %>% 
          dplyr::select(.data$token, .data$firstname, .data$lastname, rv$attributes_groups),
        by = "token"
      ) %>% 
      dplyr::select(.data$value, .data$key, .data$firstname, .data$lastname, rv$attributes_groups)
    
    names(data) <- c("value", "key", "Pr\u00e9nom", "Nom", rv$attributes_groups)

    data %>% 
      DT::datatable(
        selection = 'none',
        rownames = FALSE,
        options = list(
          pageLength = -1,
          dom = 'rft',
          scrollY = '60vh'
        )
      )
    
  })
  
  output$phone_appointments <- fullcalendar::renderFullcalendar({

    data <- rv$df_phoning_team_events %>%
      dplyr::filter(.data$type == "Rendez-vous t\u00e9l\u00e9phonique") %>% 
      dplyr::select(-.data$user) %>% 
      dplyr::inner_join(
        rv$df_participants_user() %>%
          dplyr::select_at(c("token", "group" = rv$attributes_groups, "firstname", "lastname", "user")),
        by = "token"
      )
    
    if (rv$user$user == "admin") {
      data <- dplyr::mutate(data, title = glue::glue("{user} - {firstname} {lastname} - {group}"))
    } else {
      data <- dplyr::mutate(data, title = glue::glue("{firstname} {lastname} - {group}"))
    }
    
    data %>% 
      dplyr::mutate_at("comment", stringr::str_replace, "Le (.+?) \u00e0 (.+)", "\\1 \\2:00") %>% 
      dplyr::mutate_at("comment", lubridate::dmy_hms) %>% 
      dplyr::select(.data$title, start = .data$comment) %>%
      fullcalendar::fullcalendar(
        settings = list(
          defaultView = "listWeek",
          titleFormat = "DD MMM YYYY",
          noEventsMessage = "Aucun rendez-vous programm\u00e9",
          firstDay = 1,
          timeFormat = "H:mm",
          buttonText = list("today" = "Aujourd'hui"),
          listDayAltFormat = "DD MMMM YYYY",
          dayNames = list("Dimanche","Lundi","Mardi","Mercredi","Jeudi","Vendredi","Samedi"),
          dayNamesShort = list("Dim","Lun","Mar","Mer","Jeu","Ven","Sam"),
          monthNames = list("Janvier","F\u00e9vrier","Mars","Avril","Mai","Juin","Juillet","Ao\u00fbt","Septembre","Octobre","Novembre","D\u00e9cembre"),
          monthNamesShort = list("Jan","Fev","Mar","Avr","Mai","Juin","Juil","Aou","Sep","Oct","Nov","D\u00e9c")
        )
      )

  })
  
  output$ui_help_text <- renderText({
    
    rv$df_config %>% 
      dplyr::filter(.data$key == "phoning_help_text") %>% 
      dplyr::pull(.data$value) %>% 
      stringr::str_replace_all("\n", "<br/>") %>% 
      HTML()
    
  })
  
}
