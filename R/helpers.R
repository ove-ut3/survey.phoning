#' groups data frame
#' 
#' @param df_participants_user \dots
#' @param attributes_groups \dots
#' 
#' internal function
#' @export
#' @keywords internal
df_groups <- function(df_participants_user, attributes_groups, user) {
  
  df_groups <- df_participants_user %>% 
    df_participants_events() %>% 
    dplyr::mutate(participants = TRUE) %>% 
    dplyr::group_by_at(c("order", attributes_groups, "user")) %>% 
    dplyr::summarise(
      participants = sum(participants),
      optout = sum(optout),
      completed = sum(completed),
      n_events = suppressWarnings(as.integer(min(n_events, na.rm = TRUE))),
      last_event_date = suppressWarnings(min(last_event_date, na.rm = TRUE))
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      response_rate = completed / participants,
      to_contact = participants - optout - completed,
      n_events = dplyr::if_else(is.na(last_event_date), NA_integer_, n_events)
    ) %>% 
    dplyr::filter(to_contact >= 1) %>% 
    dplyr::arrange(order, dplyr::desc(n_events), last_event_date, response_rate, dplyr::desc(participants)) %>% 
    dplyr::select_at(c(attributes_groups, "participants", "completed", "response_rate", "optout", "to_contact", "n_events", "last_event_date", "user"))
  
  if (user != "admin") {
    df_groups <- dplyr::select(df_groups, -user)
  }
  
  df_groups
}

#' participants event summary data frame
#' 
#' @param df \dots
#' 
#' internal function
#' @export
#' @keywords internal
df_participants_events <- function(df) {
  
  df %>% 
    dplyr::left_join(
      impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "phoning_team_events") %>%
        dplyr::filter(type != "launch_questionnaire") %>% 
        dplyr::arrange(token, date) %>%
        dplyr::select(token, date) %>%
        unique() %>%
        dplyr::group_by(token) %>%
        dplyr::summarise(
          n_events = dplyr::n_distinct(date),
          last_event_date = suppressWarnings(max(date))
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(token, n_events, last_event_date),
      by = "token"
    ) %>%
    tidyr::replace_na(list(n_events = 0L)) %>% 
    dplyr::mutate(
      n_events = dplyr::if_else(completed | optout, NA_integer_, n_events),
      last_event_date = dplyr::if_else(completed | optout, NA_character_, last_event_date)
    )
  
}