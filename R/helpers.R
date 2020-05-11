#' df_groups
#'
#' groups data frame
#' 
#' @param df_participants_user \dots
#' @param attributes_groups \dots
#' 
#' internal function
#' @export
#' @keywords internal
df_groups <- function(df_participants_user, attributes_groups) {
  
  df_participants_user %>% 
    df_participants_events() %>% 
    dplyr::mutate(participants = TRUE) %>% 
    dplyr::group_by_at(c("order", attributes_groups, "user")) %>% 
    dplyr::summarise(
      participants = sum(.data$participants),
      optout = sum(.data$optout),
      completed = sum(.data$completed),
      n_events = suppressWarnings(as.integer(min(.data$n_events, na.rm = TRUE))),
      last_event_date = suppressWarnings(min(.data$last_event_date, na.rm = TRUE))
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      response_rate = .data$completed / .data$participants,
      to_contact = .data$participants - .data$optout - .data$completed,
      n_events = dplyr::if_else(is.na(.data$last_event_date), NA_integer_, .data$n_events)
    ) %>% 
    dplyr::filter(.data$to_contact >= 1) %>% 
    dplyr::arrange(.data$order, dplyr::desc(.data$n_events), .data$last_event_date, .data$response_rate, dplyr::desc(.data$participants))
  
}

#' df_participants_events
#' 
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
        dplyr::filter(.data$type != "launch_questionnaire") %>% 
        dplyr::arrange(.data$token, .data$date) %>%
        dplyr::select(.data$token, .data$date) %>%
        unique() %>%
        dplyr::group_by(.data$token) %>%
        dplyr::summarise(
          n_events = dplyr::n_distinct(.data$date),
          last_event_date = suppressWarnings(max(.data$date))
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(.data$token, .data$n_events, .data$last_event_date),
      by = "token"
    ) %>%
    tidyr::replace_na(list(n_events = 0L)) %>% 
    dplyr::mutate(
      n_events = dplyr::if_else(.data$completed | .data$optout, NA_integer_, .data$n_events),
      last_event_date = dplyr::if_else(.data$completed | .data$optout, NA_character_, .data$last_event_date)
    )
  
}

#' remaining_calls
#' 
#' @param sqlite_base \dots
#' @param cron_responses \dots
#' @param maximal_date \dots
#' @param \dots
#' 
#' internal function
#' @export
#' @keywords internal
remaining_calls <- function(sqlite_base, cron_responses, maximal_date, ...) {
  
  impexp::r_import(cron_responses) %>% 
    dplyr::filter(!.data$completed, !.data$optout) %>% 
    dplyr::anti_join(
      impexp::sqlite_import(sqlite_base, "phoning_team_events") %>% 
        dplyr::filter(lubridate::as_date(.data$date) >= lubridate::as_date(.data$maximal_date)),
      by = "token"
    ) %>% 
    dplyr::anti_join(
      impexp::sqlite_import(sqlite_base, "participants") %>% 
        dplyr::filter(stringr::str_detect(.data$`Contact.local`, "telephone")),
      by = "token"
    ) %>% 
    dplyr::count(...)
  
}
