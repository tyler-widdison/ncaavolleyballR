#' Get NCAA volleyball conference schedule
#'
#' @param conf_name conference name, found in [ncaa_wvolleyball_teams()]
#' @param yr year YYYY format
#' @author Tyler Widdison
#' @description
#' @importFrom ncaavolleyballR ncaa_wvolleyball_teams
#' @importFrom dplyr filter distinct arrange
#' @export
#'
#' @examples
#' try(ncaa_wvolleyball_conf_schedule())
#' Return NCAA teams dataframe object
#'
#' @return ncaa_wvolleyball_conf_schedule data.frame: 'url', 'date', 'home', 'away', 'location'
#'
#' @examples
#' \dontrun{
#'   ncaa_wvolleyball_conf_schedule('Big Ten', 2023)
#'   }
#'
#' @export
get_conf_schedule <- function(conf_name, yr) {
  # Return all teams from a conference
  teams <- datavolleyXtra::get_ncaaw_teams()
  # Filter for selected conference
  conf <- teams %>%
    dplyr::filter(Conference == conf_name)
  # Apply to mapply
  schedules <- mapply(get_team_schedule, conf$Name, conf$ID, yr, SIMPLIFY = FALSE)
  combined_schedule <- do.call(rbind, schedules)
  final_schedule <- combined_schedule %>%
    dplyr::distinct(url, date, home, away, location) %>%
    dplyr::arrange(date)
  return(final_schedule)
}
