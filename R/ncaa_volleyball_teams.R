#' Get all NCAA volleyball team names and ids
#'
#' @author Tyler Widdison
#' @description
#' @importFrom stringr str_split str_split_fixed str_remove_all str_replace_all str_extract
#' @importFrom dplyr filter tibble case_when mutate
#' @importFrom rvest read_html html_table html_node html_text
#' @export
#'
#' @examples
#' try(ncaa_wvolleyball_teams())
#'
#' @return ncaa_wvolleyball_teams data.frame: 'Name', 'Conference', 'ID', NOTE: Only Division 1 conferences are returned
#'
#' @examples
#' \dontrun{
#'   get_ncaaw_teams()
#'   }
#'
#' @export
ncaa_wvolleyball_teams <- function(){
  # get all the team names and corresponding IDs from the NCAA website

  # Step 1: get the giant character vector that matches teams and id
  url <- "https://stats.ncaa.org/team/search"
  all_teams_messy <- rvest::read_html(url) %>%
    rvest::html_node("body") %>%
    rvest::html_text()

  # Step 2: split the character vector
  all_teams_matrix <- stringr::str_split(all_teams_messy, ",") %>%
    unlist() %>%
    matrix(ncol = 4, byrow = T)

  teams_and_conferences <- stringr::str_split_fixed(all_teams_matrix[,3], " - ", 2)

  team_names <- stringr::str_remove_all(teams_and_conferences[,1], '\\"label\\"\\:\\"') %>%
    stringr::str_remove_all('\\[') %>%
    stringr::str_replace_all('\\\\u0026', '&')

  team_conferences <- stringr::str_remove_all(teams_and_conferences[,2], '\\"')
  team_ids <- stringr::str_extract(all_teams_matrix[,2], "\\d+")

  # Step 3: reconstitute the teams/ids as a tibble
  teams_df <- dplyr::tibble(
    Name = team_names,
    Conference = team_conferences,
    ID = team_ids
  ) %>%
    dplyr::filter(!grepl('Test', Conference) &
                    Conference %in%
                    c("Pac-12",
                      "Big 12",
                      "Big Ten",
                      "ACC",
                      "SEC",
                      "Big East",
                      "AAC",
                      "America East",
                      "ASUN",
                      "Atlantic 10",
                      "Big Sky",
                      "Big West",
                      "Big South",
                      "CAA",
                      "C-USA",
                      "Horizon",
                      "Ivy League",
                      "MAAC",
                      "MAC",
                      "MEAC",
                      "Mountain West",
                      "MVC",
                      "NEC",
                      "OVC",
                      "Patriot",
                      "SoCon",
                      "Southland",
                      "Summit League",
                      "Sun Belt",
                      "SWAC",
                      "WAC",
                      "WCC"))

  return(teams_df)
}
