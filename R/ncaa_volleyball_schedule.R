#' Get NCAA volleyball team schedule
#'
#' @param team_name team name, found in [ncaa_wvolleyball_teams()]
#' @param team_id team id, found in [ncaa_wvolleyball_teams()]
#' @param yr year YYYY format
#' @author Tyler Widdison
#' @description
#' @importFrom stringr str_split str_split_fixed str_remove_all str_replace_all str_extract str_detect str_trim str_remove
#' @importFrom lubridate parse_date_time
#' @importFrom dplyr filter tibble case_when mutate if_else
#' @importFrom rvest read_html html_table html_node html_text html_attr html_nodes
#' @importFrom magrittr "%>%"
#' @export
#'
#' @examples
#' try(ncaa_wvolleyball_schedule())
#'
#' @return ncaa_wvolleyball_schedule data.frame: 'url', 'date', 'home', 'away', 'location'
#'
#' @examples
#' \dontrun{
#'   ncaa_wvolleyball_schedule('Yale', 813, 2019)
#'   }
#'
#' @export
ncaa_wvolleyball_schedule <- function(team_name, team_id, yr) {
  # Step 1: figure out if the sport exists
  url <- paste0("https://stats.ncaa.org/team/", team_id)

  schedule_links <- rvest::read_html(url) %>%
    rvest::html_nodes("a")

  schedule_link_detect <- rvest::html_text(schedule_links) %>%
    stringr::str_detect("Women's Volleyball")

  schedule_link <-
    rvest::html_attr(schedule_links, "href")[schedule_link_detect]

  url2 <- paste0("https://stats.ncaa.org", schedule_link)[1]

  # Step 2: see if the team supported the sport in that year
  if (is.numeric(yr)) {
    # if year is numeric, have to convert to academic year
    year <- paste0(yr - 1, "-", yr %% 100)
  }

  if (!is.na(url2)) {
    year_options <-
      rvest::read_html(url2) %>% rvest::html_nodes("option")

    year_detect <-
      rvest::html_text(year_options) %>% stringr::str_detect(as.character(yr))

    if (!any(year_detect)) {
      warning(
        paste(
          "Cannot find the requested year.\nMake sure",
          team_name,
          "supported that sport in that year."
        )
      )
      url3 <- NA
    } else {
      year_link <- rvest::html_attr(year_options, "value")[year_detect]

      url3 <-
        paste0("https://stats.ncaa.org/teams/", year_link)  # finally to the team
    }

  } else{
    url3 <- NA
  }

  # Step 3: find all games played by that team in that sport in that year
  if (!is.na(url3)) {
    games <- rvest::read_html(url3) %>% rvest::html_nodes(".skipMask")

    games_detect <-
      rvest::html_attr(games, "target") == "BOX_SCORE_WINDOW"

    if (!any(games_detect)) {
      warning(paste0(
        team_name,
        " does not appear to have played any games in ",
        yr,
        "."
      ))
      games_url <- NA_character_
      games_date <- NA_character_
      games_home <- NA_character_
      games_away <- NA_character_
      games_location <- NA_character_

    } else {
      games_links <- rvest::html_attr(games, "href")[games_detect]
      games_url <- paste0("https://stats.ncaa.org", games_links)
      ## The below three lines filter the table of games to include only matches with a link to the box score - other games are not counted
      games_table_table <-
        rvest::read_html(url3) %>% rvest::html_nodes("table:nth-child(2)")
      games_with_links <-
        which(stringr::str_detect(
          as.character(games_table_table %>% rvest::html_nodes("tr")),
          "contests"
        ))
      games_table <-
        (games_table_table %>% rvest::html_table())[[1]][games_with_links - 1,]
      # Extract information about each game
      games_date <- games_table[[1]]
      games_home <- dplyr::case_when(
        stringr::str_detect(games_table[[2]], "^\\@") ~ stringr::str_remove(games_table[[2]], "\\@ "),
        stringr::str_detect(games_table[[2]], ".+\\@") ~ stringr::str_remove(games_table[[2]], "\\@.+") %>% stringr::str_trim(),
        TRUE ~ team_name
      )
      games_away <-
        dplyr::if_else(stringr::str_detect(games_table[[2]], "@"),
                       team_name,
                       games_table[[2]])

      # The tricky part is to use regex to get the correct location for away and neutral games
      games_location <- dplyr::case_when(
        stringr::str_detect(games_table[[2]], "\\@(?=[^ ])") ~ stringr::str_extract(games_table[[2]], "\\@(?=[^ ]).*") %>% stringr::str_remove("\\@"),
        stringr::str_detect(games_table[[2]], "\\@") ~ stringr::str_extract(games_table[[2]], "\\@.*$") %>% stringr::str_remove("\\@") %>% stringr::str_trim(),
        TRUE ~ team_name
      )
    }

  } else {
    games_url <- NA_character_
    games_date <- NA_character_
    games_home <- NA_character_
    games_away <- NA_character_
    games_location <- NA_character_

  }

  # Once we've matched as many games as we can, create the data frame
  games_df <- dplyr::tibble(
    url = games_url,
    date = lubridate::parse_date_time(games_date, orders = c("m/d/Y", "m/d/Y(h)")),
    home = games_home,
    away = games_away,
    location = games_location
  )

  return(games_df)
}
