#' Get NCAA volleyball team boxscore from a season
#'
#' @param team_name team name, found in [ncaa_wvolleyball_teams()]
#' @param yr year YYYY format
#' @author Tyler Widdison
#' @description
#' @importFrom stringr str_split str_split_fixed str_remove_all str_replace_all str_extract str_detect str_trim str_remove
#' @importFrom lubridate parse_date_time
#' @importFrom dplyr filter tibble case_when mutate if_else bind_rows select
#' @importFrom rvest read_html html_table html_node html_text html_attr html_nodes
#' @importFrom magrittr "%>%"
#' @importFrom purrr map map_df
#' @importFrom tidyr replace_na
#' @export
#'
#' @examples
#' try(ncaa_wvolleyball_boxscore())
#'
#' @return ncaa_wvolleyball_boxscore data.frame: 'Player', 'Pos', 'S', 'MP', 'Kills', 'Errors', 'Total Attacks', 'Hit Pct', 'Assists', 'Aces', 'SErr', 'Digs', 'RErr', 'Block Solos', 'Block Assists', 'BErr', 'PTS', 'BHE', 'team', 'opponent', 'date', 'game_id', 'away', 'home', 'away_sets', 'home_sets', 'away_points', 'away_pointpct', 'home_pointpct'
#'
#' @examples
#' \dontrun{
#'   ncaa_wvolleyball_boxscore('Yale', 2019)
#'   }
#'
#' @export
ncaa_wvolleyball_boxscore <- function(team, year) {

  teams <- ncaavolleyballR::ncaa_wvolleyball_teams() %>%
    dplyr::filter(Name == team)

  sched <- ncaavolleyballR::ncaa_wvolleyball_schedule(team_name = teams$Name, team_id = teams$ID, yr = year)


  pbp_boxscore_links <- function(url){
    all_links <- read_html(url) %>% html_nodes("a")
    box_score_link <- html_attr(all_links, "href")[str_detect(html_text(all_links), "1st Set")] %>% str_remove("\\?period_no=1")
    if (length(box_score_link) == 0) {
      box_score_link <- html_attr(all_links, "href")[str_detect(html_text(all_links), "Box Score") & (html_attr(all_links, "href") != "#")]
    }
    pbp_link <- html_attr(all_links, "href")[str_detect(html_text(all_links), "Play by Play")]

    if (length(pbp_link) == 1) {
      return_links <- paste0("https://stats.ncaa.org", c(box_score_link, pbp_link))
    } else {
      return_links <- c(paste0("https://stats.ncaa.org", box_score_link), NA_character_)
    }

    return_links <- c(url, return_links)

    names(return_links) <- c("url", "box_score", "play_by_play")

    return(return_links)
  }



  pbp_box_urls <- purrr::map_df(sched$url, pbp_boxscore_links)

  game_pbp <- dplyr::inner_join(sched, pbp_box_urls, by = "url") %>%
    dplyr::mutate(game_id = stringr::str_remove(play_by_play, "https://stats.ncaa.org/game/play_by_play/"))

  pbp_url <- game_pbp$play_by_play[which(!is.na(game_pbp$play_by_play))]


  vb_boxscore <- function(box_score_url, include_set_scores = FALSE){
    game <- vector("list")
    game_html <- read_html(box_score_url)

    # Note: for some reason the NCAA occasionally lists box score links but they're broken
    # This will also work if you put in a valid game address on NCAA website but it's for a future game
    if (html_text(game_html) == "Game not found") {
      game <- list(info = NULL,
                   player_stats = NULL
      )
      return(game)
    }
    game_date <- (game_html %>% html_nodes("table:nth-child(1)") %>% html_table())[[1]][1,2] %>%
      lubridate::parse_date_time(orders = c("mdYHMp", "mdY"))
    if (length(game_html %>% html_nodes(".errors")) > 0) {
      game_teams_scores <- (game_html %>% html_nodes("table:nth-child(6)") %>% html_table())[[1]]
    } else{
      game_teams_scores <- (game_html %>% html_nodes("table:nth-child(5)") %>% html_table())[[1]]
    }
    # if there is an error message about the boxscore, then the scores are in the 6th table instead of the 5th

    ## Remove the team records
    game_away <- game_teams_scores[2, 1] %>% str_remove(" \\([0-9]+-[0-9]+.*\\)")
    game_home <- game_teams_scores[3, 1] %>% str_remove(" \\([0-9]+-[0-9]+.*\\)")

    game_away_sets <- game_teams_scores[2, ncol(game_teams_scores)] %>% as.numeric()
    game_home_sets <- game_teams_scores[3, ncol(game_teams_scores)] %>% as.numeric()
    game_away_points <- game_teams_scores[2, 2:(ncol(game_teams_scores) - 1)] %>% as.numeric()
    game_away_total_points <-  sum(game_away_points)
    game_home_points <- game_teams_scores[3, 2:(ncol(game_teams_scores) - 1)] %>% as.numeric()
    game_home_total_points <-  sum(game_home_points)

    set_scores <- tibble(set = seq(1, game_away_sets + game_home_sets),
                         away = game_away_points,
                         home = game_home_points)
    names(set_scores) <- c("Set", game_away, game_home)

    if (length(game_html %>% html_nodes(".errors")) > 0) {
      # if there are known errors then we shouldn't scrape the known-error player data
      game_players_away_stats <- tibble(
        Player = NA_character_,
        Pos = NA_character_,
        S = NA_real_,
        MS = NA_character_,
        Kills = NA_real_,
        Errors = NA_real_,
        `Total Attacks` = NA_real_,
        `Hit Pct` = NA_real_,
        Assists = NA_real_,
        Aces = NA_real_,
        SErr = NA_real_,
        Digs = NA_real_,
        RErr = NA_real_,
        `Block Solos` = NA_real_,
        `Block Assists` = NA_real_,
        BErr = NA_real_,
        PTS = NA_real_,
        BHE = NA_real_,
        team = NA_character_,
        opponent = NA_character_
      )
      game_players_home_stats <- game_players_away_stats
    } else {
      game_players_away <- (game_html %>% html_nodes("table:nth-child(11)") %>% html_table())[[1]]
      game_players_home <- (game_html %>% html_nodes("table:nth-child(13)") %>% html_table())[[1]]

      game_players_away_stats <- suppressMessages(game_players_away %>% clean_vb_box_score()) %>%
        mutate(opponent = game_players_home[1,1])
      game_players_home_stats <- suppressMessages(game_players_home %>% clean_vb_box_score()) %>%
        mutate(opponent = game_players_away[1,1])
    }

    game$info <- tibble(game_id = str_remove(box_score_url, "https://stats.ncaa.org/game/box_score/"),
                        date = game_date,
                        away = game_away,
                        home = game_home,
                        away_sets = game_away_sets,
                        home_sets = game_home_sets,
                        if (include_set_scores) {nest(set_scores, scores = everything())},
                        away_points = game_away_total_points,
                        home_points = game_home_total_points,
                        away_pointpct = game_away_total_points/(game_away_total_points + game_home_total_points),
                        home_pointpct = game_home_total_points/(game_away_total_points + game_home_total_points))
    game$player_stats <- bind_rows(game_players_away_stats, game_players_home_stats) %>%
      mutate(date = game_date,
             game_id = game$info$game_id)

    return(game)
  }

  clean_vb_box_score <- function(box_score_table){
    players_stats <- box_score_table[3:(nrow(box_score_table) - 2),]
    names(players_stats) <- as.character(box_score_table[2,])
    players_stats$team <- box_score_table[1,1]

    players_stats <- players_stats %>%
      mutate(S = as.numeric(str_remove(S, "/")),  # I think the forward slash indicates some kind of season high?
             Kills = as.numeric(str_remove(Kills, "/")),
             Errors = as.numeric(str_remove(Errors, "/")),
             `Total Attacks` = as.numeric(str_remove(`Total Attacks`, "/")),
             `Hit Pct` = as.numeric(str_remove(`Hit Pct`, "/")),
             Assists = as.numeric(str_remove(Assists, "/")),
             Aces = as.numeric(str_remove(Aces, "/")),
             SErr = as.numeric(str_remove(SErr, "/")),
             Digs = as.numeric(str_remove(Digs, "/")),
             RErr = as.numeric(str_remove(RErr, "/")),
             `Block Solos` = as.numeric(str_remove(`Block Solos`, "/")),
             `Block Assists` = as.numeric(str_remove(`Block Assists`, "/")),
             BErr = as.numeric(str_remove(BErr, "/")),
             PTS = as.numeric(str_remove(PTS, "/")),
             BHE = as.numeric(str_remove(BHE, "/"))
      ) %>%
      tidyr::replace_na(list(Kills = 0,
                      Errors = 0,
                      `Total Attacks` = 0,
                      Assists = 0,
                      Aces = 0,
                      SErr = 0,
                      Digs = 0,
                      RErr = 0,
                      `Block Solos` = 0,
                      `Block Assists` = 0,
                      BErr = 0,
                      PTS = 0,
                      BHE = 0))

    return(players_stats)
  }

  dd <- purrr::map(pbp_box_urls$box_score, vb_boxscore)
  dd <- dd[lengths(dd) > 0]

  box_df <- do.call(Map, c(rbind, dd))$player_stats %>%
    dplyr::left_join(do.call(Map, c(rbind, dd))$info) %>%
    dplyr::filter(Player != 'TEAM')

  return(box_df)
}

