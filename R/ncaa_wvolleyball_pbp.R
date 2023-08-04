#' Get NCAA volleyball team play by play from a season
#'
#' @param team_name team name, found in [ncaa_wvolleyball_teams()]
#' @param yr year YYYY format
#' @author Tyler Widdison
#' @description
#' @importFrom stringr str_split str_split_fixed str_remove_all str_replace_all str_extract str_detect str_trim str_remove str_squish str_to_title
#' @importFrom lubridate parse_date_time
#' @importFrom dplyr filter tibble case_when mutate if_else bind_rows select inner_join lag group_by ungroup rename first
#' @importFrom rvest read_html html_table html_node html_text html_attr html_nodes
#' @importFrom magrittr "%>%"
#' @importFrom purrr map map_df
#' @importFrom tidyr replace_na fill
#' @export
#'
#' @examples
#' try(ncaa_wvolleyball_pbp())
#'
#' @return ncaa_wvolleyball_pbp data.frame: 'set_number', 'skill', 'team', 'player_name', 'away_score', 'home_score', 'serving_team', 'away_team', 'home_team', 'opponent', 'point_won_by', 'ncaa_match_id', 'point_id', 'id', 'location', 'box_score', 'play_by_play'
#'
#' @examples
#' \dontrun{
#'   ncaa_wvolleyball_pbp('Yale', 2019)
#'   }
#'
#' @export
ncaa_wvolleyball_pbp <- function(team, year) {

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

  safe_pbp_boxscore_links <- function(url) {
    tryCatch(pbp_boxscore_links(url),
             error = function(e) NULL)
  }

  pbp_box_urls <- map_df(sched$url, safe_pbp_boxscore_links)

  game_pbp <- inner_join(sched, pbp_box_urls, by = "url") %>% mutate(game_id = str_remove(play_by_play, "https://stats.ncaa.org/game/play_by_play/"))

  pbp_url <- game_pbp$play_by_play[which(!is.na(game_pbp$play_by_play))]

  #Cleaning code
  fix_wncaa_names <- function(x){
    case_when(str_detect(x, "#") ~ str_remove(x, "#[0-9]+ "),
              x == "Americanan" ~ "American",
              x == "N.C. AT" ~ "N.C. A&T",
              str_detect(x, "Alabama AM") ~ "Alabama A&M",
              str_detect(x, "Texas AM") ~ "Texas A&M",
              str_detect(x, "Florida AM") ~ "Florida A&M",
              x == "LMU (CA) (CA)" | x == "LMU" ~ "LMU (CA)",
              x == "Binghamtonmton" ~ "Binghamton",
              x == "Saint Marys" | x == "St. Marys" ~ "Saint Mary's (CA)",
              x == "Gardner" ~ "Gardner-Webb",
              x == "St. Johns" ~ "St. John's (NY)",
              x == "William Mary" ~ "William & Mary",
              x == "Bethune" ~ "Bethune-Cookman",
              x == "Saint Francis" ~ "Saint Francis (PA)",
              x == "Saint Peters" ~ "Saint Peter's",
              TRUE ~ x)
  } # This function fixes every team naming issue seen in 2021 (spring and fall) data

  # Miami issue
  fix_wncaa_parentheses <- function(x){
    x %>% mutate(team = case_when(
      team == "Albany" & (away_team == "Albany (NY)" | home_team == "Albany (NY)") ~ "Albany (NY)",
      team == "LMU" & (away_team == "LMU (CA)" | home_team == "LMU (CA)") ~ "LMU (CA)",
      team == "Miami" & (away_team == "Miami (FL)" | home_team == "Miami (FL)") ~ "Miami (FL)",
      team == "Miami" & (away_team == "Miami (OH)" | home_team == "Miami (OH)") ~ "Miami (OH)",
      team == "Saint Francis" & (away_team == "Saint Francis (PA)" | home_team == "Saint Francis (PA)") ~ "Saint Francis (PA)",
      team == "Saint Mary's" & (away_team == "Saint Mary's (CA)" | home_team == "Saint Mary's (CA)") ~ "Saint Mary's (CA)",
      team == "St. John's" & (away_team == "St. John's (NY)" | home_team == "St. John's (NY)") ~ "St. John's (NY)",
      team == "St. Thomas" & (away_team == "St. Thomas (MN)" | home_team == "St. Thomas (MN)") ~ "St. Thomas (MN)",
      TRUE ~ team
    ),
    serving_team = case_when(
      serving_team == "Albany" & (away_team == "Albany (NY)" | home_team == "Albany (NY)") ~ "Albany (NY)",
      serving_team == "LMU" & (away_team == "LMU (CA)" | home_team == "LMU (CA)") ~ "LMU (CA)",
      serving_team == "Miami" & (away_team == "Miami (FL)" | home_team == "Miami (FL)") ~ "Miami (FL)",
      serving_team == "Miami" & (away_team == "Miami (OH)" | home_team == "Miami (OH)") ~ "Miami (OH)",
      serving_team == "Saint Francis" & (away_team == "Saint Francis (PA)" | home_team == "Saint Francis (PA)") ~ "Saint Francis (PA)",
      serving_team == "Saint Mary's" & (away_team == "Saint Mary's (CA)" | home_team == "Saint Mary's (CA)") ~ "Saint Mary's (CA)",
      serving_team == "St. John's" & (away_team == "St. John's (NY)" | home_team == "St. John's (NY)") ~ "St. John's (NY)",
      serving_team == "St. Thomas" & (away_team == "St. Thomas (MN)" | home_team == "St. Thomas (MN)") ~ "St. Thomas (MN)",
      TRUE ~ serving_team
    ),
    point_won_by = case_when(
      point_won_by == "Albany" & (away_team == "Albany (NY)" | home_team == "Albany (NY)") ~ "Albany (NY)",
      point_won_by == "LMU" & (away_team == "LMU (CA)" | home_team == "LMU (CA)") ~ "LMU (CA)",
      point_won_by == "Miami" & (away_team == "Miami (FL)" | home_team == "Miami (FL)") ~ "Miami (FL)",
      point_won_by == "Miami" & (away_team == "Miami (OH)" | home_team == "Miami (OH)") ~ "Miami (OH)",
      point_won_by == "Saint Francis" & (away_team == "Saint Francis (PA)" | home_team == "Saint Francis (PA)") ~ "Saint Francis (PA)",
      point_won_by == "Saint Mary's" & (away_team == "Saint Mary's (CA)" | home_team == "Saint Mary's (CA)") ~ "Saint Mary's (CA)",
      point_won_by == "St. John's" & (away_team == "St. John's (NY)" | home_team == "St. John's (NY)") ~ "St. John's (NY)",
      point_won_by == "St. Thomas" & (away_team == "St. Thomas (MN)" | home_team == "St. Thomas (MN)") ~ "St. Thomas (MN)",
      TRUE ~ point_won_by
    ))
  }

  # Play by play code
  vb_play_by_play <- function(pbp_url){

    pbp1 <- vb_play_by_play_ncaa(pbp_url)

    if (is.null(pbp1) || nrow(pbp1) == 0) {  # some weirdness with nulls
      pbp2 <- vb_play_by_play_ncaa2(pbp_url) # if pbp1 doesn't work try pbp2
      if (is.null(pbp2) || nrow(pbp2) == 0) {
        return(NULL)  # if neither works return NULL
      } else {
        return(pbp2)
      }
    } else {
      return(pbp1)
    }
  }  # Wrapper function because NCAA is not consistent with how the play-by-play is formatted

  skills_regex <- "serve|Reception|Block|Set|Attack|Freeball|Dig"
  skills_removed <- " serves|Reception by |Block by |Set by |Attack by |Dig by | serves |Set error |Block error "

  # This is the cleaner way the play-by-play is formatted
  vb_play_by_play_ncaa <- function(pbp_url){

    game_info <- read_html(pbp_url) %>% html_nodes(".mytable") %>% html_table(fill = TRUE)
    match_id <- str_remove(pbp_url, "https://stats.ncaa.org/game/play_by_play/")

    sets <- bind_rows(game_info[-c(1:2)])

    if (nrow(sets) == 0) {
      return(NULL)
    }   ## return NULL if there are no sets in the file, I'm hoping this fixes the issues

    teams <- c(sets$X1[1], sets$X3[1]) %>% str_squish()

    ## Step 1: Add set number, skill type, away and home scores, serving team
    sets <- sets %>% mutate(
      set_number = cumsum(X1 == "Set started") + cumsum(X3 == "Set started"),
      skill = if_else(
        nchar(X1) > 0, str_extract(X1, skills_regex), str_extract(X3, skills_regex)
      ) %>% str_to_title(),
      team = if_else(
        nchar(X1) > 0, X1[1], X3[1]
      ),
      player_name = if_else(
        nchar(X1) > 0, str_remove(X1, skills_removed), str_remove(X3, skills_removed)
      ) %>% str_squish(),
      away_score = suppressWarnings(as.numeric(str_split_fixed(X2, "-", 2)[,1])),  ## Warning here since Score will be non-numeric
      home_score = suppressWarnings(as.numeric(str_split_fixed(X2, "-", 2)[,2])),
      serving_team = if_else(skill == "Serve", team, NA_character_),
      away_team = teams[1],
      home_team = teams[2],
      opponent = if_else(team == away_team, home_team, away_team)
    )

    # Step 2: add point won by and fill in all the missing information
    sets <- sets %>% tidyr::fill(away_score, home_score, .direction = "up") %>%
      mutate(point_won_by = case_when(
        away_score == 1 & home_score == 0 ~ X1[1],
        away_score == 0 & home_score == 1 ~ X3[1],
        away_score == (lag(away_score) + 1) ~ X1[1],
        home_score == (lag(home_score) + 1) ~ X3[1],
        TRUE ~ NA_character_),
        match_id = match_id) %>%
      tidyr::fill(point_won_by, serving_team, .direction = "down")

    # Step 3: filter to get skills
    skills <- sets %>% filter(!is.na(skill), X1 != "Set started", X3 != "Set started", !str_detect(X1, "\\+|End of"), !str_detect(X3, "\\+|End of")) %>% select(-X1, -X2, -X3) %>%
      group_by(set_number, match_id) %>%
      mutate(point_id = cumsum(skill == "Serve")) %>%
      ungroup() %>%
      rename(ncaa_match_id = match_id)

    return(skills)
  }

  # This is the messier way
  vb_play_by_play_ncaa2 <- function(pbp_url){

    game_info <- read_html(pbp_url) %>% html_nodes(".mytable") %>% html_table(fill = TRUE)
    match_id <- str_remove(pbp_url, "https://stats.ncaa.org/game/play_by_play/")

    sets <- bind_rows(game_info[-c(1:2)])

    if (nrow(sets) == 0) {
      return(NULL)
    }   ## return NULL if there are no sets in the file, I'm hoping this fixes the issues

    teams <- str_split(sets$X1[1], "-") %>% unlist() %>% str_squish() %>% fix_wncaa_names()
    # Problem here: if team has hyphenated name, we may not get the correct split

    set_starters <- list(starters_away = character((length(game_info) - 2)),
                         starters_home = character((length(game_info) - 2))
    )
    for(i in 3:length(game_info)){
      set_starters[[1]][(i-2)] <- game_info[[i]]$X2[2] %>% str_replace_all("3a", " ")
      set_starters[[2]][(i-2)] <- game_info[[i]]$X2[3] %>% str_replace_all("3a", " ")
    }
    set_starters <- unlist(set_starters)

    starters_away <- paste(set_starters[str_detect(set_starters, teams[1])], collapse = " ")
    starters_home <- paste(set_starters[str_detect(set_starters, teams[2])], collapse = " ")


    ## Step 1: Add set number, skill type, away and home scores, serving team
    sets <- sets %>% filter(nchar(X1) > 0) %>%
      mutate(
        set_number = cumsum(str_detect(X1, "End of")) + 1,
        skill = "Serve",
        player_name = str_extract(X2, ": \\(.+\\)\\s") %>% str_remove(": \\(") %>% str_remove("\\)") %>% str_squish(),
        away_score = suppressWarnings(as.numeric(str_split_fixed(X1, "-", 2)[,1] %>% str_squish())),
        home_score = suppressWarnings(as.numeric(str_split_fixed(X1, "-", 2)[,2] %>% str_squish())),
        point_won_by = str_extract(X2, "Point .+:") %>% str_remove("Point ") %>% str_remove(":") %>% str_remove(" \\(.+"),
        serving_team = lag(point_won_by),
        serving_team = if_else(
          is.na(serving_team),
          case_when(
            str_detect(starters_away, player_name) ~ teams[1],
            str_detect(starters_home, player_name) ~ teams[2],
            TRUE ~ NA_character_
          ),
          serving_team),
        serving_team = if_else(is.na(serving_team),
                               first(serving_team[which(player_name == eval(player_name))] %>% na.omit()),
                               serving_team),  # yes, we need 3 separate mutates here to deal with wackiness
        serving_team = fix_wncaa_names(serving_team),
        team = serving_team,
        point_won_by = fix_wncaa_names(point_won_by),
        away_team = teams[1],
        match_id = match_id,
        home_team = teams[2],
        opponent = if_else(team == away_team, home_team, away_team)
      ) %>%
      filter(!is.na(away_score))

    # Step 3: filter to get skills
    skills <- sets %>% filter(!is.na(skill), X1 != "Set started", X3 != "Set started", !str_detect(X1, "\\+|End of"), !str_detect(X3, "\\+|End of")) %>% select(-X1, -X2, -X3) %>%
      group_by(set_number, match_id) %>%
      mutate(point_id = cumsum(skill == "Serve")) %>%
      ungroup() %>%
      rename(ncaa_match_id = match_id)

    return(skills)
  }

  all_pbp <- map(pbp_url, vb_play_by_play)

  all_pbp_df <- bind_rows(all_pbp) %>% fill(away_score, home_score, .direction = "down") %>%
    filter(!(player_name %in% c("Set end", "Set ended")))

  all_pbp_df <- all_pbp_df %>% mutate(id = seq(1, nrow(all_pbp_df)))

  # Now we merge with the data frame containing the links
  full_pbp <- all_pbp_df %>%
    left_join(game_pbp %>% select(game_id, location, box_score, play_by_play), by = c("ncaa_match_id" = "game_id")) %>%
    mutate(ncaa_match_id = as.numeric(ncaa_match_id),
           player_name = if_else(grepl(' by ', player_name), str_remove(player_name, skills_removed), player_name))



  return(full_pbp)
}

