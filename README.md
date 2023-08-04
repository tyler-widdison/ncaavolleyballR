
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ncaavolleyballR

<!-- badges: start -->
<!-- badges: end -->

**ncaavolleyballR** is an R package for acquiring NCAA volleyball data.

## Installation

You can install the development version of ncaavolleyballR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tyler-widdison/ncaavolleyballR")
```

## Example (existing functions)

Get NCAA women’s volleyball teams.

``` r
library(ncaavolleyballR)
ncaavolleyballR::ncaa_wvolleyball_teams()
#> # A tibble: 366 × 3
#>    Name               Conference    ID   
#>    <chr>              <chr>         <chr>
#>  1 A&M-Corpus Christi Southland     26172
#>  2 Abilene Christian  WAC           2    
#>  3 Air Force          Mountain West 721  
#>  4 Akron              MAC           5    
#>  5 Alabama            SEC           8    
#>  6 Alabama A&M        SWAC          6    
#>  7 Alabama St.        SWAC          7    
#>  8 Alcorn             SWAC          17   
#>  9 American           Patriot       23   
#> 10 App State          Sun Belt      27   
#> # ℹ 356 more rows
```

Get NCAA women’s volleyball schedules.

``` r
ncaavolleyballR::ncaa_wvolleyball_schedule('Yale', 813, 2017)
#> # A tibble: 24 × 5
#>    url                                  date                home  away  location
#>    <chr>                                <dttm>              <chr> <chr> <chr>   
#>  1 https://stats.ncaa.org/contests/646… 2017-09-01 00:00:00 Yale  Dela… Yale    
#>  2 https://stats.ncaa.org/contests/654… 2017-09-02 00:00:00 Yale  Rhod… Yale    
#>  3 https://stats.ncaa.org/contests/654… 2017-09-02 00:00:00 Yale  Clem… Yale    
#>  4 https://stats.ncaa.org/contests/670… 2017-09-08 00:00:00 Arka… Yale  Santa B…
#>  5 https://stats.ncaa.org/contests/671… 2017-09-08 00:00:00 UC S… Yale  UC Sant…
#>  6 https://stats.ncaa.org/contests/678… 2017-09-09 00:00:00 Sout… Yale  Santa B…
#>  7 https://stats.ncaa.org/contests/686… 2017-09-15 00:00:00 Penn… Yale  Penn St.
#>  8 https://stats.ncaa.org/contests/695… 2017-09-16 00:00:00 Wake… Yale  Univers…
#>  9 https://stats.ncaa.org/contests/696… 2017-09-16 00:00:00 Ohio  Yale  Univers…
#> 10 https://stats.ncaa.org/contests/703… 2017-09-22 00:00:00 Yale  Brown Yale    
#> # ℹ 14 more rows
```

``` r
ncaavolleyballR::ncaa_wvolleyball_conf_schedule('Big Ten', 2022)
#> # A tibble: 303 × 5
#>    url                                  date                home  away  location
#>    <chr>                                <dttm>              <chr> <chr> <chr>   
#>  1 https://stats.ncaa.org/contests/229… 2022-08-26 00:00:00 Indi… Indi… Indiana 
#>  2 https://stats.ncaa.org/contests/229… 2022-08-26 00:00:00 Gonz… Iowa  Norman,…
#>  3 https://stats.ncaa.org/contests/229… 2022-08-26 00:00:00 FIU   Iowa  Norman,…
#>  4 https://stats.ncaa.org/contests/229… 2022-08-26 00:00:00 Mary… Rhod… Maryland
#>  5 https://stats.ncaa.org/contests/229… 2022-08-26 00:00:00 Mary… Navy  Maryland
#>  6 https://stats.ncaa.org/contests/229… 2022-08-26 00:00:00 Old … Mich… Flagsta…
#>  7 https://stats.ncaa.org/contests/229… 2022-08-26 00:00:00 Nort… Mich… Norther…
#>  8 https://stats.ncaa.org/contests/229… 2022-08-26 00:00:00 Mich… Loui… Michiga…
#>  9 https://stats.ncaa.org/contests/229… 2022-08-26 00:00:00 Bayl… Minn… Fort Wo…
#> 10 https://stats.ncaa.org/contests/229… 2022-08-26 00:00:00 Nebr… A&M-… Nebraska
#> # ℹ 293 more rows
```

Get NCAA women’s box score, single team for entire season.

``` r
ncaavolleyballR::ncaa_wvolleyball_boxscore('BYU', 2015)
#> Joining with `by = join_by(date, game_id)`
#> # A tibble: 675 × 32
#>    Player Pos       S MP    Kills Errors `Total Attacks` `Hit Pct` Assists  Aces
#>    <chr>  <chr> <dbl> <chr> <dbl>  <dbl>           <dbl>     <dbl>   <dbl> <dbl>
#>  1 Davis… ""        3 1         0      1               5    -0.2        21     0
#>  2 Jacks… ""        3 1         3      4              15    -0.067       0     2
#>  3 Kunz,… ""        3 1         2      0               5     0.4         0     1
#>  4 Staen… ""        3 1         1      3               9    -0.222       0     0
#>  5 Vaima… ""        3 1        11      3              19     0.421       0     0
#>  6 Woino… ""        3 1         5      6              21    -0.048       2     0
#>  7 Addis… ""        3 1         0      1               1    -1           1     0
#>  8 Stewa… ""        3 1         5      0               8     0.625       2     0
#>  9 Mille… ""        1 1         0      2               2    -1           0     0
#> 10 Kaka,… ""        3 1         0      0               0    NA           0     1
#> # ℹ 665 more rows
#> # ℹ 22 more variables: SErr <dbl>, Digs <dbl>, RErr <dbl>, `Block Solos` <dbl>,
#> #   `Block Assists` <dbl>, BErr <dbl>, PTS <dbl>, BHE <dbl>, team <tibble[,1]>,
#> #   RetAtt <dbl>, TB <dbl>, opponent <tibble[,1]>, date <dttm>, game_id <chr>,
#> #   away <chr>, home <chr>, away_sets <dbl>, home_sets <dbl>,
#> #   away_points <dbl>, home_points <dbl>, away_pointpct <dbl>,
#> #   home_pointpct <dbl>
```

Get NCAA women’s play by play, single team for entire season.

``` r
ncaavolleyballR::ncaa_wvolleyball_pbp('BYU', 2021)
#> # A tibble: 22,611 × 17
#>    set_number skill     team      player_name away_score home_score serving_team
#>         <int> <chr>     <chr>     <chr>            <dbl>      <dbl> <chr>       
#>  1          1 Serve     Southern… Corrin Pet…          0          1 Southern Ut…
#>  2          1 Reception BYU       Gretchen R…          0          1 Southern Ut…
#>  3          1 Set       BYU       Whitney Bo…          0          1 Southern Ut…
#>  4          1 Attack    BYU       Kenzie Koe…          0          1 Southern Ut…
#>  5          1 Dig       Southern… Isabella S…          0          1 Southern Ut…
#>  6          1 Set       Southern… by Corrin …          0          1 Southern Ut…
#>  7          1 Serve     BYU       Tayler Tau…          0          2 BYU         
#>  8          1 Serve     BYU       Tayler Tau…          0          3 BYU         
#>  9          1 Reception Southern… Andreanna …          0          3 BYU         
#> 10          1 Set       BYU       Whitney Bo…          0          3 BYU         
#> # ℹ 22,601 more rows
#> # ℹ 10 more variables: away_team <chr>, home_team <chr>, opponent <chr>,
#> #   point_won_by <chr>, ncaa_match_id <dbl>, point_id <int>, id <int>,
#> #   location <chr>, box_score <chr>, play_by_play <chr>
```
