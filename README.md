
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

## Example

Get NCAA women’s volleyball teams.

``` r
library(ncaavolleyballR)
ncaavolleyballR::ncaa_wvolleyball_teams()
#> # A tibble: 361 × 3
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
#> # ℹ 351 more rows
```

Get NCAA women’s volleyball schedules.

``` r
ncaavolleyballR::ncaa_wvolleyball_schedule('Yale', 813, 2019)
#> # A tibble: 23 × 5
#>    url                                  date                home  away  location
#>    <chr>                                <dttm>              <chr> <chr> <chr>   
#>  1 https://stats.ncaa.org/contests/175… 2019-09-06 00:00:00 Okla… Yale  Bloomin…
#>  2 https://stats.ncaa.org/contests/175… 2019-09-06 00:00:00 Oreg… Yale  Bloomin…
#>  3 https://stats.ncaa.org/contests/175… 2019-09-08 00:00:00 Indi… Yale  Indiana 
#>  4 https://stats.ncaa.org/contests/175… 2019-09-13 00:00:00 Vill… Yale  Los Ang…
#>  5 https://stats.ncaa.org/contests/175… 2019-09-13 00:00:00 Sout… Yale  Souther…
#>  6 https://stats.ncaa.org/contests/176… 2019-09-14 00:00:00 Howa… Yale  Los Ang…
#>  7 https://stats.ncaa.org/contests/176… 2019-09-20 00:00:00 Yale  Ston… Yale    
#>  8 https://stats.ncaa.org/contests/176… 2019-09-21 00:00:00 Yale  Army… Yale    
#>  9 https://stats.ncaa.org/contests/176… 2019-09-21 00:00:00 Yale  Sacr… Yale    
#> 10 https://stats.ncaa.org/contests/176… 2019-09-28 00:00:00 Yale  Brown Yale    
#> # ℹ 13 more rows
```

``` r
ncaavolleyballR::ncaa_wvolleyball_conf_schedule('Big Ten', 2021)
#> # A tibble: 306 × 5
#>    url                                  date                home  away  location
#>    <chr>                                <dttm>              <chr> <chr> <chr>   
#>  1 https://stats.ncaa.org/contests/210… 2021-08-27 00:00:00 UC S… Illi… Milwauk…
#>  2 https://stats.ncaa.org/contests/210… 2021-08-27 00:00:00 Milw… Illi… Milwauk…
#>  3 https://stats.ncaa.org/contests/210… 2021-08-27 00:00:00 UIC   Indi… Indiana…
#>  4 https://stats.ncaa.org/contests/210… 2021-08-27 00:00:00 Mary… Virg… Maryland
#>  5 https://stats.ncaa.org/contests/210… 2021-08-27 00:00:00 LSU   Mich… LSU     
#>  6 https://stats.ncaa.org/contests/210… 2021-08-27 00:00:00 Mich… West… Michiga…
#>  7 https://stats.ncaa.org/contests/210… 2021-08-27 00:00:00 Bayl… Minn… Madison…
#>  8 https://stats.ncaa.org/contests/210… 2021-08-27 00:00:00 Nebr… Colg… Nebraska
#>  9 https://stats.ncaa.org/contests/210… 2021-08-27 00:00:00 Nort… Nort… Norther…
#> 10 https://stats.ncaa.org/contests/210… 2021-08-27 00:00:00 Ohio… Nort… Ohio St.
#> # ℹ 296 more rows
```
