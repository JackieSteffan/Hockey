Hockey Project
================
Jackie Steffan
9/11/2020

``` r
render("hockey.md", output_file = "README.md")
```

# Required Packages

Load in the following packages: `rmarkdown`, `tidyverse`, `devtools`,
`httr`, `jsonlite`, `ggplot2`

# Create Functions

``` r
franchise <- function(team, type) {
  franchise_url <- paste0("https://records.nhl.com/site/api/franchise")
  get_franchise <- GET(franchise_url)
  txt_franchise <- content(get_franchise, "text")
  json_franchise <- fromJSON(txt_franchise, flatten=TRUE)
  if (missing(team)) {
   return(as_tibble(json_franchise$data))
    }
  else {
      filtered<-switch(type, 
                   num = (json_franchise$data) %>% filter(id == team),
                   char = (json_franchise$data) %>% filter(teamCommonName == team))
      }
  return(as_tibble(filtered))
}

franchise("Wild", "char")
```

    ## No encoding supplied: defaulting to UTF-8.

    ## # A tibble: 1 x 6
    ##      id firstSeasonId lastSeasonId mostRecentTeamId teamCommonName teamPlaceName
    ##   <int>         <int>        <int>            <int> <chr>          <chr>        
    ## 1    37      20002001           NA               30 Wild           Minnesota

``` r
franchise_team <- function(team, type) {
  team_url <- paste0("https://records.nhl.com/site/api/franchise-team-totals")
  get_franchise_team <- GET(team_url)
  txt_fran_team <- content(get_franchise_team, "text")
  json_fran_team <- fromJSON(txt_fran_team, flatten = TRUE)
  if (missing(team)) {
   return(as_tibble(json_fran_team$data))
    }
  else {filtered <- switch(type, 
                           num = (json_fran_team$data) %>% filter(franchiseId == team),
                           char = (json_fran_team$data) %>% filter(teamName == team))
  }
  return(as_tibble(filtered))
  }

franchise_team(15, "num")
```

    ## No encoding supplied: defaulting to UTF-8.

    ## # A tibble: 4 x 30
    ##      id activeFranchise firstSeasonId franchiseId gameTypeId gamesPlayed
    ##   <int>           <int>         <int>       <int>      <int>       <int>
    ## 1    49               1      19931994          15          2        2053
    ## 2    50               1      19931994          15          3         194
    ## 3    61               1      19671968          15          2        2062
    ## 4    62               1      19671968          15          3         166
    ## # ... with 24 more variables: goalsAgainst <int>, goalsFor <int>,
    ## #   homeLosses <int>, homeOvertimeLosses <int>, homeTies <int>, homeWins <int>,
    ## #   lastSeasonId <int>, losses <int>, overtimeLosses <int>,
    ## #   penaltyMinutes <int>, pointPctg <dbl>, points <int>, roadLosses <int>,
    ## #   roadOvertimeLosses <int>, roadTies <int>, roadWins <int>,
    ## #   shootoutLosses <int>, shootoutWins <int>, shutouts <int>, teamId <int>,
    ## #   teamName <chr>, ties <int>, triCode <chr>, wins <int>

``` r
season_record <- function(ID){
  season_url <- paste0("https://records.nhl.com/site/api/franchise-season-records?cayenneExp=franchiseId=", ID)
  get_season <- GET(season_url)
  txt_season <- content(get_season, "text")
  json_season <- fromJSON(txt_season, flatten = TRUE)
  return(as_tibble(json_season$data))
}

season_record(ID= 24)  
```

    ## No encoding supplied: defaulting to UTF-8.

    ## # A tibble: 1 x 57
    ##      id fewestGoals fewestGoalsAgai~ fewestGoalsAgai~ fewestGoalsSeas~
    ##   <int>       <int>            <int> <chr>            <chr>           
    ## 1    15         181              182 2016-17 (82)     1974-75 (80)    
    ## # ... with 52 more variables: fewestLosses <int>, fewestLossesSeasons <chr>,
    ## #   fewestPoints <int>, fewestPointsSeasons <chr>, fewestTies <int>,
    ## #   fewestTiesSeasons <chr>, fewestWins <int>, fewestWinsSeasons <chr>,
    ## #   franchiseId <int>, franchiseName <chr>, homeLossStreak <int>,
    ## #   homeLossStreakDates <chr>, homePointStreak <int>,
    ## #   homePointStreakDates <chr>, homeWinStreak <int>, homeWinStreakDates <chr>,
    ## #   homeWinlessStreak <int>, homeWinlessStreakDates <chr>, lossStreak <int>,
    ## #   lossStreakDates <chr>, mostGameGoals <int>, mostGameGoalsDates <chr>,
    ## #   mostGoals <int>, mostGoalsAgainst <int>, mostGoalsAgainstSeasons <chr>,
    ## #   mostGoalsSeasons <chr>, mostLosses <int>, mostLossesSeasons <chr>,
    ## #   mostPenaltyMinutes <int>, mostPenaltyMinutesSeasons <chr>,
    ## #   mostPoints <int>, mostPointsSeasons <chr>, mostShutouts <int>,
    ## #   mostShutoutsSeasons <chr>, mostTies <int>, mostTiesSeasons <chr>,
    ## #   mostWins <int>, mostWinsSeasons <chr>, pointStreak <int>,
    ## #   pointStreakDates <chr>, roadLossStreak <int>, roadLossStreakDates <chr>,
    ## #   roadPointStreak <int>, roadPointStreakDates <chr>, roadWinStreak <int>,
    ## #   roadWinStreakDates <chr>, roadWinlessStreak <int>,
    ## #   roadWinlessStreakDates <chr>, winStreak <int>, winStreakDates <chr>,
    ## #   winlessStreak <lgl>, winlessStreakDates <lgl>

``` r
goalie_record <- function(ID){
  goalie_url <- paste0("https://records.nhl.com/site/api/franchise-goalie-records?cayenneExp=franchiseId=", ID)
  get_goalie <- GET(goalie_url)
  txt_goalie <- content(get_goalie, "text")
  json_goalie <- fromJSON(txt_goalie, flatten = TRUE)
  return(as_tibble(json_goalie$data))
}

goalie_record(24)
```

    ## No encoding supplied: defaulting to UTF-8.

    ## # A tibble: 30 x 29
    ##       id activePlayer firstName franchiseId franchiseName gameTypeId gamesPlayed
    ##    <int> <lgl>        <chr>           <int> <chr>              <int>       <int>
    ##  1   241 FALSE        Olie               24 Washington C~          2         711
    ##  2   326 TRUE         Braden             24 Washington C~          2         468
    ##  3   339 FALSE        Don                24 Washington C~          2         269
    ##  4   357 FALSE        Craig              24 Washington C~          2          47
    ##  5   468 FALSE        Gary               24 Washington C~          2          54
    ##  6   489 FALSE        Mike               24 Washington C~          2          64
    ##  7   494 FALSE        Ron                24 Washington C~          2         145
    ##  8   498 FALSE        Clint              24 Washington C~          2          96
    ##  9   564 FALSE        Roger              24 Washington C~          2           3
    ## 10   648 FALSE        Mike               24 Washington C~          2          60
    ## # ... with 20 more rows, and 22 more variables: lastName <chr>, losses <int>,
    ## #   mostGoalsAgainstDates <chr>, mostGoalsAgainstOneGame <int>,
    ## #   mostSavesDates <chr>, mostSavesOneGame <int>, mostShotsAgainstDates <chr>,
    ## #   mostShotsAgainstOneGame <int>, mostShutoutsOneSeason <int>,
    ## #   mostShutoutsSeasonIds <chr>, mostWinsOneSeason <int>,
    ## #   mostWinsSeasonIds <chr>, overtimeLosses <int>, playerId <int>,
    ## #   positionCode <chr>, rookieGamesPlayed <int>, rookieShutouts <int>,
    ## #   rookieWins <int>, seasons <int>, shutouts <int>, ties <int>, wins <int>

``` r
skater_record <- function(ID){
  skater_url <- paste0("https://records.nhl.com/site/api/franchise-skater-records?cayenneExp=franchiseId=", ID)
  get_skater <- GET(skater_url)
  txt_skater <- content(get_skater, "text")
  json_skater <- fromJSON(txt_skater, flatten = TRUE)
  return(as_tibble(json_skater$data))
}

skater_record(24)
```

    ## No encoding supplied: defaulting to UTF-8.

    ## # A tibble: 509 x 30
    ##       id activePlayer assists firstName franchiseId franchiseName gameTypeId
    ##    <int> <lgl>          <int> <chr>           <int> <chr>              <int>
    ##  1 16910 FALSE            361 Calle              24 Washington C~          2
    ##  2 16982 TRUE             572 Alex               24 Washington C~          2
    ##  3 17011 TRUE             684 Nicklas            24 Washington C~          2
    ##  4 17021 FALSE            375 Dale               24 Washington C~          2
    ##  5 17075 FALSE            249 Dennis             24 Washington C~          2
    ##  6 17106 FALSE             42 Alan               24 Washington C~          2
    ##  7 17158 FALSE            392 Mike               24 Washington C~          2
    ##  8 17179 FALSE            259 Larry              24 Washington C~          2
    ##  9 17250 FALSE              0 Keith              24 Washington C~          2
    ## 10 17255 FALSE             98 Greg               24 Washington C~          2
    ## # ... with 499 more rows, and 23 more variables: gamesPlayed <int>,
    ## #   goals <int>, lastName <chr>, mostAssistsGameDates <chr>,
    ## #   mostAssistsOneGame <int>, mostAssistsOneSeason <int>,
    ## #   mostAssistsSeasonIds <chr>, mostGoalsGameDates <chr>,
    ## #   mostGoalsOneGame <int>, mostGoalsOneSeason <int>, mostGoalsSeasonIds <chr>,
    ## #   mostPenaltyMinutesOneSeason <int>, mostPenaltyMinutesSeasonIds <chr>,
    ## #   mostPointsGameDates <chr>, mostPointsOneGame <int>,
    ## #   mostPointsOneSeason <int>, mostPointsSeasonIds <chr>, penaltyMinutes <int>,
    ## #   playerId <int>, points <int>, positionCode <chr>, rookiePoints <int>,
    ## #   seasons <int>

``` r
hockey_stats <- function(endpoint, team){
  stats_url <- paste0("https://statsapi.web.nhl.com/api/v1/teams", endpoint)
  get_stats <- GET(stats_url)
  txt_stats <- content(get_stats, "text")
  json_stats <- fromJSON(txt_stats, flatten = TRUE)
  names <- json_stats$teams$name
  if (endpoint == "?expand=team.roster") {
  filtered <- list(json_stats$teams[["roster.roster"]], .name_repair = "unique")
  }
  else if (endpoint == "?expand=person.names"){
    filtered <- as_tibble(data.frame(json_stats$teams))
  }
  else if (endpoint == "?expand=team.schedule.next") {
    filtered <- list(json_stats$teams[["nextGameSchedule.dates"]])
  }
  else if (endpoint == "?expand=team.schedule.previous") {
    filtered <- list(json_stats$teams[["previousGameSchedule.dates"]])
  }
  else if (endpoint == "?expand=team.stats"){
    filtered <- list(json_stats$teams$teamStats[["splits"]])
  }
  else if (endpoint == "?expand=team.roster&season=20142015") {
    filtered <- list(json_stats$teams[["roster.roster"]])
  }
  else if (endpoint == "?teamId=4,5,29") {
    filtered <- list(json_stats$teams)
  }
  else {
    filtered <- list(json_stats$teams)
  }
  return(list(filtered))
  }


hockey_stats("?expand=team.schedule.next")
```

    ## [[1]]
    ## [[1]][[1]]
    ## [[1]][[1]][[1]]
    ## NULL
    ## 
    ## [[1]][[1]][[2]]
    ## NULL
    ## 
    ## [[1]][[1]][[3]]
    ## NULL
    ## 
    ## [[1]][[1]][[4]]
    ## NULL
    ## 
    ## [[1]][[1]][[5]]
    ## NULL
    ## 
    ## [[1]][[1]][[6]]
    ## NULL
    ## 
    ## [[1]][[1]][[7]]
    ## NULL
    ## 
    ## [[1]][[1]][[8]]
    ## NULL
    ## 
    ## [[1]][[1]][[9]]
    ## NULL
    ## 
    ## [[1]][[1]][[10]]
    ## NULL
    ## 
    ## [[1]][[1]][[11]]
    ## NULL
    ## 
    ## [[1]][[1]][[12]]
    ## NULL
    ## 
    ## [[1]][[1]][[13]]
    ##         date totalItems totalEvents totalGames totalMatches
    ## 1 2020-09-19          1           0          1            0
    ##                                                                                                                                                                                                                                                                                                                  games
    ## 1 2019030411, /api/v1/game/2019030411/feed/live, P, 20192020, 2020-09-19T23:30:00Z, Preview, 1, Scheduled, 1, FALSE, 0, 13, 8, 0, league, 25, Dallas Stars, /api/v1/teams/25, 0, 14, 5, 0, league, 14, Tampa Bay Lightning, /api/v1/teams/14, 5100, Rogers Place, /api/v1/venues/5100, /api/v1/game/2019030411/content
    ##   events matches
    ## 1   NULL    NULL
    ## 
    ## [[1]][[1]][[14]]
    ## NULL
    ## 
    ## [[1]][[1]][[15]]
    ## NULL
    ## 
    ## [[1]][[1]][[16]]
    ## NULL
    ## 
    ## [[1]][[1]][[17]]
    ## NULL
    ## 
    ## [[1]][[1]][[18]]
    ## NULL
    ## 
    ## [[1]][[1]][[19]]
    ## NULL
    ## 
    ## [[1]][[1]][[20]]
    ## NULL
    ## 
    ## [[1]][[1]][[21]]
    ## NULL
    ## 
    ## [[1]][[1]][[22]]
    ## NULL
    ## 
    ## [[1]][[1]][[23]]
    ## NULL
    ## 
    ## [[1]][[1]][[24]]
    ##         date totalItems totalEvents totalGames totalMatches
    ## 1 2020-09-19          1           0          1            0
    ##                                                                                                                                                                                                                                                                                                                  games
    ## 1 2019030411, /api/v1/game/2019030411/feed/live, P, 20192020, 2020-09-19T23:30:00Z, Preview, 1, Scheduled, 1, FALSE, 0, 13, 8, 0, league, 25, Dallas Stars, /api/v1/teams/25, 0, 14, 5, 0, league, 14, Tampa Bay Lightning, /api/v1/teams/14, 5100, Rogers Place, /api/v1/venues/5100, /api/v1/game/2019030411/content
    ##   events matches
    ## 1   NULL    NULL
    ## 
    ## [[1]][[1]][[25]]
    ## NULL
    ## 
    ## [[1]][[1]][[26]]
    ## NULL
    ## 
    ## [[1]][[1]][[27]]
    ## NULL
    ## 
    ## [[1]][[1]][[28]]
    ## NULL
    ## 
    ## [[1]][[1]][[29]]
    ## NULL
    ## 
    ## [[1]][[1]][[30]]
    ## NULL
    ## 
    ## [[1]][[1]][[31]]
    ## NULL

# Wrapper

``` r
wrapper <- function(endpoint, ...) {
  if (endpoint == "/franchise" ) {
    franchise()
  }
  else if (endpoint == "/franchise-team-totals") {
    franchise_team()
  }
  else if (endpoint == "/franchise-season-records?cayenneExp=franchiseId=") {
    season_record(...)
  }
  else if (endpoint == "/franchise-goalie-records?cayenneExp=franchiseId=") {
    goalie_record(...)
  }
  else if (endpoint == "/franchise-skater-records?cayenneExp=franchiseId=") {
    skater_record(...)
  }
  else {
    hockey_stats()
  }
}

wrapper("/franchise")
```

    ## No encoding supplied: defaulting to UTF-8.

    ## # A tibble: 38 x 6
    ##       id firstSeasonId lastSeasonId mostRecentTeamId teamCommonName
    ##    <int>         <int>        <int>            <int> <chr>         
    ##  1     1      19171918           NA                8 Canadiens     
    ##  2     2      19171918     19171918               41 Wanderers     
    ##  3     3      19171918     19341935               45 Eagles        
    ##  4     4      19191920     19241925               37 Tigers        
    ##  5     5      19171918           NA               10 Maple Leafs   
    ##  6     6      19241925           NA                6 Bruins        
    ##  7     7      19241925     19371938               43 Maroons       
    ##  8     8      19251926     19411942               51 Americans     
    ##  9     9      19251926     19301931               39 Quakers       
    ## 10    10      19261927           NA                3 Rangers       
    ## # ... with 28 more rows, and 1 more variable: teamPlaceName <chr>

``` r
wrapper("/franchise-season-records?cayenneExp=franchiseId=", 24)
```

    ## No encoding supplied: defaulting to UTF-8.

    ## # A tibble: 1 x 57
    ##      id fewestGoals fewestGoalsAgai~ fewestGoalsAgai~ fewestGoalsSeas~
    ##   <int>       <int>            <int> <chr>            <chr>           
    ## 1    15         181              182 2016-17 (82)     1974-75 (80)    
    ## # ... with 52 more variables: fewestLosses <int>, fewestLossesSeasons <chr>,
    ## #   fewestPoints <int>, fewestPointsSeasons <chr>, fewestTies <int>,
    ## #   fewestTiesSeasons <chr>, fewestWins <int>, fewestWinsSeasons <chr>,
    ## #   franchiseId <int>, franchiseName <chr>, homeLossStreak <int>,
    ## #   homeLossStreakDates <chr>, homePointStreak <int>,
    ## #   homePointStreakDates <chr>, homeWinStreak <int>, homeWinStreakDates <chr>,
    ## #   homeWinlessStreak <int>, homeWinlessStreakDates <chr>, lossStreak <int>,
    ## #   lossStreakDates <chr>, mostGameGoals <int>, mostGameGoalsDates <chr>,
    ## #   mostGoals <int>, mostGoalsAgainst <int>, mostGoalsAgainstSeasons <chr>,
    ## #   mostGoalsSeasons <chr>, mostLosses <int>, mostLossesSeasons <chr>,
    ## #   mostPenaltyMinutes <int>, mostPenaltyMinutesSeasons <chr>,
    ## #   mostPoints <int>, mostPointsSeasons <chr>, mostShutouts <int>,
    ## #   mostShutoutsSeasons <chr>, mostTies <int>, mostTiesSeasons <chr>,
    ## #   mostWins <int>, mostWinsSeasons <chr>, pointStreak <int>,
    ## #   pointStreakDates <chr>, roadLossStreak <int>, roadLossStreakDates <chr>,
    ## #   roadPointStreak <int>, roadPointStreakDates <chr>, roadWinStreak <int>,
    ## #   roadWinStreakDates <chr>, roadWinlessStreak <int>,
    ## #   roadWinlessStreakDates <chr>, winStreak <int>, winStreakDates <chr>,
    ## #   winlessStreak <lgl>, winlessStreakDates <lgl>

# Analytics

``` r
#Join data 
right_join(franchise_team(), goalie_record(24), by= c("teamName" = "franchiseName"))
```

    ## No encoding supplied: defaulting to UTF-8.
    ## No encoding supplied: defaulting to UTF-8.

    ## # A tibble: 60 x 58
    ##     id.x activeFranchise firstSeasonId franchiseId.x gameTypeId.x gamesPlayed.x
    ##    <int>           <int>         <int>         <int>        <int>         <int>
    ##  1    29               1      19741975            24            2          3577
    ##  2    29               1      19741975            24            2          3577
    ##  3    29               1      19741975            24            2          3577
    ##  4    29               1      19741975            24            2          3577
    ##  5    29               1      19741975            24            2          3577
    ##  6    29               1      19741975            24            2          3577
    ##  7    29               1      19741975            24            2          3577
    ##  8    29               1      19741975            24            2          3577
    ##  9    29               1      19741975            24            2          3577
    ## 10    29               1      19741975            24            2          3577
    ## # ... with 50 more rows, and 52 more variables: goalsAgainst <int>,
    ## #   goalsFor <int>, homeLosses <int>, homeOvertimeLosses <int>, homeTies <int>,
    ## #   homeWins <int>, lastSeasonId <int>, losses.x <int>, overtimeLosses.x <int>,
    ## #   penaltyMinutes <int>, pointPctg <dbl>, points <int>, roadLosses <int>,
    ## #   roadOvertimeLosses <int>, roadTies <int>, roadWins <int>,
    ## #   shootoutLosses <int>, shootoutWins <int>, shutouts.x <int>, teamId <int>,
    ## #   teamName <chr>, ties.x <int>, triCode <chr>, wins.x <int>, id.y <int>,
    ## #   activePlayer <lgl>, firstName <chr>, franchiseId.y <int>,
    ## #   gameTypeId.y <int>, gamesPlayed.y <int>, lastName <chr>, losses.y <int>,
    ## #   mostGoalsAgainstDates <chr>, mostGoalsAgainstOneGame <int>,
    ## #   mostSavesDates <chr>, mostSavesOneGame <int>, mostShotsAgainstDates <chr>,
    ## #   mostShotsAgainstOneGame <int>, mostShutoutsOneSeason <int>,
    ## #   mostShutoutsSeasonIds <chr>, mostWinsOneSeason <int>,
    ## #   mostWinsSeasonIds <chr>, overtimeLosses.y <int>, playerId <int>,
    ## #   positionCode <chr>, rookieGamesPlayed <int>, rookieShutouts <int>,
    ## #   rookieWins <int>, seasons <int>, shutouts.y <int>, ties.y <int>,
    ## #   wins.y <int>

``` r
#2 new vars
goalie_record_newVar <- goalie_record(24) %>% mutate(avg_games= gamesPlayed/seasons)
```

    ## No encoding supplied: defaulting to UTF-8.

``` r
skater_record_newVar <- skater_record(24) %>% mutate(avg_games = gamesPlayed/seasons) %>% mutate(goals_season = round(goals/seasons, 0))
```

    ## No encoding supplied: defaulting to UTF-8.

``` r
#contingency tables
table(skater_record_newVar$positionCode, skater_record_newVar$goals)
```

    ##    
    ##      0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 23 24 25
    ##   C 25 10  8  6  9  2  2  3  2  2  3  0  1  3  2  2  0  0  3  0  0  2  1  0  0
    ##   D 54 32 15  9  6  7  5  4  6  5  1  7  3  2  0  1  1  1  1  1  1  0  0  0  2
    ##   L 19  8  6  6  7  5  0  3  3  2  2  2  1  3  2  0  1  0  3  0  1  0  2  1  1
    ##   R 26 10  9  1  2  1  3  1  4  7  0  4  0  1  1  0  0  0  1  0  0  1  4  1  1
    ##    
    ##     26 27 28 29 30 31 32 33 35 36 37 38 40 42 43 45 46 48 49 51 52 53 55 56 57
    ##   C  2  2  1  1  0  1  1  0  1  0  1  1  0  0  1  1  0  0  0  2  0  0  0  0  0
    ##   D  0  0  0  1  0  1  0  0  0  0  1  0  0  1  1  0  0  0  0  0  0  0  0  0  0
    ##   L  3  1  0  1  0  0  0  1  0  1  1  0  0  0  1  0  0  1  1  0  1  0  1  0  0
    ##   R  0  0  1  1  1  1  0  0  0  0  1  0  1  0  1  0  1  0  0  0  1  1  0  1  2
    ##    
    ##     58 59 60 62 65 69 72 73 75 77 78 82 83 86 87 91 98 102 105 110 112 113 114
    ##   C  1  1  0  1  1  0  0  1  0  0  0  0  0  1  1  1  0   1   0   0   0   0   1
    ##   D  1  0  0  0  0  0  0  0  1  0  0  0  0  1  0  0  1   0   1   0   0   2   0
    ##   L  0  0  1  1  1  0  1  0  0  1  0  1  0  0  1  0  0   0   0   0   0   0   0
    ##   R  0  1  1  1  0  1  0  0  0  0  1  0  2  0  0  1  0   0   0   1   1   0   0
    ##    
    ##     118 120 128 129 133 134 140 144 146 149 162 181 182 188 193 195 197 218 243
    ##   C   1   1   0   1   1   0   0   0   0   0   0   2   1   1   0   0   0   1   1
    ##   D   0   0   0   0   0   0   0   1   0   1   0   0   0   0   0   0   0   0   0
    ##   L   0   0   0   0   0   0   0   0   1   0   1   0   0   0   0   1   0   0   0
    ##   R   0   0   1   0   0   1   1   0   0   0   0   0   0   0   1   0   1   0   0
    ##    
    ##     397 472 706
    ##   C   0   0   0
    ##   D   0   0   0
    ##   L   0   0   1
    ##   R   1   1   0

``` r
table(skater_record_newVar$seasons, skater_record_newVar$goals_season)
```

    ##     
    ##       0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
    ##   1  98 31 20  4  9  7  1  4  3  7  1  2  1  2  1  0  0  0  1  0  1  0  0  0  0
    ##   2  37 12 21  6  9  1 12  2  2  1  2  0  7  1  2  1  1  0  0  0  1  0  0  1  0
    ##   3  16 18  4 10  2  2  4  1  1  0  2  0  0  0  2  1  0  1  1  0  1  0  0  0  0
    ##   4   2  4 10  2  2  0  1  1  3  2  0  1  1  0  3  1  2  0  0  0  0  1  0  0  0
    ##   5   0  3  3  3  1  4  2  1  0  0  0  2  3  0  0  0  0  0  1  0  0  0  0  0  1
    ##   6   0  0  2  0  3  1  2  1  1  0  3  0  1  0  3  0  0  0  1  0  0  0  0  0  0
    ##   7   0  0  2  0  2  1  0  1  0  0  2  1  1  1  0  1  0  1  0  0  1  0  0  0  0
    ##   8   0  0  1  0  0  1  0  0  0  0  0  0  1  0  0  0  0  1  0  0  0  0  0  0  0
    ##   9   0  1  1  0  0  1  0  0  0  0  1  0  0  0  0  0  0  0  0  0  0  0  1  0  0
    ##   10  0  0  1  0  0  1  0  0  1  0  0  1  0  0  1  1  0  0  0  0  0  0  0  0  0
    ##   11  0  0  1  0  0  0  0  0  0  0  1  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   12  0  0  0  0  0  0  0  0  0  0  0  1  0  0  0  1  0  0  0  0  0  0  0  0  0
    ##   13  0  0  0  0  0  0  0  0  0  0  0  1  1  0  1  0  0  0  0  1  0  0  0  0  0
    ##   14  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   15  0  0  0  0  0  0  0  0  1  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##     
    ##      26 27 28 34 36 38 40 47
    ##   1   0  0  0  0  0  0  0  0
    ##   2   1  0  0  0  0  1  0  0
    ##   3   0  0  1  0  0  0  0  0
    ##   4   0  0  2  0  0  0  0  0
    ##   5   2  0  0  0  1  0  0  0
    ##   6   0  0  0  0  0  0  0  0
    ##   7   0  1  2  0  0  0  0  0
    ##   8   0  1  0  0  0  0  0  0
    ##   9   0  0  0  0  0  0  0  0
    ##   10  0  0  0  0  0  0  1  0
    ##   11  0  0  0  0  0  0  0  0
    ##   12  0  0  0  0  0  0  0  0
    ##   13  0  0  0  0  0  0  0  0
    ##   14  0  0  0  1  0  0  0  0
    ##   15  0  0  0  0  0  0  0  1

``` r
table(franchise_team()$teamName, franchise_team()$shootoutWins)
```

    ## No encoding supplied: defaulting to UTF-8.
    ## No encoding supplied: defaulting to UTF-8.

    ##                          
    ##                           0 1 10 23 36 37 46 50 51 52 56 58 64 65 67 68 69 70
    ##   Anaheim Ducks           1 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  1  0
    ##   Arizona Coyotes         1 0  0  1  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   Atlanta Flames          2 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   Atlanta Thrashers       1 0  0  0  0  1  0  0  0  0  0  0  0  0  0  0  0  0
    ##   Boston Bruins           1 0  0  0  0  0  0  0  0  0  0  0  1  0  0  0  0  0
    ##   Brooklyn Americans      1 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   Buffalo Sabres          1 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   Calgary Flames          1 0  0  0  0  0  0  0  1  0  0  0  0  0  0  0  0  0
    ##   California Golden Seals 1 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   Carolina Hurricanes     1 0  0  0  0  0  1  0  0  0  0  0  0  0  0  0  0  0
    ##   Chicago Blackhawks      1 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   Cleveland Barons        1 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   Colorado Avalanche      1 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   Colorado Rockies        2 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   Columbus Blue Jackets   1 0  0  0  0  0  0  0  0  0  0  0  0  0  0  1  0  0
    ##   Dallas Stars            0 1  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   Detroit Cougars         2 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   Detroit Falcons         2 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   Detroit Red Wings       1 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  1  0
    ##   Edmonton Oilers         1 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   Florida Panthers        1 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  1
    ##   Hamilton Tigers         1 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   Hartford Whalers        2 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   Kansas City Scouts      1 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   Los Angeles Kings       1 0  0  0  0  0  0  0  0  0  0  0  0  0  0  1  0  0
    ##   Minnesota North Stars   2 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   Minnesota Wild          1 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  1
    ##   Montréal Canadiens      1 0  0  0  0  0  0  0  0  0  0  0  0  0  0  1  0  0
    ##   Montreal Maroons        2 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   Montreal Wanderers      1 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   Nashville Predators     1 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  1  0
    ##   New Jersey Devils       1 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   New York Americans      2 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   New York Islanders      1 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   New York Rangers        1 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   Oakland Seals           2 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   Ottawa Senators         1 0  0  0  0  0  0  0  0  0  1  0  0  0  0  0  0  0
    ##   Ottawa Senators (1917)  2 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   Philadelphia Flyers     1 0  0  0  0  0  0  1  0  0  0  0  0  0  0  0  0  0
    ##   Philadelphia Quakers    1 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   Phoenix Coyotes         1 0  0  0  0  0  0  0  0  1  0  0  0  0  0  0  0  0
    ##   Pittsburgh Penguins     1 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   Pittsburgh Pirates      2 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   Quebec Bulldogs         1 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   Quebec Nordiques        2 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   San Jose Sharks         1 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   St. Louis Blues         1 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   St. Louis Eagles        1 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   Tampa Bay Lightning     0 1  0  0  0  0  0  0  0  0  0  0  0  0  1  0  0  0
    ##   Toronto Arenas          2 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   Toronto Maple Leafs     1 0  0  0  0  0  0  0  0  0  0  1  0  0  0  0  0  0
    ##   Toronto St. Patricks    2 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   Vancouver Canucks       1 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  1  0
    ##   Vegas Golden Knights    1 0  1  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   Washington Capitals     1 0  0  0  0  0  0  0  0  0  0  0  0  1  0  0  0  0
    ##   Winnipeg Jets           1 0  0  0  1  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##   Winnipeg Jets (1979)    2 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ##                          
    ##                           71 72 73 75 77 78 80 82
    ##   Anaheim Ducks            0  0  0  0  0  0  0  0
    ##   Arizona Coyotes          0  0  0  0  0  0  0  0
    ##   Atlanta Flames           0  0  0  0  0  0  0  0
    ##   Atlanta Thrashers        0  0  0  0  0  0  0  0
    ##   Boston Bruins            0  0  0  0  0  0  0  0
    ##   Brooklyn Americans       0  0  0  0  0  0  0  0
    ##   Buffalo Sabres           0  0  0  0  1  0  0  0
    ##   Calgary Flames           0  0  0  0  0  0  0  0
    ##   California Golden Seals  0  0  0  0  0  0  0  0
    ##   Carolina Hurricanes      0  0  0  0  0  0  0  0
    ##   Chicago Blackhawks       0  0  1  0  0  0  0  0
    ##   Cleveland Barons         0  0  0  0  0  0  0  0
    ##   Colorado Avalanche       0  0  1  0  0  0  0  0
    ##   Colorado Rockies         0  0  0  0  0  0  0  0
    ##   Columbus Blue Jackets    0  0  0  0  0  0  0  0
    ##   Dallas Stars             1  0  0  0  0  0  0  0
    ##   Detroit Cougars          0  0  0  0  0  0  0  0
    ##   Detroit Falcons          0  0  0  0  0  0  0  0
    ##   Detroit Red Wings        0  0  0  0  0  0  0  0
    ##   Edmonton Oilers          0  0  0  1  0  0  0  0
    ##   Florida Panthers         0  0  0  0  0  0  0  0
    ##   Hamilton Tigers          0  0  0  0  0  0  0  0
    ##   Hartford Whalers         0  0  0  0  0  0  0  0
    ##   Kansas City Scouts       0  0  0  0  0  0  0  0
    ##   Los Angeles Kings        0  0  0  0  0  0  0  0
    ##   Minnesota North Stars    0  0  0  0  0  0  0  0
    ##   Minnesota Wild           0  0  0  0  0  0  0  0
    ##   Montréal Canadiens       0  0  0  0  0  0  0  0
    ##   Montreal Maroons         0  0  0  0  0  0  0  0
    ##   Montreal Wanderers       0  0  0  0  0  0  0  0
    ##   Nashville Predators      0  0  0  0  0  0  0  0
    ##   New Jersey Devils        0  0  0  0  0  1  0  0
    ##   New York Americans       0  0  0  0  0  0  0  0
    ##   New York Islanders       0  0  0  0  0  0  0  1
    ##   New York Rangers         0  0  0  0  0  1  0  0
    ##   Oakland Seals            0  0  0  0  0  0  0  0
    ##   Ottawa Senators          0  0  0  0  0  0  0  0
    ##   Ottawa Senators (1917)   0  0  0  0  0  0  0  0
    ##   Philadelphia Flyers      0  0  0  0  0  0  0  0
    ##   Philadelphia Quakers     0  0  0  0  0  0  0  0
    ##   Phoenix Coyotes          0  0  0  0  0  0  0  0
    ##   Pittsburgh Penguins      0  0  0  0  0  0  1  0
    ##   Pittsburgh Pirates       0  0  0  0  0  0  0  0
    ##   Quebec Bulldogs          0  0  0  0  0  0  0  0
    ##   Quebec Nordiques         0  0  0  0  0  0  0  0
    ##   San Jose Sharks          1  0  0  0  0  0  0  0
    ##   St. Louis Blues          0  1  0  0  0  0  0  0
    ##   St. Louis Eagles         0  0  0  0  0  0  0  0
    ##   Tampa Bay Lightning      0  0  0  0  0  0  0  0
    ##   Toronto Arenas           0  0  0  0  0  0  0  0
    ##   Toronto Maple Leafs      0  0  0  0  0  0  0  0
    ##   Toronto St. Patricks     0  0  0  0  0  0  0  0
    ##   Vancouver Canucks        0  0  0  0  0  0  0  0
    ##   Vegas Golden Knights     0  0  0  0  0  0  0  0
    ##   Washington Capitals      0  0  0  0  0  0  0  0
    ##   Winnipeg Jets            0  0  0  0  0  0  0  0
    ##   Winnipeg Jets (1979)     0  0  0  0  0  0  0  0

``` r
#numerical summaries for quantitative data
franchise_team() %>% group_by(teamName) %>% summarise(regularandPostWins = sum(wins))
```

    ## No encoding supplied: defaulting to UTF-8.

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 57 x 2
    ##    teamName                regularandPostWins
    ##    <chr>                                <int>
    ##  1 Anaheim Ducks                         1062
    ##  2 Arizona Coyotes                        194
    ##  3 Atlanta Flames                         270
    ##  4 Atlanta Thrashers                      342
    ##  5 Boston Bruins                         3534
    ##  6 Brooklyn Americans                      16
    ##  7 Buffalo Sabres                        1914
    ##  8 Calgary Flames                        1574
    ##  9 California Golden Seals                116
    ## 10 Carolina Hurricanes                    844
    ## # ... with 47 more rows

``` r
skater_record(26) %>% group_by(positionCode) %>% summarise(avgPts = mean(goals))
```

    ## No encoding supplied: defaulting to UTF-8.
    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 4 x 2
    ##   positionCode avgPts
    ##   <chr>         <dbl>
    ## 1 C             29.1 
    ## 2 D              7.95
    ## 3 L             26.1 
    ## 4 R             21.6

``` r
#Bar Plot
skater_position <- ggplot(data = skater_record(34), aes(x= positionCode))
```

    ## No encoding supplied: defaulting to UTF-8.

``` r
skater_position+geom_bar(fill = "#ffb81c", col = "#041e42", size = 2) + labs(x="Position", title= "Number of players for each position for the Nashville Predators") +scale_fill_manual(values=("#ffb81c")) + scale_x_discrete(labels = c("Center", "Defenseman", "Left Wing", "Right Wing"))
```

![](hockey_files/figure-gfm/data%20analysis-1.png)<!-- -->

``` r
#Histogram
goals_scored <- ggplot(data = skater_record(24), aes(x= goals))
```

    ## No encoding supplied: defaulting to UTF-8.

``` r
goals_scored + geom_histogram(fill = "#C8102E", color = "#041E42", size = 2, binwidth = 50) + labs(x= "Goals", title = "Goals Scored by the Washington Capitals")
```

![](hockey_files/figure-gfm/data%20analysis-2.png)<!-- -->

``` r
#Boxplot
skater_position2 <- ggplot(data = skater_record(32), aes(x=positionCode, y=gamesPlayed))
```

    ## No encoding supplied: defaulting to UTF-8.

``` r
skater_position2 + geom_boxplot(fill = "#F47A38", col = "#00685E") + labs(x="Position", y= "Games Played", title = "Box Plots of Games Played by position for the Anaheim Ducks")
```

![](hockey_files/figure-gfm/data%20analysis-3.png)<!-- -->

``` r
skater_position2 +  stat_summary(fun.y = mean, geom = "line", lwd= 1.5, aes(group = activePlayer, col= activePlayer)) + labs(c="Position", y= "Games Played", title = "Mean games played by position for the Anaheim Ducks by active player")
```

    ## Warning: `fun.y` is deprecated. Use `fun` instead.

![](hockey_files/figure-gfm/data%20analysis-4.png)<!-- -->

``` r
#scatterplot
goalie_scat <- ggplot(data = goalie_record_newVar, aes(x=avg_games, y=wins, color = activePlayer))
goalie_scat + geom_point() + labs( x = "Average Games per Season", y = "Total Wins", title = "Total wins by Average games per Season for Washington Capitals Goalies")
```

![](hockey_files/figure-gfm/data%20analysis-5.png)<!-- -->
