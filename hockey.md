Hockey Project
================
Jackie Steffan
9/11/2020

## Required Packages

Load in the following packages: `rmarkdown`, `tidyverse`, `devtools`,
`httr`, `jsonlite`, `ggplot2`

## Create Functions

``` r
franchise <- function(team) {
  franchise_url <- paste0("https://records.nhl.com/site/api/franchise")
  get_franchise <- GET(franchise_url)
  txt_franchise <- content(get_franchise, "text")
  json_franchise <- fromJSON(txt_franchise, flatten=TRUE)
  if (missing(team)) {
   return(as_tibble(json_franchise$data))
    }
  else {
      filtered <- (json_franchise$data) %>% filter(teamCommonName == team)
    }
  return(as_tibble(filtered))
}

franchise("Wild")
```

    ## No encoding supplied: defaulting to UTF-8.

    ## # A tibble: 1 x 6
    ##      id firstSeasonId lastSeasonId mostRecentTeamId teamCommonName teamPlaceName
    ##   <int>         <int>        <int>            <int> <chr>          <chr>        
    ## 1    37      20002001           NA               30 Wild           Minnesota

``` r
franchise_team <- function(team) {
  team_url <- paste0("https://records.nhl.com/site/api/franchise-team-totals")
  get_franchise_team <- GET(team_url)
  txt_fran_team <- content(get_franchise_team, "text")
  json_fran_team <- fromJSON(txt_fran_team, flatten = TRUE)
  if (missing(team)) {
   return(as_tibble(json_fran_team$data))
    }
  else {filtered <- (json_fran_team$data) %>% filter(teamName == team)
  return(as_tibble(filtered))
  }
}

franchise_team("Dallas Stars")
```

    ## No encoding supplied: defaulting to UTF-8.

    ## # A tibble: 2 x 30
    ##      id activeFranchise firstSeasonId franchiseId gameTypeId gamesPlayed
    ##   <int>           <int>         <int>       <int>      <int>       <int>
    ## 1    49               1      19931994          15          2        2053
    ## 2    50               1      19931994          15          3         194
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
    filtered <- list(json_stats$teams[["nextGameSchedule.dates"]], .name_repair = "unique", na.rm = TRUE)
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


hockey_stats("?expand=team.roster")
```

    ## [[1]]
    ## [[1]][[1]]
    ## [[1]][[1]][[1]]
    ##    jerseyNumber person.id     person.fullName            person.link
    ## 1            19   8471233        Travis Zajac /api/v1/people/8471233
    ## 2            35   8471239      Cory Schneider /api/v1/people/8471239
    ## 3            76   8474056         P.K. Subban /api/v1/people/8474056
    ## 4            21   8475151       Kyle Palmieri /api/v1/people/8475151
    ## 5            33   8476368    Fredrik Claesson /api/v1/people/8476368
    ## 6            28   8476923      Damon Severson /api/v1/people/8476923
    ## 7             5   8476941      Connor Carrick /api/v1/people/8476941
    ## 8            97   8477038        Nikita Gusev /api/v1/people/8477038
    ## 9             8   8477355        Will Butcher /api/v1/people/8477355
    ## 10           15   8477401         John Hayden /api/v1/people/8477401
    ## 11           44   8477425          Miles Wood /api/v1/people/8477425
    ## 12           25   8477509       Mirco Mueller /api/v1/people/8477509
    ## 13           32   8477541       Dakota Mermis /api/v1/people/8477541
    ## 14           37   8478401         Pavel Zacha /api/v1/people/8478401
    ## 15           29   8478406 Mackenzie Blackwood /api/v1/people/8478406
    ## 16           16   8479291        Kevin Rooney /api/v1/people/8479291
    ## 17           14   8479315       Joey Anderson /api/v1/people/8479315
    ## 18           63   8479407        Jesper Bratt /api/v1/people/8479407
    ## 19           41   8479415      Michael McLeod /api/v1/people/8479415
    ## 20           13   8480002       Nico Hischier /api/v1/people/8480002
    ## 21           86   8481559         Jack Hughes /api/v1/people/8481559
    ##    position.code position.name position.type position.abbreviation
    ## 1              C        Center       Forward                     C
    ## 2              G        Goalie        Goalie                     G
    ## 3              D    Defenseman    Defenseman                     D
    ## 4              R    Right Wing       Forward                    RW
    ## 5              D    Defenseman    Defenseman                     D
    ## 6              D    Defenseman    Defenseman                     D
    ## 7              D    Defenseman    Defenseman                     D
    ## 8              L     Left Wing       Forward                    LW
    ## 9              D    Defenseman    Defenseman                     D
    ## 10             C        Center       Forward                     C
    ## 11             L     Left Wing       Forward                    LW
    ## 12             D    Defenseman    Defenseman                     D
    ## 13             D    Defenseman    Defenseman                     D
    ## 14             C        Center       Forward                     C
    ## 15             G        Goalie        Goalie                     G
    ## 16             C        Center       Forward                     C
    ## 17             R    Right Wing       Forward                    RW
    ## 18             L     Left Wing       Forward                    LW
    ## 19             C        Center       Forward                     C
    ## 20             C        Center       Forward                     C
    ## 21             C        Center       Forward                     C
    ## 
    ## [[1]][[1]][[2]]
    ##    jerseyNumber person.id     person.fullName            person.link
    ## 1            55   8470187      Johnny Boychuk /api/v1/people/8470187
    ## 2            16   8471217         Andrew Ladd /api/v1/people/8471217
    ## 3             1   8471306       Thomas Greiss /api/v1/people/8471306
    ## 4             4   8472382         Andy Greene /api/v1/people/8472382
    ## 5            47   8473463         Leo Komarov /api/v1/people/8473463
    ## 6            15   8473504     Cal Clutterbuck /api/v1/people/8473504
    ## 7            10   8473544     Derick Brassard /api/v1/people/8473544
    ## 8            40   8473575     Semyon Varlamov /api/v1/people/8473575
    ## 9            34   8474066       Thomas Hickey /api/v1/people/8474066
    ## 10           12   8474573         Josh Bailey /api/v1/people/8474573
    ## 11            7   8474586       Jordan Eberle /api/v1/people/8474586
    ## 12           17   8474709         Matt Martin /api/v1/people/8474709
    ## 13            2   8475181          Nick Leddy /api/v1/people/8475181
    ## 14           53   8475231       Casey Cizikas /api/v1/people/8475231
    ## 15           27   8475314          Anders Lee /api/v1/people/8475314
    ## 16           29   8475754        Brock Nelson /api/v1/people/8475754
    ## 17           14   8475832       Tom Kuhnhackl /api/v1/people/8475832
    ## 18           44   8476419 Jean-Gabriel Pageau /api/v1/people/8476419
    ## 19           24   8476429      Scott Mayfield /api/v1/people/8476429
    ## 20           33   8476444  Christopher Gibson /api/v1/people/8476444
    ## 21            3   8476917         Adam Pelech /api/v1/people/8476917
    ## 22            6   8477506         Ryan Pulock /api/v1/people/8477506
    ## 23           32   8477527       Ross Johnston /api/v1/people/8477527
    ## 24           28   8477936   Michael Dal Colle /api/v1/people/8477936
    ## 25           25   8478038         Devon Toews /api/v1/people/8478038
    ## 26           13   8478445       Mathew Barzal /api/v1/people/8478445
    ## 27           18   8478463 Anthony Beauvillier /api/v1/people/8478463
    ## 28           21   8479526        Otto Koivula /api/v1/people/8479526
    ## 29           38   8480222       Sebastian Aho /api/v1/people/8480222
    ## 30            8   8480865         Noah Dobson /api/v1/people/8480865
    ##    position.code position.name position.type position.abbreviation
    ## 1              D    Defenseman    Defenseman                     D
    ## 2              L     Left Wing       Forward                    LW
    ## 3              G        Goalie        Goalie                     G
    ## 4              D    Defenseman    Defenseman                     D
    ## 5              R    Right Wing       Forward                    RW
    ## 6              R    Right Wing       Forward                    RW
    ## 7              C        Center       Forward                     C
    ## 8              G        Goalie        Goalie                     G
    ## 9              D    Defenseman    Defenseman                     D
    ## 10             R    Right Wing       Forward                    RW
    ## 11             R    Right Wing       Forward                    RW
    ## 12             L     Left Wing       Forward                    LW
    ## 13             D    Defenseman    Defenseman                     D
    ## 14             C        Center       Forward                     C
    ## 15             L     Left Wing       Forward                    LW
    ## 16             C        Center       Forward                     C
    ## 17             R    Right Wing       Forward                    RW
    ## 18             C        Center       Forward                     C
    ## 19             D    Defenseman    Defenseman                     D
    ## 20             G        Goalie        Goalie                     G
    ## 21             D    Defenseman    Defenseman                     D
    ## 22             D    Defenseman    Defenseman                     D
    ## 23             L     Left Wing       Forward                    LW
    ## 24             L     Left Wing       Forward                    LW
    ## 25             D    Defenseman    Defenseman                     D
    ## 26             C        Center       Forward                     C
    ## 27             L     Left Wing       Forward                    LW
    ## 28             L     Left Wing       Forward                    LW
    ## 29             D    Defenseman    Defenseman                     D
    ## 30             D    Defenseman    Defenseman                     D
    ## 
    ## [[1]][[1]][[3]]
    ##    jerseyNumber person.id     person.fullName            person.link
    ## 1            38   8474230       Micheal Haley /api/v1/people/8474230
    ## 2            74   8480833     Vitali Kravtsov /api/v1/people/8480833
    ## 3            30   8468685    Henrik Lundqvist /api/v1/people/8468685
    ## 4            18   8471686          Marc Staal /api/v1/people/8471686
    ## 5            42   8474090       Brendan Smith /api/v1/people/8474090
    ## 6            20   8475184       Chris Kreider /api/v1/people/8475184
    ## 7            14   8475735         Greg McKegg /api/v1/people/8475735
    ## 8            17   8475855         Jesper Fast /api/v1/people/8475855
    ## 9            29   8476396      Steven Fogarty /api/v1/people/8476396
    ## 10           16   8476458         Ryan Strome /api/v1/people/8476458
    ## 11           93   8476459      Mika Zibanejad /api/v1/people/8476459
    ## 12           33   8476858 Phillip Di Giuseppe /api/v1/people/8476858
    ## 13            8   8476885        Jacob Trouba /api/v1/people/8476885
    ## 14           65   8476982       Danny O'Regan /api/v1/people/8476982
    ## 15           89   8477402    Pavel Buchnevich /api/v1/people/8477402
    ## 16           77   8477950       Tony DeAngelo /api/v1/people/8477950
    ## 17           48   8477962     Brendan Lemieux /api/v1/people/8477962
    ## 18           31   8478048     Igor Shesterkin /api/v1/people/8478048
    ## 19           44   8478178      Darren Raddysh /api/v1/people/8478178
    ## 20           10   8478550      Artemi Panarin /api/v1/people/8478550
    ## 21           46   8479027     Brandon Crawley /api/v1/people/8479027
    ## 22           23   8479323            Adam Fox /api/v1/people/8479323
    ## 23           55   8479324       Ryan Lindgren /api/v1/people/8479324
    ## 24           12   8479328     Julien Gauthier /api/v1/people/8479328
    ## 25           25   8479333         Libor Hajek /api/v1/people/8479333
    ## 26           21   8479353        Brett Howden /api/v1/people/8479353
    ## 27           26   8479364       Tim Gettinger /api/v1/people/8479364
    ## 28           95   8479968      Vinni Lettieri /api/v1/people/8479968
    ## 29           72   8480078        Filip Chytil /api/v1/people/8480078
    ## 30           40   8480382  Alexandar Georgiev /api/v1/people/8480382
    ## 31           24   8481554         Kaapo Kakko /api/v1/people/8481554
    ##    position.code position.name position.type position.abbreviation
    ## 1              C        Center       Forward                     C
    ## 2              R    Right Wing       Forward                    RW
    ## 3              G        Goalie        Goalie                     G
    ## 4              D    Defenseman    Defenseman                     D
    ## 5              D    Defenseman    Defenseman                     D
    ## 6              L     Left Wing       Forward                    LW
    ## 7              C        Center       Forward                     C
    ## 8              R    Right Wing       Forward                    RW
    ## 9              C        Center       Forward                     C
    ## 10             C        Center       Forward                     C
    ## 11             C        Center       Forward                     C
    ## 12             L     Left Wing       Forward                    LW
    ## 13             D    Defenseman    Defenseman                     D
    ## 14             C        Center       Forward                     C
    ## 15             R    Right Wing       Forward                    RW
    ## 16             D    Defenseman    Defenseman                     D
    ## 17             L     Left Wing       Forward                    LW
    ## 18             G        Goalie        Goalie                     G
    ## 19             D    Defenseman    Defenseman                     D
    ## 20             L     Left Wing       Forward                    LW
    ## 21             D    Defenseman    Defenseman                     D
    ## 22             D    Defenseman    Defenseman                     D
    ## 23             D    Defenseman    Defenseman                     D
    ## 24             R    Right Wing       Forward                    RW
    ## 25             D    Defenseman    Defenseman                     D
    ## 26             C        Center       Forward                     C
    ## 27             L     Left Wing       Forward                    LW
    ## 28             R    Right Wing       Forward                    RW
    ## 29             C        Center       Forward                     C
    ## 30             G        Goalie        Goalie                     G
    ## 31             R    Right Wing       Forward                    RW
    ## 
    ## [[1]][[1]][[4]]
    ##    jerseyNumber person.id     person.fullName            person.link
    ## 1            55   8477502        Samuel Morin /api/v1/people/8477502
    ## 2            44   8473485       Chris Stewart /api/v1/people/8473485
    ## 3            44   8470775       Nate Thompson /api/v1/people/8470775
    ## 4            37   8470880       Brian Elliott /api/v1/people/8470880
    ## 5            15   8471702       Matt Niskanen /api/v1/people/8471702
    ## 6            28   8473512       Claude Giroux /api/v1/people/8473512
    ## 7            61   8474027        Justin Braun /api/v1/people/8474027
    ## 8            25   8474037  James van Riemsdyk /api/v1/people/8474037
    ## 9            93   8474161       Jakub Voracek /api/v1/people/8474161
    ## 10           38   8474683         Derek Grant /api/v1/people/8474683
    ## 11           18   8475752       Tyler Pitlick /api/v1/people/8475752
    ## 12           13   8475763         Kevin Hayes /api/v1/people/8475763
    ## 13           10   8476404       Andy Andreoff /api/v1/people/8476404
    ## 14            3   8476407       Andy Welinski /api/v1/people/8476407
    ## 15           14   8476461      Sean Couturier /api/v1/people/8476461
    ## 16           21   8476872      Scott Laughton /api/v1/people/8476872
    ## 17           53   8476906 Shayne Gostisbehere /api/v1/people/8476906
    ## 18           12   8477290       Michael Raffl /api/v1/people/8477290
    ## 19            8   8477462         Robert Hagg /api/v1/people/8477462
    ## 20            6   8477948      Travis Sanheim /api/v1/people/8477948
    ## 21           62   8477979  Nicolas Aube-Kubel /api/v1/people/8477979
    ## 22           59   8478017       Mark Friedman /api/v1/people/8478017
    ## 23           23   8478067      Oskar Lindblom /api/v1/people/8478067
    ## 24           11   8478439      Travis Konecny /api/v1/people/8478439
    ## 25            9   8478500       Ivan Provorov /api/v1/people/8478500
    ## 26            5   8479026      Philippe Myers /api/v1/people/8479026
    ## 27           34   8479312           Alex Lyon /api/v1/people/8479312
    ## 28           82   8479382     Connor Bunnaman /api/v1/people/8479382
    ## 29           79   8479394         Carter Hart /api/v1/people/8479394
    ## 30           48   8480028        Morgan Frost /api/v1/people/8480028
    ## 31           67   8480279    Kirill Ustimenko /api/v1/people/8480279
    ## 32           49   8480797        Joel Farabee /api/v1/people/8480797
    ## 33           54   8481178         Egor Zamula /api/v1/people/8481178
    ##    position.code position.name position.type position.abbreviation
    ## 1              D    Defenseman    Defenseman                     D
    ## 2              R    Right Wing       Forward                    RW
    ## 3              C        Center       Forward                     C
    ## 4              G        Goalie        Goalie                     G
    ## 5              D    Defenseman    Defenseman                     D
    ## 6              C        Center       Forward                     C
    ## 7              D    Defenseman    Defenseman                     D
    ## 8              L     Left Wing       Forward                    LW
    ## 9              R    Right Wing       Forward                    RW
    ## 10             C        Center       Forward                     C
    ## 11             C        Center       Forward                     C
    ## 12             C        Center       Forward                     C
    ## 13             C        Center       Forward                     C
    ## 14             D    Defenseman    Defenseman                     D
    ## 15             C        Center       Forward                     C
    ## 16             C        Center       Forward                     C
    ## 17             D    Defenseman    Defenseman                     D
    ## 18             L     Left Wing       Forward                    LW
    ## 19             D    Defenseman    Defenseman                     D
    ## 20             D    Defenseman    Defenseman                     D
    ## 21             R    Right Wing       Forward                    RW
    ## 22             D    Defenseman    Defenseman                     D
    ## 23             L     Left Wing       Forward                    LW
    ## 24             R    Right Wing       Forward                    RW
    ## 25             D    Defenseman    Defenseman                     D
    ## 26             D    Defenseman    Defenseman                     D
    ## 27             G        Goalie        Goalie                     G
    ## 28             C        Center       Forward                     C
    ## 29             G        Goalie        Goalie                     G
    ## 30             C        Center       Forward                     C
    ## 31             G        Goalie        Goalie                     G
    ## 32             L     Left Wing       Forward                    LW
    ## 33             D    Defenseman    Defenseman                     D
    ## 
    ## [[1]][[1]][[5]]
    ##    jerseyNumber person.id   person.fullName            person.link
    ## 1            18   8478866     Dominik Simon /api/v1/people/8478866
    ## 2            12   8466139   Patrick Marleau /api/v1/people/8466139
    ## 3            71   8471215     Evgeni Malkin /api/v1/people/8471215
    ## 4            87   8471675     Sidney Crosby /api/v1/people/8471675
    ## 5             3   8471677      Jack Johnson /api/v1/people/8471677
    ## 6            58   8471724       Kris Letang /api/v1/people/8471724
    ## 7            72   8471887  Patric Hornqvist /api/v1/people/8471887
    ## 8             4   8474602    Justin Schultz /api/v1/people/8474602
    ## 9             8   8475208    Brian Dumoulin /api/v1/people/8475208
    ## 10           16   8475722      Jason Zucker /api/v1/people/8475722
    ## 11           17   8475810        Bryan Rust /api/v1/people/8475810
    ## 12           30   8476899       Matt Murray /api/v1/people/8476899
    ## 13           53   8476927     Teddy Blueger /api/v1/people/8476927
    ## 14            2   8477244     Chad Ruhwedel /api/v1/people/8477244
    ## 15           59   8477404     Jake Guentzel /api/v1/people/8477404
    ## 16           35   8477465     Tristan Jarry /api/v1/people/8477465
    ## 17           43   8477839      Conor Sheary /api/v1/people/8477839
    ## 18           42   8477953   Kasperi Kapanen /api/v1/people/8477953
    ## 19           19   8477955      Jared McCann /api/v1/people/8477955
    ## 20           28   8477969 Marcus Pettersson /api/v1/people/8477969
    ## 21           37   8478043      Sam Lafferty /api/v1/people/8478043
    ## 22           57   8478074   Anthony Angello /api/v1/people/8478074
    ## 23            6   8478507       John Marino /api/v1/people/8478507
    ## 24           13   8479293     Brandon Tanev /api/v1/people/8479293
    ## 25           46   8479944  Zach Aston-Reese /api/v1/people/8479944
    ## 26           50   8480945     Juuso Riikola /api/v1/people/8480945
    ##    position.code position.name position.type position.abbreviation
    ## 1              C        Center       Forward                     C
    ## 2              C        Center       Forward                     C
    ## 3              C        Center       Forward                     C
    ## 4              C        Center       Forward                     C
    ## 5              D    Defenseman    Defenseman                     D
    ## 6              D    Defenseman    Defenseman                     D
    ## 7              R    Right Wing       Forward                    RW
    ## 8              D    Defenseman    Defenseman                     D
    ## 9              D    Defenseman    Defenseman                     D
    ## 10             L     Left Wing       Forward                    LW
    ## 11             R    Right Wing       Forward                    RW
    ## 12             G        Goalie        Goalie                     G
    ## 13             C        Center       Forward                     C
    ## 14             D    Defenseman    Defenseman                     D
    ## 15             L     Left Wing       Forward                    LW
    ## 16             G        Goalie        Goalie                     G
    ## 17             L     Left Wing       Forward                    LW
    ## 18             R    Right Wing       Forward                    RW
    ## 19             C        Center       Forward                     C
    ## 20             D    Defenseman    Defenseman                     D
    ## 21             C        Center       Forward                     C
    ## 22             C        Center       Forward                     C
    ## 23             D    Defenseman    Defenseman                     D
    ## 24             L     Left Wing       Forward                    LW
    ## 25             C        Center       Forward                     C
    ## 26             D    Defenseman    Defenseman                     D
    ## 
    ## [[1]][[1]][[6]]
    ##    jerseyNumber person.id  person.fullName            person.link position.code
    ## 1            86   8476191     Kevan Miller /api/v1/people/8476191             D
    ## 2            33   8465009      Zdeno Chara /api/v1/people/8465009             D
    ## 3            37   8470638 Patrice Bergeron /api/v1/people/8470638             C
    ## 4            41   8470860   Jaroslav Halak /api/v1/people/8470860             G
    ## 5            46   8471276     David Krejci /api/v1/people/8471276             C
    ## 6            40   8471695      Tuukka Rask /api/v1/people/8471695             G
    ## 7            63   8473419    Brad Marchand /api/v1/people/8473419             L
    ## 8            27   8475186       John Moore /api/v1/people/8475186             D
    ## 9            13   8475745    Charlie Coyle /api/v1/people/8475745             C
    ## 10           14   8475780     Chris Wagner /api/v1/people/8475780             R
    ## 11           20   8475807 Joakim Nordstrom /api/v1/people/8475807             C
    ## 12           52   8476374      Sean Kuraly /api/v1/people/8476374             C
    ## 13           35   8476509    Maxime Lagace /api/v1/people/8476509             G
    ## 14           47   8476792       Torey Krug /api/v1/people/8476792             D
    ## 15           48   8476891    Matt Grzelcyk /api/v1/people/8476891             D
    ## 16           75   8477365   Connor Clifton /api/v1/people/8477365             D
    ## 17           21   8477941     Nick Ritchie /api/v1/people/8477941             L
    ## 18           88   8477956   David Pastrnak /api/v1/people/8477956             R
    ## 19           10   8478075     Anders Bjork /api/v1/people/8478075             L
    ## 20           28   8478131      Ondrej Kase /api/v1/people/8478131             R
    ## 21           67   8478415     Jakub Zboril /api/v1/people/8478415             D
    ## 22           80   8478435       Dan Vladar /api/v1/people/8478435             G
    ## 23           25   8478443    Brandon Carlo /api/v1/people/8478443             D
    ## 24           79   8478468    Jeremy Lauzon /api/v1/people/8478468             D
    ## 25           19   8478485    Zach Senyshyn /api/v1/people/8478485             R
    ## 26           74   8478498     Jake DeBrusk /api/v1/people/8478498             L
    ## 27           73   8479325   Charlie McAvoy /api/v1/people/8479325             D
    ## 28           82   8479365   Trent Frederic /api/v1/people/8479365             C
    ## 29           58   8480001 Urho Vaakanainen /api/v1/people/8480001             D
    ## 30           68   8480021   Jack Studnicka /api/v1/people/8480021             C
    ## 31           83   8480901   Karson Kuhlman /api/v1/people/8480901             C
    ## 32           26   8480944     Par Lindholm /api/v1/people/8480944             C
    ##    position.name position.type position.abbreviation
    ## 1     Defenseman    Defenseman                     D
    ## 2     Defenseman    Defenseman                     D
    ## 3         Center       Forward                     C
    ## 4         Goalie        Goalie                     G
    ## 5         Center       Forward                     C
    ## 6         Goalie        Goalie                     G
    ## 7      Left Wing       Forward                    LW
    ## 8     Defenseman    Defenseman                     D
    ## 9         Center       Forward                     C
    ## 10    Right Wing       Forward                    RW
    ## 11        Center       Forward                     C
    ## 12        Center       Forward                     C
    ## 13        Goalie        Goalie                     G
    ## 14    Defenseman    Defenseman                     D
    ## 15    Defenseman    Defenseman                     D
    ## 16    Defenseman    Defenseman                     D
    ## 17     Left Wing       Forward                    LW
    ## 18    Right Wing       Forward                    RW
    ## 19     Left Wing       Forward                    LW
    ## 20    Right Wing       Forward                    RW
    ## 21    Defenseman    Defenseman                     D
    ## 22        Goalie        Goalie                     G
    ## 23    Defenseman    Defenseman                     D
    ## 24    Defenseman    Defenseman                     D
    ## 25    Right Wing       Forward                    RW
    ## 26     Left Wing       Forward                    LW
    ## 27    Defenseman    Defenseman                     D
    ## 28        Center       Forward                     C
    ## 29    Defenseman    Defenseman                     D
    ## 30        Center       Forward                     C
    ## 31        Center       Forward                     C
    ## 32        Center       Forward                     C
    ## 
    ## [[1]][[1]][[7]]
    ##    jerseyNumber person.id    person.fullName            person.link
    ## 1            17   8471743   Vladimir Sobotka /api/v1/people/8471743
    ## 2            72   8479420      Tage Thompson /api/v1/people/8479420
    ## 3            48   8471436       Matt Hunwick /api/v1/people/8471436
    ## 4            12   8470595         Eric Staal /api/v1/people/8470595
    ## 5            21   8473449        Kyle Okposo /api/v1/people/8473449
    ## 6            67   8473564     Michael Frolik /api/v1/people/8473564
    ## 7            17   8474190     Wayne Simmonds /api/v1/people/8474190
    ## 8            40   8475622      Carter Hutton /api/v1/people/8475622
    ## 9            22   8475728      Johan Larsson /api/v1/people/8475728
    ## 10           53   8475784       Jeff Skinner /api/v1/people/8475784
    ## 11           33   8476525       Colin Miller /api/v1/people/8476525
    ## 12           28   8476878  Zemgus Girgensons /api/v1/people/8476878
    ## 13           13   8476918        Jimmy Vesey /api/v1/people/8476918
    ## 14           19   8476931        Jake McCabe /api/v1/people/8476931
    ## 15           35   8476999      Linus Ullmark /api/v1/people/8476999
    ## 16           55   8477499 Rasmus Ristolainen /api/v1/people/8477499
    ## 17           27   8477508       Curtis Lazar /api/v1/people/8477508
    ## 18           23   8477933       Sam Reinhart /api/v1/people/8477933
    ## 19           62   8477986    Brandon Montour /api/v1/people/8477986
    ## 20           68   8478109    Victor Olofsson /api/v1/people/8478109
    ## 21            9   8478403        Jack Eichel /api/v1/people/8478403
    ## 22           10   8480035    Henri Jokiharju /api/v1/people/8480035
    ## 23           26   8480839      Rasmus Dahlin /api/v1/people/8480839
    ## 24           24   8480935     Lawrence Pilut /api/v1/people/8480935
    ## 25           95   8480946      Dominik Kahun /api/v1/people/8480946
    ##    position.code position.name position.type position.abbreviation
    ## 1              C        Center       Forward                     C
    ## 2              R    Right Wing       Forward                    RW
    ## 3              D    Defenseman    Defenseman                     D
    ## 4              C        Center       Forward                     C
    ## 5              R    Right Wing       Forward                    RW
    ## 6              R    Right Wing       Forward                    RW
    ## 7              R    Right Wing       Forward                    RW
    ## 8              G        Goalie        Goalie                     G
    ## 9              C        Center       Forward                     C
    ## 10             L     Left Wing       Forward                    LW
    ## 11             D    Defenseman    Defenseman                     D
    ## 12             C        Center       Forward                     C
    ## 13             L     Left Wing       Forward                    LW
    ## 14             D    Defenseman    Defenseman                     D
    ## 15             G        Goalie        Goalie                     G
    ## 16             D    Defenseman    Defenseman                     D
    ## 17             C        Center       Forward                     C
    ## 18             C        Center       Forward                     C
    ## 19             D    Defenseman    Defenseman                     D
    ## 20             L     Left Wing       Forward                    LW
    ## 21             C        Center       Forward                     C
    ## 22             D    Defenseman    Defenseman                     D
    ## 23             D    Defenseman    Defenseman                     D
    ## 24             D    Defenseman    Defenseman                     D
    ## 25             C        Center       Forward                     C
    ## 
    ## [[1]][[1]][[8]]
    ##    jerseyNumber person.id    person.fullName            person.link
    ## 1            45   8477460    Laurent Dauphin /api/v1/people/8477460
    ## 2            46   8479990         Josh Brook /api/v1/people/8479990
    ## 3             6   8470642         Shea Weber /api/v1/people/8470642
    ## 4            31   8471679        Carey Price /api/v1/people/8471679
    ## 5            26   8473507         Jeff Petry /api/v1/people/8473507
    ## 6            41   8474038         Paul Byron /api/v1/people/8474038
    ## 7            34   8474596         Jake Allen /api/v1/people/8474596
    ## 8            22   8474668         Dale Weise /api/v1/people/8474668
    ## 9            90   8475193        Tomas Tatar /api/v1/people/8475193
    ## 10            8   8475279        Ben Chiarot /api/v1/people/8475279
    ## 11           43   8475738        Jordan Weal /api/v1/people/8475738
    ## 12           11   8475848  Brendan Gallagher /api/v1/people/8475848
    ## 13           60   8475968       Alex Belzile /api/v1/people/8475968
    ## 14            6   8476441     Joel Edmundson /api/v1/people/8476441
    ## 15           61   8476443     Xavier Ouellet /api/v1/people/8476443
    ## 16           40   8476469         Joel Armia /api/v1/people/8476469
    ## 17           24   8476479    Phillip Danault /api/v1/people/8476479
    ## 18           54   8476948      Charles Hudon /api/v1/people/8476948
    ## 19           77   8476967        Brett Kulak /api/v1/people/8476967
    ## 20           51   8477467    Gustav Olofsson /api/v1/people/8477467
    ## 21           62   8477476   Artturi Lehkonen /api/v1/people/8477476
    ## 22           92   8477494    Jonathan Drouin /api/v1/people/8477494
    ## 23           13   8477503           Max Domi /api/v1/people/8477503
    ## 24           32   8477850    Christian Folin /api/v1/people/8477850
    ## 25           71   8478133         Jake Evans /api/v1/people/8478133
    ## 26           58   8478454       Noah Juulsen /api/v1/people/8478454
    ## 27           70   8478933    Michael McNiven /api/v1/people/8478933
    ## 28           39   8479292   Charlie Lindgren /api/v1/people/8479292
    ## 29           53   8479376        Victor Mete /api/v1/people/8479376
    ## 30           20   8479985        Cale Fleury /api/v1/people/8479985
    ## 31           14   8480018        Nick Suzuki /api/v1/people/8480018
    ## 32           30   8480051     Cayden Primeau /api/v1/people/8480051
    ## 33           25   8480068      Ryan Poehling /api/v1/people/8480068
    ## 34           15   8480829 Jesperi Kotkaniemi /api/v1/people/8480829
    ##    position.code position.name position.type position.abbreviation
    ## 1              C        Center       Forward                     C
    ## 2              D    Defenseman    Defenseman                     D
    ## 3              D    Defenseman    Defenseman                     D
    ## 4              G        Goalie        Goalie                     G
    ## 5              D    Defenseman    Defenseman                     D
    ## 6              L     Left Wing       Forward                    LW
    ## 7              G        Goalie        Goalie                     G
    ## 8              R    Right Wing       Forward                    RW
    ## 9              L     Left Wing       Forward                    LW
    ## 10             D    Defenseman    Defenseman                     D
    ## 11             C        Center       Forward                     C
    ## 12             R    Right Wing       Forward                    RW
    ## 13             R    Right Wing       Forward                    RW
    ## 14             D    Defenseman    Defenseman                     D
    ## 15             D    Defenseman    Defenseman                     D
    ## 16             R    Right Wing       Forward                    RW
    ## 17             C        Center       Forward                     C
    ## 18             L     Left Wing       Forward                    LW
    ## 19             D    Defenseman    Defenseman                     D
    ## 20             D    Defenseman    Defenseman                     D
    ## 21             L     Left Wing       Forward                    LW
    ## 22             L     Left Wing       Forward                    LW
    ## 23             C        Center       Forward                     C
    ## 24             D    Defenseman    Defenseman                     D
    ## 25             C        Center       Forward                     C
    ## 26             D    Defenseman    Defenseman                     D
    ## 27             G        Goalie        Goalie                     G
    ## 28             G        Goalie        Goalie                     G
    ## 29             D    Defenseman    Defenseman                     D
    ## 30             D    Defenseman    Defenseman                     D
    ## 31             C        Center       Forward                     C
    ## 32             G        Goalie        Goalie                     G
    ## 33             C        Center       Forward                     C
    ## 34             C        Center       Forward                     C
    ## 
    ## [[1]][[1]][[9]]
    ##    jerseyNumber person.id   person.fullName            person.link
    ## 1            31   8475195    Anders Nilsson /api/v1/people/8475195
    ## 2            41   8467950    Craig Anderson /api/v1/people/8467950
    ## 3            81   8468493       Ron Hainsey /api/v1/people/8468493
    ## 4             9   8471676        Bobby Ryan /api/v1/people/8471676
    ## 5            51   8473573    Artem Anisimov /api/v1/people/8473573
    ## 6            89   8474571    Mikkel Boedker /api/v1/people/8474571
    ## 7            74   8474697   Mark Borowiecki /api/v1/people/8474697
    ## 8            53   8476285      Matthew Peca /api/v1/people/8476285
    ## 9             5   8476422       Mike Reilly /api/v1/people/8476422
    ## 10           71   8476919     Chris Tierney /api/v1/people/8476919
    ## 11           28   8477015      Connor Brown /api/v1/people/8477015
    ## 12           49   8477149    Scott Sabourin /api/v1/people/8477149
    ## 13           35   8477405    Marcus Hogberg /api/v1/people/8477405
    ## 14           10   8477407   Anthony Duclair /api/v1/people/8477407
    ## 15           13   8477426         Nick Paul /api/v1/people/8477426
    ## 16           79   8477963    Jayce Hawryluk /api/v1/people/8477963
    ## 17           39   8477971   Andreas Englund /api/v1/people/8477971
    ## 18           36   8478400       Colin White /api/v1/people/8478400
    ## 19           72   8478469     Thomas Chabot /api/v1/people/8478469
    ## 20           86   8478846 Christian Wolanin /api/v1/people/8478846
    ## 21           38   8478870   Rudolfs Balcers /api/v1/people/8478870
    ## 22           22   8479458    Nikita Zaitsev /api/v1/people/8479458
    ## 23            7   8480801     Brady Tkachuk /api/v1/people/8480801
    ##    position.code position.name position.type position.abbreviation
    ## 1              G        Goalie        Goalie                     G
    ## 2              G        Goalie        Goalie                     G
    ## 3              D    Defenseman    Defenseman                     D
    ## 4              R    Right Wing       Forward                    RW
    ## 5              C        Center       Forward                     C
    ## 6              L     Left Wing       Forward                    LW
    ## 7              D    Defenseman    Defenseman                     D
    ## 8              C        Center       Forward                     C
    ## 9              D    Defenseman    Defenseman                     D
    ## 10             C        Center       Forward                     C
    ## 11             R    Right Wing       Forward                    RW
    ## 12             R    Right Wing       Forward                    RW
    ## 13             G        Goalie        Goalie                     G
    ## 14             L     Left Wing       Forward                    LW
    ## 15             L     Left Wing       Forward                    LW
    ## 16             R    Right Wing       Forward                    RW
    ## 17             D    Defenseman    Defenseman                     D
    ## 18             C        Center       Forward                     C
    ## 19             D    Defenseman    Defenseman                     D
    ## 20             D    Defenseman    Defenseman                     D
    ## 21             L     Left Wing       Forward                    LW
    ## 22             D    Defenseman    Defenseman                     D
    ## 23             L     Left Wing       Forward                    LW
    ## 
    ## [[1]][[1]][[10]]
    ##    jerseyNumber person.id    person.fullName            person.link
    ## 1            19   8469455       Jason Spezza /api/v1/people/8469455
    ## 2             8   8474162        Jake Muzzin /api/v1/people/8474162
    ## 3            73   8475160      Kyle Clifford /api/v1/people/8475160
    ## 4            91   8475166       John Tavares /api/v1/people/8475166
    ## 5            94   8475197       Tyson Barrie /api/v1/people/8475197
    ## 6            52   8475716    Martin Marincin /api/v1/people/8475716
    ## 7             3   8475718        Justin Holl /api/v1/people/8475718
    ## 8            11   8475786         Zach Hyman /api/v1/people/8475786
    ## 9            36   8475789      Jack Campbell /api/v1/people/8475789
    ## 10           31   8475883  Frederik Andersen /api/v1/people/8475883
    ## 11           44   8476853      Morgan Rielly /api/v1/people/8476853
    ## 12           83   8476879          Cody Ceci /api/v1/people/8476879
    ## 13           15   8477021  Alexander Kerfoot /api/v1/people/8477021
    ## 14           18   8477341   Andreas Johnsson /api/v1/people/8477341
    ## 15           61   8477464          Nic Petan /api/v1/people/8477464
    ## 16           33   8477512  Frederik Gauthier /api/v1/people/8477512
    ## 17           88   8477939   William Nylander /api/v1/people/8477939
    ## 18           47   8478115     Pierre Engvall /api/v1/people/8478115
    ## 19           23   8478408     Travis Dermott /api/v1/people/8478408
    ## 20           16   8478483    Mitchell Marner /api/v1/people/8478483
    ## 21            9   8478542     Evan Rodrigues /api/v1/people/8478542
    ## 22           62   8478843       Denis Malgin /api/v1/people/8478843
    ## 23           50   8479288   Kasimir Kaskisuo /api/v1/people/8479288
    ## 24           34   8479318    Auston Matthews /api/v1/people/8479318
    ## 25           60   8479361        Joseph Woll /api/v1/people/8479361
    ## 26           48   8480157        Calle Rosen /api/v1/people/8480157
    ## 27           38   8480873      Rasmus Sandin /api/v1/people/8480873
    ## 28           89   8481582 Nicholas Robertson /api/v1/people/8481582
    ## 29           65   8481624      Ilya Mikheyev /api/v1/people/8481624
    ##    position.code position.name position.type position.abbreviation
    ## 1              C        Center       Forward                     C
    ## 2              D    Defenseman    Defenseman                     D
    ## 3              L     Left Wing       Forward                    LW
    ## 4              C        Center       Forward                     C
    ## 5              D    Defenseman    Defenseman                     D
    ## 6              D    Defenseman    Defenseman                     D
    ## 7              D    Defenseman    Defenseman                     D
    ## 8              L     Left Wing       Forward                    LW
    ## 9              G        Goalie        Goalie                     G
    ## 10             G        Goalie        Goalie                     G
    ## 11             D    Defenseman    Defenseman                     D
    ## 12             D    Defenseman    Defenseman                     D
    ## 13             C        Center       Forward                     C
    ## 14             L     Left Wing       Forward                    LW
    ## 15             C        Center       Forward                     C
    ## 16             C        Center       Forward                     C
    ## 17             R    Right Wing       Forward                    RW
    ## 18             L     Left Wing       Forward                    LW
    ## 19             D    Defenseman    Defenseman                     D
    ## 20             R    Right Wing       Forward                    RW
    ## 21             C        Center       Forward                     C
    ## 22             C        Center       Forward                     C
    ## 23             G        Goalie        Goalie                     G
    ## 24             C        Center       Forward                     C
    ## 25             G        Goalie        Goalie                     G
    ## 26             D    Defenseman    Defenseman                     D
    ## 27             D    Defenseman    Defenseman                     D
    ## 28             L     Left Wing       Forward                    LW
    ## 29             R    Right Wing       Forward                    RW
    ## 
    ## [[1]][[1]][[11]]
    ##    jerseyNumber person.id     person.fullName            person.link
    ## 1            22   8477488         Brett Pesce /api/v1/people/8477488
    ## 2            14   8468508     Justin Williams /api/v1/people/8468508
    ## 3            47   8473503        James Reimer /api/v1/people/8473503
    ## 4            11   8473533        Jordan Staal /api/v1/people/8473533
    ## 5            51   8474581       Jake Gardiner /api/v1/people/8474581
    ## 6            45   8475222        Sami Vatanen /api/v1/people/8475222
    ## 7            21   8475799   Nino Niederreiter /api/v1/people/8475799
    ## 8            34   8475852         Petr Mrazek /api/v1/people/8475852
    ## 9            18   8476288        Ryan Dzingel /api/v1/people/8476288
    ## 10           28   8476323       Max McCormick /api/v1/people/8476323
    ## 11           31   8476341      Anton Forsberg /api/v1/people/8476341
    ## 12           16   8476389    Vincent Trocheck /api/v1/people/8476389
    ## 13           19   8476462     Dougie Hamilton /api/v1/people/8476462
    ## 14           76   8476869         Brady Skjei /api/v1/people/8476869
    ## 15           86   8476882    Teuvo Teravainen /api/v1/people/8476882
    ## 16           48   8476921    Jordan Martinook /api/v1/people/8476921
    ## 17           23   8476934        Brock McGinn /api/v1/people/8476934
    ## 18           74   8476958       Jaccob Slavin /api/v1/people/8476958
    ## 19           57   8477845 Trevor van Riemsdyk /api/v1/people/8477845
    ## 20            4   8477938        Haydn Fleury /api/v1/people/8477938
    ## 21           39   8477968    Alex Nedeljkovic /api/v1/people/8477968
    ## 22           55   8477981      Roland McKeown /api/v1/people/8477981
    ## 23           13   8477998      Warren Foegele /api/v1/people/8477998
    ## 24           64   8478056        Clark Bishop /api/v1/people/8478056
    ## 25           20   8478427       Sebastian Aho /api/v1/people/8478427
    ## 26           78   8478904      Steven Lorentz /api/v1/people/8478904
    ## 27           24   8479402           Jake Bean /api/v1/people/8479402
    ## 28           43   8479987       Morgan Geekie /api/v1/people/8479987
    ## 29           88   8480039        Martin Necas /api/v1/people/8480039
    ## 30           37   8480830   Andrei Svechnikov /api/v1/people/8480830
    ##    position.code position.name position.type position.abbreviation
    ## 1              D    Defenseman    Defenseman                     D
    ## 2              R    Right Wing       Forward                    RW
    ## 3              G        Goalie        Goalie                     G
    ## 4              C        Center       Forward                     C
    ## 5              D    Defenseman    Defenseman                     D
    ## 6              D    Defenseman    Defenseman                     D
    ## 7              R    Right Wing       Forward                    RW
    ## 8              G        Goalie        Goalie                     G
    ## 9              C        Center       Forward                     C
    ## 10             L     Left Wing       Forward                    LW
    ## 11             G        Goalie        Goalie                     G
    ## 12             C        Center       Forward                     C
    ## 13             D    Defenseman    Defenseman                     D
    ## 14             D    Defenseman    Defenseman                     D
    ## 15             L     Left Wing       Forward                    LW
    ## 16             L     Left Wing       Forward                    LW
    ## 17             L     Left Wing       Forward                    LW
    ## 18             D    Defenseman    Defenseman                     D
    ## 19             D    Defenseman    Defenseman                     D
    ## 20             D    Defenseman    Defenseman                     D
    ## 21             G        Goalie        Goalie                     G
    ## 22             D    Defenseman    Defenseman                     D
    ## 23             L     Left Wing       Forward                    LW
    ## 24             C        Center       Forward                     C
    ## 25             C        Center       Forward                     C
    ## 26             C        Center       Forward                     C
    ## 27             D    Defenseman    Defenseman                     D
    ## 28             C        Center       Forward                     C
    ## 29             C        Center       Forward                     C
    ## 30             R    Right Wing       Forward                    RW
    ## 
    ## [[1]][[1]][[12]]
    ##    jerseyNumber person.id     person.fullName            person.link
    ## 1            27   8480185    Eetu Luostarinen /api/v1/people/8480185
    ## 2             9   8470619         Brian Boyle /api/v1/people/8470619
    ## 3             3   8471735        Keith Yandle /api/v1/people/8471735
    ## 4             6   8471873      Anton Stralman /api/v1/people/8471873
    ## 5             7   8474098     Colton Sceviour /api/v1/people/8474098
    ## 6            63   8474149     Evgenii Dadonov /api/v1/people/8474149
    ## 7            68   8474884        Mike Hoffman /api/v1/people/8474884
    ## 8            56   8475287          Erik Haula /api/v1/people/8475287
    ## 9            72   8475683    Sergei Bobrovsky /api/v1/people/8475683
    ## 10           10   8475792      Brett Connolly /api/v1/people/8475792
    ## 11           13   8475796          Mark Pysyk /api/v1/people/8475796
    ## 12           11   8476456  Jonathan Huberdeau /api/v1/people/8476456
    ## 13           19   8476875       Mike Matheson /api/v1/people/8476875
    ## 14           60   8476904      Chris Driedger /api/v1/people/8476904
    ## 15           14   8476952    Dominic Toninato /api/v1/people/8476952
    ## 16           52   8477346    MacKenzie Weegar /api/v1/people/8477346
    ## 17            2   8477384          Josh Brown /api/v1/people/8477384
    ## 18           30   8477475 Philippe Desrosiers /api/v1/people/8477475
    ## 19           16   8477493   Aleksander Barkov /api/v1/people/8477493
    ## 20            5   8477932        Aaron Ekblad /api/v1/people/8477932
    ## 21           71   8478027      Lucas Wallmark /api/v1/people/8478027
    ## 22           73   8478211         Dryden Hunt /api/v1/people/8478211
    ## 23           77   8478366       Frank Vatrano /api/v1/people/8478366
    ## 24           33   8478470    Sam Montembeault /api/v1/people/8478470
    ## 25           55   8478569        Noel Acciari /api/v1/people/8478569
    ## 26           28   8478839      Aleksi Saarela /api/v1/people/8478839
    ## 27           61   8479388      Riley Stillman /api/v1/people/8479388
    ## 28           22   8479597       Chase Priskie /api/v1/people/8479597
    ## 29           74   8480015        Owen Tippett /api/v1/people/8480015
    ## 30           25   8481442        Brady Keeper /api/v1/people/8481442
    ##    position.code position.name position.type position.abbreviation
    ## 1              C        Center       Forward                     C
    ## 2              C        Center       Forward                     C
    ## 3              D    Defenseman    Defenseman                     D
    ## 4              D    Defenseman    Defenseman                     D
    ## 5              C        Center       Forward                     C
    ## 6              R    Right Wing       Forward                    RW
    ## 7              L     Left Wing       Forward                    LW
    ## 8              L     Left Wing       Forward                    LW
    ## 9              G        Goalie        Goalie                     G
    ## 10             R    Right Wing       Forward                    RW
    ## 11             D    Defenseman    Defenseman                     D
    ## 12             L     Left Wing       Forward                    LW
    ## 13             D    Defenseman    Defenseman                     D
    ## 14             G        Goalie        Goalie                     G
    ## 15             C        Center       Forward                     C
    ## 16             D    Defenseman    Defenseman                     D
    ## 17             D    Defenseman    Defenseman                     D
    ## 18             G        Goalie        Goalie                     G
    ## 19             C        Center       Forward                     C
    ## 20             D    Defenseman    Defenseman                     D
    ## 21             C        Center       Forward                     C
    ## 22             L     Left Wing       Forward                    LW
    ## 23             C        Center       Forward                     C
    ## 24             G        Goalie        Goalie                     G
    ## 25             C        Center       Forward                     C
    ## 26             C        Center       Forward                     C
    ## 27             D    Defenseman    Defenseman                     D
    ## 28             D    Defenseman    Defenseman                     D
    ## 29             R    Right Wing       Forward                    RW
    ## 30             D    Defenseman    Defenseman                     D
    ## 
    ## [[1]][[1]][[13]]
    ##    jerseyNumber person.id    person.fullName            person.link
    ## 1            35   8470147  Curtis McElhinney /api/v1/people/8470147
    ## 2            55   8470601     Braydon Coburn /api/v1/people/8470601
    ## 3            17   8473986       Alex Killorn /api/v1/people/8473986
    ## 4            22   8474031  Kevin Shattenkirk /api/v1/people/8474031
    ## 5            14   8474034         Pat Maroon /api/v1/people/8474034
    ## 6            27   8474151      Ryan McDonagh /api/v1/people/8474151
    ## 7            91   8474564     Steven Stamkos /api/v1/people/8474564
    ## 8            24   8474567      Zach Bogosian /api/v1/people/8474567
    ## 9             2   8474568        Luke Schenn /api/v1/people/8474568
    ## 10            9   8474870      Tyler Johnson /api/v1/people/8474870
    ## 11           77   8475167      Victor Hedman /api/v1/people/8475167
    ## 12           29   8475809    Scott Wedgewood /api/v1/people/8475809
    ## 13           18   8476292       Ondrej Palat /api/v1/people/8476292
    ## 14           20   8476399      Blake Coleman /api/v1/people/8476399
    ## 15           86   8476453    Nikita Kucherov /api/v1/people/8476453
    ## 16           19   8476624    Barclay Goodrow /api/v1/people/8476624
    ## 17           37   8476826       Yanni Gourde /api/v1/people/8476826
    ## 18           88   8476883 Andrei Vasilevskiy /api/v1/people/8476883
    ## 19           13   8476975    Cedric Paquette /api/v1/people/8476975
    ## 20           23   8477409   Carter Verhaeghe /api/v1/people/8477409
    ## 21           21   8478010      Brayden Point /api/v1/people/8478010
    ## 22           81   8478416        Erik Cernak /api/v1/people/8478416
    ## 23            7   8478472     Mathieu Joseph /api/v1/people/8478472
    ## 24           67   8478477  Mitchell Stephens /api/v1/people/8478477
    ## 25           71   8478519    Anthony Cirelli /api/v1/people/8478519
    ## 26           98   8479410  Mikhail Sergachev /api/v1/people/8479410
    ## 27           44   8480172          Jan Rutta /api/v1/people/8480172
    ##    position.code position.name position.type position.abbreviation
    ## 1              G        Goalie        Goalie                     G
    ## 2              D    Defenseman    Defenseman                     D
    ## 3              L     Left Wing       Forward                    LW
    ## 4              D    Defenseman    Defenseman                     D
    ## 5              L     Left Wing       Forward                    LW
    ## 6              D    Defenseman    Defenseman                     D
    ## 7              C        Center       Forward                     C
    ## 8              D    Defenseman    Defenseman                     D
    ## 9              D    Defenseman    Defenseman                     D
    ## 10             C        Center       Forward                     C
    ## 11             D    Defenseman    Defenseman                     D
    ## 12             G        Goalie        Goalie                     G
    ## 13             L     Left Wing       Forward                    LW
    ## 14             C        Center       Forward                     C
    ## 15             R    Right Wing       Forward                    RW
    ## 16             C        Center       Forward                     C
    ## 17             C        Center       Forward                     C
    ## 18             G        Goalie        Goalie                     G
    ## 19             C        Center       Forward                     C
    ## 20             C        Center       Forward                     C
    ## 21             C        Center       Forward                     C
    ## 22             D    Defenseman    Defenseman                     D
    ## 23             R    Right Wing       Forward                    RW
    ## 24             C        Center       Forward                     C
    ## 25             C        Center       Forward                     C
    ## 26             D    Defenseman    Defenseman                     D
    ## 27             D    Defenseman    Defenseman                     D
    ## 
    ## [[1]][[1]][[14]]
    ##    jerseyNumber person.id    person.fullName            person.link
    ## 1            63   8478063      Shane Gersich /api/v1/people/8478063
    ## 2            30   8478492      Ilya Samsonov /api/v1/people/8478492
    ## 3            40   8479516      Garrett Pilon /api/v1/people/8479516
    ## 4            27   8480823 Alexander Alexeyev /api/v1/people/8480823
    ## 5            17   8469454     Ilya Kovalchuk /api/v1/people/8469454
    ## 6             8   8471214      Alex Ovechkin /api/v1/people/8471214
    ## 7            77   8471698         T.J. Oshie /api/v1/people/8471698
    ## 8            19   8473563  Nicklas Backstrom /api/v1/people/8473563
    ## 9            62   8474176       Carl Hagelin /api/v1/people/8474176
    ## 10           20   8474189         Lars Eller /api/v1/people/8474189
    ## 11           74   8474590       John Carlson /api/v1/people/8474590
    ## 12           70   8474651      Braden Holtby /api/v1/people/8474651
    ## 13            9   8475200       Dmitry Orlov /api/v1/people/8475200
    ## 14           14   8475209      Richard Panik /api/v1/people/8475209
    ## 15            3   8475324        Nick Jensen /api/v1/people/8475324
    ## 16           26   8475343           Nic Dowd /api/v1/people/8475343
    ## 17            4   8475455     Brenden Dillon /api/v1/people/8475455
    ## 18           33   8475462        Radko Gudas /api/v1/people/8475462
    ## 19           92   8475744   Evgeny Kuznetsov /api/v1/people/8475744
    ## 20           72   8476329        Travis Boyd /api/v1/people/8476329
    ## 21           43   8476880         Tom Wilson /api/v1/people/8476880
    ## 22           64   8477314        Brian Pinho /api/v1/people/8477314
    ## 23           78   8477343    Tyler Lewington /api/v1/people/8477343
    ## 24           16   8477544   Philippe Maillet /api/v1/people/8477544
    ## 25            1   8477831     Pheonix Copley /api/v1/people/8477831
    ## 26           21   8477903    Garnet Hathaway /api/v1/people/8477903
    ## 27           13   8477944        Jakub Vrana /api/v1/people/8477944
    ## 28           41   8477970      Vitek Vanecek /api/v1/people/8477970
    ## 29           34   8478399 Jonas Siegenthaler /api/v1/people/8478399
    ## 30           10   8478466      Daniel Sprong /api/v1/people/8478466
    ## 31           47   8479359     Beck Malenstyn /api/v1/people/8479359
    ## 32            6   8479482      Michal Kempny /api/v1/people/8479482
    ## 33           42   8480796   Martin Fehervary /api/v1/people/8480796
    ## 34           24   8481580   Connor McMichael /api/v1/people/8481580
    ##    position.code position.name position.type position.abbreviation
    ## 1              L     Left Wing       Forward                    LW
    ## 2              G        Goalie        Goalie                     G
    ## 3              C        Center       Forward                     C
    ## 4              D    Defenseman    Defenseman                     D
    ## 5              L     Left Wing       Forward                    LW
    ## 6              L     Left Wing       Forward                    LW
    ## 7              R    Right Wing       Forward                    RW
    ## 8              C        Center       Forward                     C
    ## 9              L     Left Wing       Forward                    LW
    ## 10             C        Center       Forward                     C
    ## 11             D    Defenseman    Defenseman                     D
    ## 12             G        Goalie        Goalie                     G
    ## 13             D    Defenseman    Defenseman                     D
    ## 14             R    Right Wing       Forward                    RW
    ## 15             D    Defenseman    Defenseman                     D
    ## 16             C        Center       Forward                     C
    ## 17             D    Defenseman    Defenseman                     D
    ## 18             D    Defenseman    Defenseman                     D
    ## 19             C        Center       Forward                     C
    ## 20             C        Center       Forward                     C
    ## 21             R    Right Wing       Forward                    RW
    ## 22             C        Center       Forward                     C
    ## 23             D    Defenseman    Defenseman                     D
    ## 24             C        Center       Forward                     C
    ## 25             G        Goalie        Goalie                     G
    ## 26             R    Right Wing       Forward                    RW
    ## 27             L     Left Wing       Forward                    LW
    ## 28             G        Goalie        Goalie                     G
    ## 29             D    Defenseman    Defenseman                     D
    ## 30             R    Right Wing       Forward                    RW
    ## 31             L     Left Wing       Forward                    LW
    ## 32             D    Defenseman    Defenseman                     D
    ## 33             D    Defenseman    Defenseman                     D
    ## 34             C        Center       Forward                     C
    ## 
    ## [[1]][[1]][[15]]
    ##    jerseyNumber person.id     person.fullName            person.link
    ## 1             7   8470607      Brent Seabrook /api/v1/people/8470607
    ## 2            65   8476381         Andrew Shaw /api/v1/people/8476381
    ## 3            90   8477035        Matt Tomkins /api/v1/people/8477035
    ## 4            43   8479342           Chad Krys /api/v1/people/8479342
    ## 5            71   8480798    Philipp Kurashev /api/v1/people/8480798
    ## 6            52   8481147       Reese Johnson /api/v1/people/8481147
    ## 7             2   8470281        Duncan Keith /api/v1/people/8470281
    ## 8            50   8470645      Corey Crawford /api/v1/people/8470645
    ## 9            19   8473604      Jonathan Toews /api/v1/people/8473604
    ## 10           88   8474141        Patrick Kane /api/v1/people/8474141
    ## 11           44   8475177      Calvin de Haan /api/v1/people/8475177
    ## 12           55   8476372         Nick Seeler /api/v1/people/8476372
    ## 13           20   8476438        Brandon Saad /api/v1/people/8476438
    ## 14            5   8476473       Connor Murphy /api/v1/people/8476473
    ## 15            6   8476874         Olli Maatta /api/v1/people/8476874
    ## 16           30   8476876      Malcolm Subban /api/v1/people/8476876
    ## 17           68   8476886     Slater Koekkoek /api/v1/people/8476886
    ## 18            8   8477330     Dominik Kubalik /api/v1/people/8477330
    ## 19           22   8477846      Ryan Carpenter /api/v1/people/8477846
    ## 20           47   8477961    John Quenneville /api/v1/people/8477961
    ## 21           95   8478106        Dylan Sikura /api/v1/people/8478106
    ## 22           36   8478146    Matthew Highmore /api/v1/people/8478146
    ## 23           17   8478440        Dylan Strome /api/v1/people/8478440
    ## 24           12   8479337      Alex DeBrincat /api/v1/people/8479337
    ## 25           92   8479423       Alex Nylander /api/v1/people/8479423
    ## 26           91   8479465      Drake Caggiula /api/v1/people/8479465
    ## 27           46   8479523      Lucas Carlsson /api/v1/people/8479523
    ## 28           38   8479542       Brandon Hagel /api/v1/people/8479542
    ## 29           58   8480025 MacKenzie Entwistle /api/v1/people/8480025
    ## 30           64   8480144         David Kampf /api/v1/people/8480144
    ## 31           60   8480420        Collin Delia /api/v1/people/8480420
    ## 32           74   8480814     Nicolas Beaudin /api/v1/people/8480814
    ## 33           75   8480831         Alec Regula /api/v1/people/8480831
    ## 34           27   8480871        Adam Boqvist /api/v1/people/8480871
    ## 35           34   8480947      Kevin Lankinen /api/v1/people/8480947
    ## 36           77   8481523          Kirby Dach /api/v1/people/8481523
    ##    position.code position.name position.type position.abbreviation
    ## 1              D    Defenseman    Defenseman                     D
    ## 2              R    Right Wing       Forward                    RW
    ## 3              G        Goalie        Goalie                     G
    ## 4              D    Defenseman    Defenseman                     D
    ## 5              C        Center       Forward                     C
    ## 6              C        Center       Forward                     C
    ## 7              D    Defenseman    Defenseman                     D
    ## 8              G        Goalie        Goalie                     G
    ## 9              C        Center       Forward                     C
    ## 10             R    Right Wing       Forward                    RW
    ## 11             D    Defenseman    Defenseman                     D
    ## 12             D    Defenseman    Defenseman                     D
    ## 13             L     Left Wing       Forward                    LW
    ## 14             D    Defenseman    Defenseman                     D
    ## 15             D    Defenseman    Defenseman                     D
    ## 16             G        Goalie        Goalie                     G
    ## 17             D    Defenseman    Defenseman                     D
    ## 18             L     Left Wing       Forward                    LW
    ## 19             C        Center       Forward                     C
    ## 20             C        Center       Forward                     C
    ## 21             R    Right Wing       Forward                    RW
    ## 22             C        Center       Forward                     C
    ## 23             C        Center       Forward                     C
    ## 24             L     Left Wing       Forward                    LW
    ## 25             L     Left Wing       Forward                    LW
    ## 26             C        Center       Forward                     C
    ## 27             D    Defenseman    Defenseman                     D
    ## 28             L     Left Wing       Forward                    LW
    ## 29             R    Right Wing       Forward                    RW
    ## 30             C        Center       Forward                     C
    ## 31             G        Goalie        Goalie                     G
    ## 32             D    Defenseman    Defenseman                     D
    ## 33             D    Defenseman    Defenseman                     D
    ## 34             D    Defenseman    Defenseman                     D
    ## 35             G        Goalie        Goalie                     G
    ## 36             C        Center       Forward                     C
    ## 
    ## [[1]][[1]][[16]]
    ##    jerseyNumber person.id   person.fullName            person.link
    ## 1            65   8477215    Danny DeKeyser /api/v1/people/8477215
    ## 2            21   8479395  Dennis Cholowski /api/v1/people/8479395
    ## 3            17   8479425      Filip Hronek /api/v1/people/8479425
    ## 4            28   8480184  Gustav Lindstrom /api/v1/people/8480184
    ## 5            51   8470047 Valtteri Filppula /api/v1/people/8470047
    ## 6            83   8470110      Trevor Daley /api/v1/people/8470110
    ## 7            81   8470144     Frans Nielsen /api/v1/people/8470144
    ## 8            52   8470318 Jonathan Ericsson /api/v1/people/8470318
    ## 9            35   8470657      Jimmy Howard /api/v1/people/8470657
    ## 10            8   8471716 Justin Abdelkader /api/v1/people/8471716
    ## 11           43   8471794       Darren Helm /api/v1/people/8471794
    ## 12            3   8473415        Alex Biega /api/v1/people/8473415
    ## 13           45   8473541  Jonathan Bernier /api/v1/people/8473541
    ## 14           89   8474040        Sam Gagner /api/v1/people/8474040
    ## 15           26   8474597     Cody Goloubef /api/v1/people/8474597
    ## 16           22   8475747     Patrik Nemeth /api/v1/people/8475747
    ## 17           41   8476822   Luke Glendening /api/v1/people/8476822
    ## 18           73   8477454         Adam Erne /api/v1/people/8477454
    ## 19           74   8477474     Madison Bowey /api/v1/people/8477474
    ## 20           59   8477479    Tyler Bertuzzi /api/v1/people/8477479
    ## 21           39   8477511    Anthony Mantha /api/v1/people/8477511
    ## 22           29   8477943   Brendan Perlini /api/v1/people/8477943
    ## 23           71   8477946      Dylan Larkin /api/v1/people/8477946
    ## 24           14   8477952      Robby Fabbri /api/v1/people/8477952
    ## 25           70   8478036   Christoffer Ehn /api/v1/people/8478036
    ## 26           15   8478857   Dmytro Timashov /api/v1/people/8478857
    ##    position.code position.name position.type position.abbreviation
    ## 1              D    Defenseman    Defenseman                     D
    ## 2              D    Defenseman    Defenseman                     D
    ## 3              D    Defenseman    Defenseman                     D
    ## 4              D    Defenseman    Defenseman                     D
    ## 5              C        Center       Forward                     C
    ## 6              D    Defenseman    Defenseman                     D
    ## 7              C        Center       Forward                     C
    ## 8              D    Defenseman    Defenseman                     D
    ## 9              G        Goalie        Goalie                     G
    ## 10             L     Left Wing       Forward                    LW
    ## 11             L     Left Wing       Forward                    LW
    ## 12             D    Defenseman    Defenseman                     D
    ## 13             G        Goalie        Goalie                     G
    ## 14             C        Center       Forward                     C
    ## 15             D    Defenseman    Defenseman                     D
    ## 16             D    Defenseman    Defenseman                     D
    ## 17             C        Center       Forward                     C
    ## 18             L     Left Wing       Forward                    LW
    ## 19             D    Defenseman    Defenseman                     D
    ## 20             L     Left Wing       Forward                    LW
    ## 21             R    Right Wing       Forward                    RW
    ## 22             L     Left Wing       Forward                    LW
    ## 23             C        Center       Forward                     C
    ## 24             C        Center       Forward                     C
    ## 25             C        Center       Forward                     C
    ## 26             L     Left Wing       Forward                    LW
    ## 
    ## [[1]][[1]][[17]]
    ##    jerseyNumber person.id   person.fullName            person.link
    ## 1            28   8480009     Eeli Tolvanen /api/v1/people/8480009
    ## 2             5   8469465       Dan Hamhuis /api/v1/people/8469465
    ## 3            35   8471469       Pekka Rinne /api/v1/people/8471469
    ## 4            22   8473560  Korbinian Holzer /api/v1/people/8473560
    ## 5            13   8474009       Nick Bonino /api/v1/people/8474009
    ## 6             8   8474068       Kyle Turris /api/v1/people/8474068
    ## 7             7   8474134     Yannick Weber /api/v1/people/8474134
    ## 8            59   8474600        Roman Josi /api/v1/people/8474600
    ## 9            95   8475168      Matt Duchene /api/v1/people/8475168
    ## 10            4   8475176        Ryan Ellis /api/v1/people/8475176
    ## 11           14   8475218    Mattias Ekholm /api/v1/people/8475218
    ## 12           15   8475225       Craig Smith /api/v1/people/8475225
    ## 13           19   8475714    Calle Jarnkrok /api/v1/people/8475714
    ## 14           51   8475766     Austin Watson /api/v1/people/8475766
    ## 15           92   8475793     Ryan Johansen /api/v1/people/8475793
    ## 16           24   8475797    Jarred Tinordi /api/v1/people/8475797
    ## 17           64   8475798   Mikael Granlund /api/v1/people/8475798
    ## 18           42   8476278   Colin Blackwell /api/v1/people/8476278
    ## 19           23   8476428    Rocco Grimaldi /api/v1/people/8476428
    ## 20            9   8476887    Filip Forsberg /api/v1/people/8476887
    ## 21           10   8476925    Colton Sissons /api/v1/people/8476925
    ## 22            1   8477234    Troy Grosenick /api/v1/people/8477234
    ## 23           74   8477424       Juuse Saros /api/v1/people/8477424
    ## 24           47   8477446  Michael McCarron /api/v1/people/8477446
    ## 25           26   8477901       Daniel Carr /api/v1/people/8477901
    ## 26           33   8478042  Viktor Arvidsson /api/v1/people/8478042
    ## 27           32   8478508      Yakov Trenin /api/v1/people/8478508
    ## 28           45   8478851 Alexandre Carrier /api/v1/people/8478851
    ## 29           39   8478971     Connor Ingram /api/v1/people/8478971
    ## 30           57   8479371      Dante Fabbro /api/v1/people/8479371
    ##    position.code position.name position.type position.abbreviation
    ## 1              R    Right Wing       Forward                    RW
    ## 2              D    Defenseman    Defenseman                     D
    ## 3              G        Goalie        Goalie                     G
    ## 4              D    Defenseman    Defenseman                     D
    ## 5              C        Center       Forward                     C
    ## 6              C        Center       Forward                     C
    ## 7              D    Defenseman    Defenseman                     D
    ## 8              D    Defenseman    Defenseman                     D
    ## 9              C        Center       Forward                     C
    ## 10             D    Defenseman    Defenseman                     D
    ## 11             D    Defenseman    Defenseman                     D
    ## 12             R    Right Wing       Forward                    RW
    ## 13             C        Center       Forward                     C
    ## 14             L     Left Wing       Forward                    LW
    ## 15             C        Center       Forward                     C
    ## 16             D    Defenseman    Defenseman                     D
    ## 17             C        Center       Forward                     C
    ## 18             C        Center       Forward                     C
    ## 19             R    Right Wing       Forward                    RW
    ## 20             L     Left Wing       Forward                    LW
    ## 21             C        Center       Forward                     C
    ## 22             G        Goalie        Goalie                     G
    ## 23             G        Goalie        Goalie                     G
    ## 24             R    Right Wing       Forward                    RW
    ## 25             L     Left Wing       Forward                    LW
    ## 26             R    Right Wing       Forward                    RW
    ## 27             C        Center       Forward                     C
    ## 28             D    Defenseman    Defenseman                     D
    ## 29             G        Goalie        Goalie                     G
    ## 30             D    Defenseman    Defenseman                     D
    ## 
    ## [[1]][[1]][[18]]
    ##    jerseyNumber person.id      person.fullName            person.link
    ## 1            19   8470151      Jay Bouwmeester /api/v1/people/8470151
    ## 2            20   8470257      Alexander Steen /api/v1/people/8470257
    ## 3            36   8471426         Troy Brouwer /api/v1/people/8471426
    ## 4            57   8474102         David Perron /api/v1/people/8474102
    ## 5             4   8474125      Carl Gunnarsson /api/v1/people/8474125
    ## 6            41   8474145      Robert Bortuzzo /api/v1/people/8474145
    ## 7            27   8474565     Alex Pietrangelo /api/v1/people/8474565
    ## 8             6   8474618      Marco Scandella /api/v1/people/8474618
    ## 9            21   8475098          Tyler Bozak /api/v1/people/8475098
    ## 10           90   8475158        Ryan O'Reilly /api/v1/people/8475158
    ## 11           10   8475170       Brayden Schenn /api/v1/people/8475170
    ## 12           72   8475753         Justin Faulk /api/v1/people/8475753
    ## 13           91   8475765   Vladimir Tarasenko /api/v1/people/8475765
    ## 14           17   8475768       Jaden Schwartz /api/v1/people/8475768
    ## 15           50   8476412    Jordan Binnington /api/v1/people/8476412
    ## 16           51   8476884      Derrick Pouliot /api/v1/people/8476884
    ## 17           55   8476892       Colton Parayko /api/v1/people/8476892
    ## 18           70   8476897      Oskar Sundqvist /api/v1/people/8476897
    ## 19           28   8476907 Mackenzie MacEachern /api/v1/people/8476907
    ## 20           61   8477455     Jacob de la Rose /api/v1/people/8477455
    ## 21           12   8477482         Zach Sanford /api/v1/people/8477482
    ## 22           49   8477964       Ivan Barbashev /api/v1/people/8477964
    ## 23           46   8478013          Jake Walman /api/v1/people/8478013
    ## 24           35   8478024          Ville Husso /api/v1/people/8478024
    ## 25           53   8478040      Austin Poganski /api/v1/people/8478040
    ## 26            9   8478104          Sammy Blais /api/v1/people/8478104
    ## 27           29   8478407           Vince Dunn /api/v1/people/8478407
    ## 28           77   8478859         Niko Mikkola /api/v1/people/8478859
    ## 29           33   8479385         Jordan Kyrou /api/v1/people/8479385
    ## 30           37   8480011          Klim Kostin /api/v1/people/8480011
    ## 31           18   8480023        Robert Thomas /api/v1/people/8480023
    ##    position.code position.name position.type position.abbreviation
    ## 1              D    Defenseman    Defenseman                     D
    ## 2              L     Left Wing       Forward                    LW
    ## 3              R    Right Wing       Forward                    RW
    ## 4              L     Left Wing       Forward                    LW
    ## 5              D    Defenseman    Defenseman                     D
    ## 6              D    Defenseman    Defenseman                     D
    ## 7              D    Defenseman    Defenseman                     D
    ## 8              D    Defenseman    Defenseman                     D
    ## 9              C        Center       Forward                     C
    ## 10             C        Center       Forward                     C
    ## 11             C        Center       Forward                     C
    ## 12             D    Defenseman    Defenseman                     D
    ## 13             R    Right Wing       Forward                    RW
    ## 14             L     Left Wing       Forward                    LW
    ## 15             G        Goalie        Goalie                     G
    ## 16             D    Defenseman    Defenseman                     D
    ## 17             D    Defenseman    Defenseman                     D
    ## 18             C        Center       Forward                     C
    ## 19             L     Left Wing       Forward                    LW
    ## 20             L     Left Wing       Forward                    LW
    ## 21             L     Left Wing       Forward                    LW
    ## 22             C        Center       Forward                     C
    ## 23             D    Defenseman    Defenseman                     D
    ## 24             G        Goalie        Goalie                     G
    ## 25             R    Right Wing       Forward                    RW
    ## 26             L     Left Wing       Forward                    LW
    ## 27             D    Defenseman    Defenseman                     D
    ## 28             D    Defenseman    Defenseman                     D
    ## 29             C        Center       Forward                     C
    ## 30             C        Center       Forward                     C
    ## 31             C        Center       Forward                     C
    ## 
    ## [[1]][[1]][[19]]
    ##    jerseyNumber person.id   person.fullName            person.link
    ## 1            24   8474612    Travis Hamonic /api/v1/people/8474612
    ## 2             8   8479976    Juuso Valimaki /api/v1/people/8479976
    ## 3             5   8470966     Mark Giordano /api/v1/people/8470966
    ## 4            17   8473473       Milan Lucic /api/v1/people/8473473
    ## 5            11   8474150   Mikael Backlund /api/v1/people/8474150
    ## 6            26   8474628     Michael Stone /api/v1/people/8474628
    ## 7             7   8474673         TJ Brodie /api/v1/people/8474673
    ## 8            36   8474736       Zac Rinaldo /api/v1/people/8474736
    ## 9            38   8475278      Byron Froese /api/v1/people/8475278
    ## 10           39   8475660        Cam Talbot /api/v1/people/8475660
    ## 11           20   8475762     Derek Forbort /api/v1/people/8475762
    ## 12           13   8476346   Johnny Gaudreau /api/v1/people/8476346
    ## 13           16   8476356     Tobias Rieder /api/v1/people/8476356
    ## 14           89   8476409        Alan Quine /api/v1/people/8476409
    ## 15           77   8476873    Mark Jankowski /api/v1/people/8476873
    ## 16           32   8476903       Jon Gillies /api/v1/people/8476903
    ## 17           56   8476979   Erik Gustafsson /api/v1/people/8476979
    ## 18           53   8477210    Buddy Robinson /api/v1/people/8477210
    ## 19           28   8477496    Elias Lindholm /api/v1/people/8477496
    ## 20           23   8477497      Sean Monahan /api/v1/people/8477497
    ## 21           93   8477935       Sam Bennett /api/v1/people/8477935
    ## 22           88   8478233 Andrew Mangiapane /api/v1/people/8478233
    ## 23           55   8478396      Noah Hanifin /api/v1/people/8478396
    ## 24            4   8478397  Rasmus Andersson /api/v1/people/8478397
    ## 25           58   8478430  Oliver Kylington /api/v1/people/8478430
    ## 26           27   8478512    Austin Czarnik /api/v1/people/8478512
    ## 27           10   8478585        Derek Ryan /api/v1/people/8478585
    ## 28           19   8479314   Matthew Tkachuk /api/v1/people/8479314
    ## 29           29   8479346       Dillon Dube /api/v1/people/8479346
    ## 30           33   8479496     David Rittich /api/v1/people/8479496
    ## 31           50   8481501  Artyom Zagidulin /api/v1/people/8481501
    ## 32           45   8481630 Alexander Yelesin /api/v1/people/8481630
    ##    position.code position.name position.type position.abbreviation
    ## 1              D    Defenseman    Defenseman                     D
    ## 2              D    Defenseman    Defenseman                     D
    ## 3              D    Defenseman    Defenseman                     D
    ## 4              L     Left Wing       Forward                    LW
    ## 5              C        Center       Forward                     C
    ## 6              D    Defenseman    Defenseman                     D
    ## 7              D    Defenseman    Defenseman                     D
    ## 8              C        Center       Forward                     C
    ## 9              C        Center       Forward                     C
    ## 10             G        Goalie        Goalie                     G
    ## 11             D    Defenseman    Defenseman                     D
    ## 12             L     Left Wing       Forward                    LW
    ## 13             C        Center       Forward                     C
    ## 14             C        Center       Forward                     C
    ## 15             C        Center       Forward                     C
    ## 16             G        Goalie        Goalie                     G
    ## 17             D    Defenseman    Defenseman                     D
    ## 18             R    Right Wing       Forward                    RW
    ## 19             C        Center       Forward                     C
    ## 20             C        Center       Forward                     C
    ## 21             C        Center       Forward                     C
    ## 22             L     Left Wing       Forward                    LW
    ## 23             D    Defenseman    Defenseman                     D
    ## 24             D    Defenseman    Defenseman                     D
    ## 25             D    Defenseman    Defenseman                     D
    ## 26             C        Center       Forward                     C
    ## 27             C        Center       Forward                     C
    ## 28             L     Left Wing       Forward                    LW
    ## 29             C        Center       Forward                     C
    ## 30             G        Goalie        Goalie                     G
    ## 31             G        Goalie        Goalie                     G
    ## 32             D    Defenseman    Defenseman                     D
    ## 
    ## [[1]][[1]][[20]]
    ##    jerseyNumber person.id          person.fullName            person.link
    ## 1            22   8474569             Colin Wilson /api/v1/people/8474569
    ## 2             6   8473446             Erik Johnson /api/v1/people/8473446
    ## 3            28   8474013                 Ian Cole /api/v1/people/8474013
    ## 4            35   8474636       Michael Hutchinson /api/v1/people/8474636
    ## 5            11   8474685             Matt Calvert /api/v1/people/8474685
    ## 6            44   8474717            Mark Barberio /api/v1/people/8474717
    ## 7            91   8475172              Nazem Kadri /api/v1/people/8475172
    ## 8             7   8475246          Kevin Connauton /api/v1/people/8475246
    ## 9            72   8475820           Joonas Donskoi /api/v1/people/8475820
    ## 10           31   8475831         Philipp Grubauer /api/v1/people/8475831
    ## 11           36   8476391               T.J. Tynan /api/v1/people/8476391
    ## 12           83   8476442               Matt Nieto /api/v1/people/8476442
    ## 13           92   8476455        Gabriel Landeskog /api/v1/people/8476455
    ## 14           90   8476480    Vladislav Namestnikov /api/v1/people/8476480
    ## 15           27   8477435              Ryan Graves /api/v1/people/8477435
    ## 16           95   8477444         Andre Burakovsky /api/v1/people/8477444
    ## 17           37   8477456             J.T. Compher /api/v1/people/8477456
    ## 18           29   8477492         Nathan MacKinnon /api/v1/people/8477492
    ## 19           13   8477501        Valeri Nichushkin /api/v1/people/8477501
    ## 20           16   8477507           Nikita Zadorov /api/v1/people/8477507
    ## 21           41   8477930 Pierre-Edouard Bellemare /api/v1/people/8477930
    ## 22           54   8478073           Anton Lindholm /api/v1/people/8478073
    ## 23           96   8478420           Mikko Rantanen /api/v1/people/8478420
    ## 24           17   8479370               Tyson Jost /api/v1/people/8479370
    ## 25           49   8479398            Samuel Girard /api/v1/people/8479398
    ## 26           20   8479982            Conor Timmins /api/v1/people/8479982
    ## 27           14   8480032             Shane Bowers /api/v1/people/8480032
    ## 28            8   8480069               Cale Makar /api/v1/people/8480069
    ## 29           32   8480112             Hunter Miska /api/v1/people/8480112
    ## 30           15   8480326            Sheldon Dries /api/v1/people/8480326
    ## 31           39   8480925           Pavel Francouz /api/v1/people/8480925
    ## 32           25   8481186           Logan O'Connor /api/v1/people/8481186
    ## 33           45   8481524              Bowen Byram /api/v1/people/8481524
    ##    position.code position.name position.type position.abbreviation
    ## 1              C        Center       Forward                     C
    ## 2              D    Defenseman    Defenseman                     D
    ## 3              D    Defenseman    Defenseman                     D
    ## 4              G        Goalie        Goalie                     G
    ## 5              L     Left Wing       Forward                    LW
    ## 6              D    Defenseman    Defenseman                     D
    ## 7              C        Center       Forward                     C
    ## 8              D    Defenseman    Defenseman                     D
    ## 9              R    Right Wing       Forward                    RW
    ## 10             G        Goalie        Goalie                     G
    ## 11             C        Center       Forward                     C
    ## 12             L     Left Wing       Forward                    LW
    ## 13             L     Left Wing       Forward                    LW
    ## 14             C        Center       Forward                     C
    ## 15             D    Defenseman    Defenseman                     D
    ## 16             L     Left Wing       Forward                    LW
    ## 17             L     Left Wing       Forward                    LW
    ## 18             C        Center       Forward                     C
    ## 19             R    Right Wing       Forward                    RW
    ## 20             D    Defenseman    Defenseman                     D
    ## 21             C        Center       Forward                     C
    ## 22             D    Defenseman    Defenseman                     D
    ## 23             R    Right Wing       Forward                    RW
    ## 24             C        Center       Forward                     C
    ## 25             D    Defenseman    Defenseman                     D
    ## 26             D    Defenseman    Defenseman                     D
    ## 27             C        Center       Forward                     C
    ## 28             D    Defenseman    Defenseman                     D
    ## 29             G        Goalie        Goalie                     G
    ## 30             C        Center       Forward                     C
    ## 31             G        Goalie        Goalie                     G
    ## 32             R    Right Wing       Forward                    RW
    ## 33             D    Defenseman    Defenseman                     D
    ## 
    ## [[1]][[1]][[21]]
    ##    jerseyNumber person.id     person.fullName            person.link
    ## 1            70   8480802         Ryan McLeod /api/v1/people/8480802
    ## 2            75   8480803       Evan Bouchard /api/v1/people/8480803
    ## 3            86   8481598      Philip Broberg /api/v1/people/8481598
    ## 4            10   8481638       Joakim Nygard /api/v1/people/8481638
    ## 5            91   8481813         Gaetan Haas /api/v1/people/8481813
    ## 6            41   8469608          Mike Smith /api/v1/people/8469608
    ## 7            18   8471707          James Neal /api/v1/people/8471707
    ## 8             4   8471729        Kris Russell /api/v1/people/8471729
    ## 9            63   8474589         Tyler Ennis /api/v1/people/8474589
    ## 10           19   8475156      Mikko Koskinen /api/v1/people/8475156
    ## 11           39   8475163       Alex Chiasson /api/v1/people/8475163
    ## 12           44   8475178        Zack Kassian /api/v1/people/8475178
    ## 13           23   8475772       Riley Sheahan /api/v1/people/8475772
    ## 14           15   8476326      Josh Archibald /api/v1/people/8476326
    ## 15           93   8476454 Ryan Nugent-Hopkins /api/v1/people/8476454
    ## 16            6   8476457        Adam Larsson /api/v1/people/8476457
    ## 17           77   8476472       Oscar Klefbom /api/v1/people/8476472
    ## 18           16   8476915       Jujhar Khaira /api/v1/people/8476915
    ## 19           28   8476960  Andreas Athanasiou /api/v1/people/8476960
    ## 20           83   8476988        Matt Benning /api/v1/people/8476988
    ## 21           25   8477498       Darnell Nurse /api/v1/people/8477498
    ## 22           29   8477934      Leon Draisaitl /api/v1/people/8477934
    ## 23           84   8478021    William Lagesson /api/v1/people/8478021
    ## 24           97   8478402      Connor McDavid /api/v1/people/8478402
    ## 25           65   8478442       Cooper Marody /api/v1/people/8478442
    ## 26           74   8478451          Ethan Bear /api/v1/people/8478451
    ## 27           82   8478452         Caleb Jones /api/v1/people/8478452
    ## 28           49   8479347        Tyler Benson /api/v1/people/8479347
    ## 29           52   8479466     Patrick Russell /api/v1/people/8479466
    ## 30           50   8479973      Stuart Skinner /api/v1/people/8479973
    ## 31           56   8479977     Kailer Yamamoto /api/v1/people/8479977
    ##    position.code position.name position.type position.abbreviation
    ## 1              C        Center       Forward                     C
    ## 2              D    Defenseman    Defenseman                     D
    ## 3              D    Defenseman    Defenseman                     D
    ## 4              L     Left Wing       Forward                    LW
    ## 5              C        Center       Forward                     C
    ## 6              G        Goalie        Goalie                     G
    ## 7              L     Left Wing       Forward                    LW
    ## 8              D    Defenseman    Defenseman                     D
    ## 9              C        Center       Forward                     C
    ## 10             G        Goalie        Goalie                     G
    ## 11             R    Right Wing       Forward                    RW
    ## 12             R    Right Wing       Forward                    RW
    ## 13             C        Center       Forward                     C
    ## 14             R    Right Wing       Forward                    RW
    ## 15             C        Center       Forward                     C
    ## 16             D    Defenseman    Defenseman                     D
    ## 17             D    Defenseman    Defenseman                     D
    ## 18             L     Left Wing       Forward                    LW
    ## 19             L     Left Wing       Forward                    LW
    ## 20             D    Defenseman    Defenseman                     D
    ## 21             D    Defenseman    Defenseman                     D
    ## 22             C        Center       Forward                     C
    ## 23             D    Defenseman    Defenseman                     D
    ## 24             C        Center       Forward                     C
    ## 25             C        Center       Forward                     C
    ## 26             D    Defenseman    Defenseman                     D
    ## 27             D    Defenseman    Defenseman                     D
    ## 28             L     Left Wing       Forward                    LW
    ## 29             R    Right Wing       Forward                    RW
    ## 30             G        Goalie        Goalie                     G
    ## 31             R    Right Wing       Forward                    RW
    ## 
    ## [[1]][[1]][[22]]
    ##    jerseyNumber person.id   person.fullName            person.link
    ## 1            21   8470626     Loui Eriksson /api/v1/people/8470626
    ## 2            23   8471303   Alexander Edler /api/v1/people/8471303
    ## 3            20   8474091    Brandon Sutter /api/v1/people/8474091
    ## 4            83   8474291        Jay Beagle /api/v1/people/8474291
    ## 5            57   8474574       Tyler Myers /api/v1/people/8474574
    ## 6            25   8474593   Jacob Markstrom /api/v1/people/8474593
    ## 7             4   8474818       Jordie Benn /api/v1/people/8474818
    ## 8            26   8474849   Antoine Roussel /api/v1/people/8474849
    ## 9             8   8475690 Christopher Tanev /api/v1/people/8475690
    ## 10           73   8475726     Tyler Toffoli /api/v1/people/8475726
    ## 11           30   8475839    Louis Domingue /api/v1/people/8475839
    ## 12           79   8475907   Micheal Ferland /api/v1/people/8475907
    ## 13           44   8476344     Tyler Graovac /api/v1/people/8476344
    ## 14            9   8476468       J.T. Miller /api/v1/people/8476468
    ## 15           70   8476871    Tanner Pearson /api/v1/people/8476871
    ## 16           64   8477353       Tyler Motte /api/v1/people/8477353
    ## 17           38   8477473     Justin Bailey /api/v1/people/8477473
    ## 18           53   8477500         Bo Horvat /api/v1/people/8477500
    ## 19           18   8477937     Jake Virtanen /api/v1/people/8477937
    ## 20           35   8477967    Thatcher Demko /api/v1/people/8477967
    ## 21            6   8478444      Brock Boeser /api/v1/people/8478444
    ## 22           88   8478874     Adam Gaudette /api/v1/people/8478874
    ## 23           63   8478970   Jalen Chatfield /api/v1/people/8478970
    ## 24           48   8479355      Olli Juolevi /api/v1/people/8479355
    ## 25           51   8479442      Troy Stecher /api/v1/people/8479442
    ## 26           71   8479772      Zack MacEwen /api/v1/people/8479772
    ## 27           40   8480012  Elias Pettersson /api/v1/people/8480012
    ## 28            5   8480147  Oscar Fantenberg /api/v1/people/8480147
    ## 29           43   8480800      Quinn Hughes /api/v1/people/8480800
    ## 30           30   8481478       Jake Kielly /api/v1/people/8481478
    ## 31            3   8481479   Brogan Rafferty /api/v1/people/8481479
    ##    position.code position.name position.type position.abbreviation
    ## 1              L     Left Wing       Forward                    LW
    ## 2              D    Defenseman    Defenseman                     D
    ## 3              C        Center       Forward                     C
    ## 4              C        Center       Forward                     C
    ## 5              D    Defenseman    Defenseman                     D
    ## 6              G        Goalie        Goalie                     G
    ## 7              D    Defenseman    Defenseman                     D
    ## 8              L     Left Wing       Forward                    LW
    ## 9              D    Defenseman    Defenseman                     D
    ## 10             R    Right Wing       Forward                    RW
    ## 11             G        Goalie        Goalie                     G
    ## 12             L     Left Wing       Forward                    LW
    ## 13             C        Center       Forward                     C
    ## 14             C        Center       Forward                     C
    ## 15             L     Left Wing       Forward                    LW
    ## 16             C        Center       Forward                     C
    ## 17             R    Right Wing       Forward                    RW
    ## 18             C        Center       Forward                     C
    ## 19             R    Right Wing       Forward                    RW
    ## 20             G        Goalie        Goalie                     G
    ## 21             R    Right Wing       Forward                    RW
    ## 22             C        Center       Forward                     C
    ## 23             D    Defenseman    Defenseman                     D
    ## 24             D    Defenseman    Defenseman                     D
    ## 25             D    Defenseman    Defenseman                     D
    ## 26             C        Center       Forward                     C
    ## 27             C        Center       Forward                     C
    ## 28             D    Defenseman    Defenseman                     D
    ## 29             D    Defenseman    Defenseman                     D
    ## 30             G        Goalie        Goalie                     G
    ## 31             D    Defenseman    Defenseman                     D
    ## 
    ## [[1]][[1]][[23]]
    ##    jerseyNumber person.id     person.fullName            person.link
    ## 1            30   8468011         Ryan Miller /api/v1/people/8468011
    ## 2            15   8470612        Ryan Getzlaf /api/v1/people/8470612
    ## 3            21   8470655        David Backes /api/v1/people/8470655
    ## 4            44   8474584   Michael Del Zotto /api/v1/people/8474584
    ## 5            14   8474641       Adam Henrique /api/v1/people/8474641
    ## 6            33   8475164   Jakob Silfverberg /api/v1/people/8475164
    ## 7            20   8475235 Nicolas Deslauriers /api/v1/people/8475235
    ## 8            26   8475461     Andrew Agozzino /api/v1/people/8475461
    ## 9            52   8475625          Matt Irwin /api/v1/people/8475625
    ## 10            4   8475764          Cam Fowler /api/v1/people/8475764
    ## 11            6   8475790     Erik Gudbranson /api/v1/people/8475790
    ## 12           42   8476312         Josh Manson /api/v1/people/8476312
    ## 13           36   8476434         John Gibson /api/v1/people/8476434
    ## 14           67   8476483      Rickard Rakell /api/v1/people/8476483
    ## 15           47   8476854     Hampus Lindholm /api/v1/people/8476854
    ## 16           29   8477043     Christian Djoos /api/v1/people/8477043
    ## 17           24   8477240       Carter Rowney /api/v1/people/8477240
    ## 18           22   8477947        Sonny Milano /api/v1/people/8477947
    ## 19           43   8478046       Danton Heinen /api/v1/people/8478046
    ## 20           32   8478491       Jacob Larsson /api/v1/people/8478491
    ## 21           34   8479351           Sam Steel /api/v1/people/8479351
    ## 22           49   8479368           Max Jones /api/v1/people/8479368
    ##    position.code position.name position.type position.abbreviation
    ## 1              G        Goalie        Goalie                     G
    ## 2              C        Center       Forward                     C
    ## 3              R    Right Wing       Forward                    RW
    ## 4              D    Defenseman    Defenseman                     D
    ## 5              C        Center       Forward                     C
    ## 6              R    Right Wing       Forward                    RW
    ## 7              L     Left Wing       Forward                    LW
    ## 8              L     Left Wing       Forward                    LW
    ## 9              D    Defenseman    Defenseman                     D
    ## 10             D    Defenseman    Defenseman                     D
    ## 11             D    Defenseman    Defenseman                     D
    ## 12             D    Defenseman    Defenseman                     D
    ## 13             G        Goalie        Goalie                     G
    ## 14             L     Left Wing       Forward                    LW
    ## 15             D    Defenseman    Defenseman                     D
    ## 16             D    Defenseman    Defenseman                     D
    ## 17             C        Center       Forward                     C
    ## 18             L     Left Wing       Forward                    LW
    ## 19             L     Left Wing       Forward                    LW
    ## 20             D    Defenseman    Defenseman                     D
    ## 21             C        Center       Forward                     C
    ## 22             L     Left Wing       Forward                    LW
    ## 
    ## [[1]][[1]][[24]]
    ##    jerseyNumber person.id   person.fullName            person.link
    ## 1            40   8471691     Martin Hanzal /api/v1/people/8471691
    ## 2            45   8471392       Roman Polak /api/v1/people/8471392
    ## 3            10   8470621       Corey Perry /api/v1/people/8470621
    ## 4            16   8470794      Joe Pavelski /api/v1/people/8470794
    ## 5            47   8471228 Alexander Radulov /api/v1/people/8471228
    ## 6            15   8471260      Blake Comeau /api/v1/people/8471260
    ## 7             5   8471284     Andrej Sekera /api/v1/people/8471284
    ## 8            35   8471418    Anton Khudobin /api/v1/people/8471418
    ## 9            11   8471699   Andrew Cogliano /api/v1/people/8471699
    ## 10           30   8471750        Ben Bishop /api/v1/people/8471750
    ## 11           14   8473994        Jamie Benn /api/v1/people/8473994
    ## 12           37   8475413    Justin Dowling /api/v1/people/8475413
    ## 13           28   8475730     Stephen Johns /api/v1/people/8475730
    ## 14           91   8475794      Tyler Seguin /api/v1/people/8475794
    ## 15            3   8475906    John Klingberg /api/v1/people/8475906
    ## 16           42   8476166      Taylor Fedun /api/v1/people/8476166
    ## 17            2   8476467    Jamie Oleksiak /api/v1/people/8476467
    ## 18           12   8476889       Radek Faksa /api/v1/people/8476889
    ## 19           23   8476902       Esa Lindell /api/v1/people/8476902
    ## 20           13   8477406   Mattias Janmark /api/v1/people/8477406
    ## 21           18   8477450   Jason Dickinson /api/v1/people/8477450
    ## 22           24   8478449       Roope Hintz /api/v1/people/8478449
    ## 23           34   8478495    Denis Gurianov /api/v1/people/8478495
    ## 24           29   8479979    Jake Oettinger /api/v1/people/8479979
    ## 25            4   8480036    Miro Heiskanen /api/v1/people/8480036
    ## 26           60   8480848     Ty Dellandrea /api/v1/people/8480848
    ## 27           55   8481581     Thomas Harley /api/v1/people/8481581
    ## 28           25   8481641    Joel Kiviranta /api/v1/people/8481641
    ##    position.code position.name position.type position.abbreviation
    ## 1              C        Center       Forward                     C
    ## 2              D    Defenseman    Defenseman                     D
    ## 3              R    Right Wing       Forward                    RW
    ## 4              C        Center       Forward                     C
    ## 5              R    Right Wing       Forward                    RW
    ## 6              L     Left Wing       Forward                    LW
    ## 7              D    Defenseman    Defenseman                     D
    ## 8              G        Goalie        Goalie                     G
    ## 9              C        Center       Forward                     C
    ## 10             G        Goalie        Goalie                     G
    ## 11             L     Left Wing       Forward                    LW
    ## 12             C        Center       Forward                     C
    ## 13             D    Defenseman    Defenseman                     D
    ## 14             C        Center       Forward                     C
    ## 15             D    Defenseman    Defenseman                     D
    ## 16             D    Defenseman    Defenseman                     D
    ## 17             D    Defenseman    Defenseman                     D
    ## 18             C        Center       Forward                     C
    ## 19             D    Defenseman    Defenseman                     D
    ## 20             C        Center       Forward                     C
    ## 21             C        Center       Forward                     C
    ## 22             L     Left Wing       Forward                    LW
    ## 23             R    Right Wing       Forward                    RW
    ## 24             G        Goalie        Goalie                     G
    ## 25             D    Defenseman    Defenseman                     D
    ## 26             C        Center       Forward                     C
    ## 27             D    Defenseman    Defenseman                     D
    ## 28             L     Left Wing       Forward                    LW
    ## 
    ## [[1]][[1]][[25]]
    ##    jerseyNumber person.id    person.fullName            person.link
    ## 1            77   8470604        Jeff Carter /api/v1/people/8470604
    ## 2            23   8470606       Dustin Brown /api/v1/people/8470606
    ## 3            11   8471685       Anze Kopitar /api/v1/people/8471685
    ## 4            32   8471734     Jonathan Quick /api/v1/people/8471734
    ## 5            22   8473453       Trevor Lewis /api/v1/people/8473453
    ## 6             8   8474563       Drew Doughty /api/v1/people/8474563
    ## 7            29   8476924         Martin Frk /api/v1/people/8476924
    ## 8            74   8476947 Nikolai Prokhorkin /api/v1/people/8476947
    ## 9            15   8477018         Ben Hutton /api/v1/people/8477018
    ## 10            6   8477046        Joakim Ryan /api/v1/people/8477046
    ## 11           56   8477073   Kurtis MacDermid /api/v1/people/8477073
    ## 12           40   8477361    Calvin Petersen /api/v1/people/8477361
    ## 13            9   8477960       Adrian Kempe /api/v1/people/8477960
    ## 14           10   8478020     Michael Amadio /api/v1/people/8478020
    ## 15           51   8478455      Austin Wagner /api/v1/people/8478455
    ## 16            3   8478911           Matt Roy /api/v1/people/8478911
    ## 17           12   8479675       Trevor Moore /api/v1/people/8479675
    ## 18           44   8479998     Mikey Anderson /api/v1/people/8479998
    ## 19           42   8480014    Gabriel Vilardi /api/v1/people/8480014
    ## 20           19   8480113       Alex Iafallo /api/v1/people/8480113
    ## 21           26   8480336        Sean Walker /api/v1/people/8480336
    ## 22           46   8481481      Blake Lizotte /api/v1/people/8481481
    ##    position.code position.name position.type position.abbreviation
    ## 1              C        Center       Forward                     C
    ## 2              R    Right Wing       Forward                    RW
    ## 3              C        Center       Forward                     C
    ## 4              G        Goalie        Goalie                     G
    ## 5              C        Center       Forward                     C
    ## 6              D    Defenseman    Defenseman                     D
    ## 7              R    Right Wing       Forward                    RW
    ## 8              L     Left Wing       Forward                    LW
    ## 9              D    Defenseman    Defenseman                     D
    ## 10             D    Defenseman    Defenseman                     D
    ## 11             D    Defenseman    Defenseman                     D
    ## 12             G        Goalie        Goalie                     G
    ## 13             C        Center       Forward                     C
    ## 14             C        Center       Forward                     C
    ## 15             L     Left Wing       Forward                    LW
    ## 16             D    Defenseman    Defenseman                     D
    ## 17             C        Center       Forward                     C
    ## 18             D    Defenseman    Defenseman                     D
    ## 19             C        Center       Forward                     C
    ## 20             L     Left Wing       Forward                    LW
    ## 21             D    Defenseman    Defenseman                     D
    ## 22             C        Center       Forward                     C
    ## 
    ## [[1]][[1]][[26]]
    ##    jerseyNumber person.id     person.fullName            person.link
    ## 1            65   8474578       Erik Karlsson /api/v1/people/8474578
    ## 2             5   8474774        Dalton Prout /api/v1/people/8474774
    ## 3            48   8476881         Tomas Hertl /api/v1/people/8476881
    ## 4            19   8466138        Joe Thornton /api/v1/people/8466138
    ## 5            88   8470613         Brent Burns /api/v1/people/8470613
    ## 6            44   8471709 Marc-Edouard Vlasic /api/v1/people/8471709
    ## 7            39   8474053       Logan Couture /api/v1/people/8474053
    ## 8            31   8474889        Martin Jones /api/v1/people/8474889
    ## 9             9   8475169        Evander Kane /api/v1/people/8475169
    ## 10           20   8475834     Marcus Sorensen /api/v1/people/8475834
    ## 11           72   8475841            Tim Heed /api/v1/people/8475841
    ## 12           21   8475869    Brandon Davidson /api/v1/people/8475869
    ## 13           11   8476474       Stefan Noesen /api/v1/people/8476474
    ## 14           30   8477180          Aaron Dell /api/v1/people/8477180
    ## 15           68   8477922     Melker Karlsson /api/v1/people/8477922
    ## 16           62   8478099        Kevin Labanc /api/v1/people/8478099
    ## 17           67   8478136     Jacob Middleton /api/v1/people/8478136
    ## 18           28   8478414          Timo Meier /api/v1/people/8478414
    ## 19           73   8479393         Noah Gregor /api/v1/people/8479393
    ## 20            7   8479580      Dylan Gambrell /api/v1/people/8479580
    ## 21           38   8479983       Mario Ferraro /api/v1/people/8479983
    ## 22           51   8480160         Radim Simek /api/v1/people/8480160
    ## 23           70   8480384      Alexander True /api/v1/people/8480384
    ## 24           40   8480965       Antti Suomela /api/v1/people/8480965
    ## 25           45   8481640       Lean Bergmann /api/v1/people/8481640
    ##    position.code position.name position.type position.abbreviation
    ## 1              D    Defenseman    Defenseman                     D
    ## 2              D    Defenseman    Defenseman                     D
    ## 3              C        Center       Forward                     C
    ## 4              C        Center       Forward                     C
    ## 5              D    Defenseman    Defenseman                     D
    ## 6              D    Defenseman    Defenseman                     D
    ## 7              C        Center       Forward                     C
    ## 8              G        Goalie        Goalie                     G
    ## 9              L     Left Wing       Forward                    LW
    ## 10             L     Left Wing       Forward                    LW
    ## 11             D    Defenseman    Defenseman                     D
    ## 12             D    Defenseman    Defenseman                     D
    ## 13             R    Right Wing       Forward                    RW
    ## 14             G        Goalie        Goalie                     G
    ## 15             C        Center       Forward                     C
    ## 16             R    Right Wing       Forward                    RW
    ## 17             D    Defenseman    Defenseman                     D
    ## 18             R    Right Wing       Forward                    RW
    ## 19             C        Center       Forward                     C
    ## 20             C        Center       Forward                     C
    ## 21             D    Defenseman    Defenseman                     D
    ## 22             D    Defenseman    Defenseman                     D
    ## 23             C        Center       Forward                     C
    ## 24             C        Center       Forward                     C
    ## 25             C        Center       Forward                     C
    ## 
    ## [[1]][[1]][[27]]
    ##    jerseyNumber person.id    person.fullName            person.link
    ## 1            17   8471273   Brandon Dubinsky /api/v1/people/8471273
    ## 2            77   8476981      Josh Anderson /api/v1/people/8476981
    ## 3            42   8480074   Alexandre Texier /api/v1/people/8480074
    ## 4            15   8481650        Jakob Lilja /api/v1/people/8481650
    ## 5            24   8471804       Nathan Gerbe /api/v1/people/8471804
    ## 6            71   8473422       Nick Foligno /api/v1/people/8473422
    ## 7            20   8474062         Riley Nash /api/v1/people/8474062
    ## 8            14   8474679     Gustav Nyquist /api/v1/people/8474679
    ## 9            13   8474715       Cam Atkinson /api/v1/people/8474715
    ## 10           58   8475233       David Savard /api/v1/people/8475233
    ## 11           38   8476432       Boone Jenner /api/v1/people/8476432
    ## 12            4   8476449   Scott Harrington /api/v1/people/8476449
    ## 13           27   8476850        Ryan Murray /api/v1/people/8476850
    ## 14           23   8476870     Stefan Matteau /api/v1/people/8476870
    ## 15           74   8476913        Devin Shore /api/v1/people/8476913
    ## 16           70   8476914   Joonas Korpisalo /api/v1/people/8476914
    ## 17           28   8477416 Oliver Bjorkstrand /api/v1/people/8477416
    ## 18            3   8477495         Seth Jones /api/v1/people/8477495
    ## 19           10   8477505 Alexander Wennberg /api/v1/people/8477505
    ## 20           90   8478007   Elvis Merzlikins /api/v1/people/8478007
    ## 21            8   8478460      Zach Werenski /api/v1/people/8478460
    ## 22           53   8478506   Gabriel Carlsson /api/v1/people/8478506
    ## 23           46   8478567         Dean Kukan /api/v1/people/8478567
    ## 24           11   8478831     Kevin Stenlund /api/v1/people/8478831
    ## 25           44   8478882 Vladislav Gavrikov /api/v1/people/8478882
    ## 26           65   8478906   Markus Nutivaara /api/v1/people/8478906
    ## 27            2   8479369       Andrew Peeke /api/v1/people/8479369
    ## 28           18   8479400  Pierre-Luc Dubois /api/v1/people/8479400
    ## 29           80   8480162  Matiss Kivlenieks /api/v1/people/8480162
    ## 30           50   8480762      Eric Robinson /api/v1/people/8480762
    ## 31           19   8480853         Liam Foudy /api/v1/people/8480853
    ##    position.code position.name position.type position.abbreviation
    ## 1              C        Center       Forward                     C
    ## 2              R    Right Wing       Forward                    RW
    ## 3              C        Center       Forward                     C
    ## 4              L     Left Wing       Forward                    LW
    ## 5              C        Center       Forward                     C
    ## 6              L     Left Wing       Forward                    LW
    ## 7              C        Center       Forward                     C
    ## 8              C        Center       Forward                     C
    ## 9              R    Right Wing       Forward                    RW
    ## 10             D    Defenseman    Defenseman                     D
    ## 11             C        Center       Forward                     C
    ## 12             D    Defenseman    Defenseman                     D
    ## 13             D    Defenseman    Defenseman                     D
    ## 14             C        Center       Forward                     C
    ## 15             C        Center       Forward                     C
    ## 16             G        Goalie        Goalie                     G
    ## 17             R    Right Wing       Forward                    RW
    ## 18             D    Defenseman    Defenseman                     D
    ## 19             C        Center       Forward                     C
    ## 20             G        Goalie        Goalie                     G
    ## 21             D    Defenseman    Defenseman                     D
    ## 22             D    Defenseman    Defenseman                     D
    ## 23             D    Defenseman    Defenseman                     D
    ## 24             C        Center       Forward                     C
    ## 25             D    Defenseman    Defenseman                     D
    ## 26             D    Defenseman    Defenseman                     D
    ## 27             D    Defenseman    Defenseman                     D
    ## 28             C        Center       Forward                     C
    ## 29             G        Goalie        Goalie                     G
    ## 30             L     Left Wing       Forward                    LW
    ## 31             C        Center       Forward                     C
    ## 
    ## [[1]][[1]][[28]]
    ##    jerseyNumber person.id  person.fullName            person.link position.code
    ## 1            27   8475760    Nick Bjugstad /api/v1/people/8475760             C
    ## 2             9   8469459      Mikko Koivu /api/v1/people/8469459             C
    ## 3            20   8470600       Ryan Suter /api/v1/people/8470600             D
    ## 4            11   8470610      Zach Parise /api/v1/people/8470610             L
    ## 5            40   8471227     Devan Dubnyk /api/v1/people/8471227             G
    ## 6            32   8471774     Alex Stalock /api/v1/people/8471774             G
    ## 7            46   8474716   Jared Spurgeon /api/v1/people/8474716             D
    ## 8            44   8474749  Matt Bartkowski /api/v1/people/8474749             D
    ## 9            90   8475149 Marcus Johansson /api/v1/people/8475149             C
    ## 10           17   8475220   Marcus Foligno /api/v1/people/8475220             L
    ## 11           36   8475692  Mats Zuccarello /api/v1/people/8475692             R
    ## 12           37   8476415         Kyle Rau /api/v1/people/8476415             C
    ## 13           49   8476437      Victor Rask /api/v1/people/8476437             C
    ## 14           25   8476463     Jonas Brodin /api/v1/people/8476463             D
    ## 15           77   8476779        Brad Hunt /api/v1/people/8476779             D
    ## 16           27   8476851  Alex Galchenyuk /api/v1/people/8476851             C
    ## 17           24   8476856       Matt Dumba /api/v1/people/8476856             D
    ## 18           41   8477366     Luke Johnson /api/v1/people/8477366             C
    ## 19           21   8477369     Carson Soucy /api/v1/people/8477369             D
    ## 20           38   8477451     Ryan Hartman /api/v1/people/8477451             R
    ## 21           22   8477942      Kevin Fiala /api/v1/people/8477942             L
    ## 22            6   8477987      Ryan Donato /api/v1/people/8477987             C
    ## 23           47   8478011   Louie Belpedio /api/v1/people/8478011             D
    ## 24           34   8478039   Kaapo Kahkonen /api/v1/people/8478039             G
    ## 25           18   8478413  Jordan Greenway /api/v1/people/8478413             L
    ## 26           14   8478493 Joel Eriksson Ek /api/v1/people/8478493             C
    ## 27           19   8479316       Luke Kunin /api/v1/people/8479316             C
    ## 28           61   8479734   Brennan Menell /api/v1/people/8479734             D
    ## 29           26   8479933    Gerald Mayhew /api/v1/people/8479933             C
    ## 30           42   8481443       Mat Robson /api/v1/people/8481443             G
    ## 31            7   8481477       Nico Sturm /api/v1/people/8481477             C
    ##    position.name position.type position.abbreviation
    ## 1         Center       Forward                     C
    ## 2         Center       Forward                     C
    ## 3     Defenseman    Defenseman                     D
    ## 4      Left Wing       Forward                    LW
    ## 5         Goalie        Goalie                     G
    ## 6         Goalie        Goalie                     G
    ## 7     Defenseman    Defenseman                     D
    ## 8     Defenseman    Defenseman                     D
    ## 9         Center       Forward                     C
    ## 10     Left Wing       Forward                    LW
    ## 11    Right Wing       Forward                    RW
    ## 12        Center       Forward                     C
    ## 13        Center       Forward                     C
    ## 14    Defenseman    Defenseman                     D
    ## 15    Defenseman    Defenseman                     D
    ## 16        Center       Forward                     C
    ## 17    Defenseman    Defenseman                     D
    ## 18        Center       Forward                     C
    ## 19    Defenseman    Defenseman                     D
    ## 20    Right Wing       Forward                    RW
    ## 21     Left Wing       Forward                    LW
    ## 22        Center       Forward                     C
    ## 23    Defenseman    Defenseman                     D
    ## 24        Goalie        Goalie                     G
    ## 25     Left Wing       Forward                    LW
    ## 26        Center       Forward                     C
    ## 27        Center       Forward                     C
    ## 28    Defenseman    Defenseman                     D
    ## 29        Center       Forward                     C
    ## 30        Goalie        Goalie                     G
    ## 31        Center       Forward                     C
    ## 
    ## [[1]][[1]][[29]]
    ##    jerseyNumber person.id   person.fullName            person.link
    ## 1            18   8473412      Bryan Little /api/v1/people/8473412
    ## 2            26   8471218     Blake Wheeler /api/v1/people/8471218
    ## 3            85   8473618 Mathieu Perreault /api/v1/people/8473618
    ## 4            22   8473914      Mark Letestu /api/v1/people/8473914
    ## 5             5   8474579        Luca Sbisa /api/v1/people/8474579
    ## 6             7   8475179    Dmitry Kulikov /api/v1/people/8475179
    ## 7            20   8475236        Cody Eakin /api/v1/people/8475236
    ## 8            57   8475268   Gabriel Bourque /api/v1/people/8475268
    ## 9             2   8475868   Anthony Bitetto /api/v1/people/8475868
    ## 10           30   8476316  Laurent Brossoit /api/v1/people/8476316
    ## 11           12   8476331      Dylan DeMelo /api/v1/people/8476331
    ## 12           17   8476392        Adam Lowry /api/v1/people/8476392
    ## 13           38   8476400        Logan Shaw /api/v1/people/8476400
    ## 14           21   8476406    Nicholas Shore /api/v1/people/8476406
    ## 15           55   8476460    Mark Scheifele /api/v1/people/8476460
    ## 16           88   8476470   Nathan Beaulieu /api/v1/people/8476470
    ## 17           37   8476945 Connor Hellebuyck /api/v1/people/8476945
    ## 18            3   8477359    Tucker Poolman /api/v1/people/8477359
    ## 19            9   8477429       Andrew Copp /api/v1/people/8477429
    ## 20           23   8477472    Carl Dahlstrom /api/v1/people/8477472
    ## 21            1   8477480       Eric Comrie /api/v1/people/8477480
    ## 22           44   8477504    Josh Morrissey /api/v1/people/8477504
    ## 23           27   8477940    Nikolaj Ehlers /api/v1/people/8477940
    ## 24           81   8478398       Kyle Connor /api/v1/people/8478398
    ## 25           58   8478424    Jansen Harkins /api/v1/people/8478424
    ## 26           28   8478458     Jack Roslovic /api/v1/people/8478458
    ## 27           82   8478891    Mason Appleton /api/v1/people/8478891
    ## 28            8   8478915         Sami Niku /api/v1/people/8478915
    ## 29           29   8479339      Patrik Laine /api/v1/people/8479339
    ## 30           60   8479574    Mikhail Berdin /api/v1/people/8479574
    ## 31            4   8480145        Neal Pionk /api/v1/people/8480145
    ##    position.code position.name position.type position.abbreviation
    ## 1              C        Center       Forward                     C
    ## 2              R    Right Wing       Forward                    RW
    ## 3              L     Left Wing       Forward                    LW
    ## 4              C        Center       Forward                     C
    ## 5              D    Defenseman    Defenseman                     D
    ## 6              D    Defenseman    Defenseman                     D
    ## 7              C        Center       Forward                     C
    ## 8              L     Left Wing       Forward                    LW
    ## 9              D    Defenseman    Defenseman                     D
    ## 10             G        Goalie        Goalie                     G
    ## 11             D    Defenseman    Defenseman                     D
    ## 12             C        Center       Forward                     C
    ## 13             R    Right Wing       Forward                    RW
    ## 14             C        Center       Forward                     C
    ## 15             C        Center       Forward                     C
    ## 16             D    Defenseman    Defenseman                     D
    ## 17             G        Goalie        Goalie                     G
    ## 18             D    Defenseman    Defenseman                     D
    ## 19             C        Center       Forward                     C
    ## 20             D    Defenseman    Defenseman                     D
    ## 21             G        Goalie        Goalie                     G
    ## 22             D    Defenseman    Defenseman                     D
    ## 23             L     Left Wing       Forward                    LW
    ## 24             L     Left Wing       Forward                    LW
    ## 25             C        Center       Forward                     C
    ## 26             C        Center       Forward                     C
    ## 27             C        Center       Forward                     C
    ## 28             D    Defenseman    Defenseman                     D
    ## 29             R    Right Wing       Forward                    RW
    ## 30             G        Goalie        Goalie                     G
    ## 31             D    Defenseman    Defenseman                     D
    ## 
    ## [[1]][[1]][[30]]
    ##    jerseyNumber person.id      person.fullName            person.link
    ## 1            15   8470755      Brad Richardson /api/v1/people/8470755
    ## 2            34   8471262       Carl Soderberg /api/v1/people/8471262
    ## 3            33   8471274       Alex Goligoski /api/v1/people/8471274
    ## 4             4   8471769   Niklas Hjalmarsson /api/v1/people/8471769
    ## 5            40   8473546      Michael Grabner /api/v1/people/8473546
    ## 6            81   8473548          Phil Kessel /api/v1/people/8473548
    ## 7            55   8474218         Jason Demers /api/v1/people/8474218
    ## 8            21   8474613         Derek Stepan /api/v1/people/8474613
    ## 9            23   8475171 Oliver Ekman-Larsson /api/v1/people/8475171
    ## 10           35   8475311        Darcy Kuemper /api/v1/people/8475311
    ## 11           91   8475791          Taylor Hall /api/v1/people/8475791
    ## 12           13   8476994    Vinnie Hinostroza /api/v1/people/8476994
    ## 13           32   8477293         Antti Raanta /api/v1/people/8477293
    ## 14           82   8477851      Jordan Oesterle /api/v1/people/8477851
    ## 15            8   8477951        Nick Schmaltz /api/v1/people/8477951
    ## 16           18   8477989     Christian Dvorak /api/v1/people/8477989
    ## 17           36   8478432    Christian Fischer /api/v1/people/8478432
    ## 18           67   8478474        Lawson Crouse /api/v1/people/8478474
    ## 19           83   8478856        Conor Garland /api/v1/people/8478856
    ## 20            9   8479343       Clayton Keller /api/v1/people/8479343
    ## 21            6   8479345       Jakob Chychrun /api/v1/people/8479345
    ## 22           29   8480849       Barrett Hayton /api/v1/people/8480849
    ## 23           46   8480950      Ilya Lyubushkin /api/v1/people/8480950
    ##    position.code position.name position.type position.abbreviation
    ## 1              C        Center       Forward                     C
    ## 2              C        Center       Forward                     C
    ## 3              D    Defenseman    Defenseman                     D
    ## 4              D    Defenseman    Defenseman                     D
    ## 5              L     Left Wing       Forward                    LW
    ## 6              R    Right Wing       Forward                    RW
    ## 7              D    Defenseman    Defenseman                     D
    ## 8              C        Center       Forward                     C
    ## 9              D    Defenseman    Defenseman                     D
    ## 10             G        Goalie        Goalie                     G
    ## 11             L     Left Wing       Forward                    LW
    ## 12             R    Right Wing       Forward                    RW
    ## 13             G        Goalie        Goalie                     G
    ## 14             D    Defenseman    Defenseman                     D
    ## 15             C        Center       Forward                     C
    ## 16             C        Center       Forward                     C
    ## 17             R    Right Wing       Forward                    RW
    ## 18             L     Left Wing       Forward                    LW
    ## 19             R    Right Wing       Forward                    RW
    ## 20             R    Right Wing       Forward                    RW
    ## 21             D    Defenseman    Defenseman                     D
    ## 22             C        Center       Forward                     C
    ## 23             D    Defenseman    Defenseman                     D
    ## 
    ## [[1]][[1]][[31]]
    ##    jerseyNumber person.id       person.fullName            person.link
    ## 1            73   8475204         Brandon Pirri /api/v1/people/8475204
    ## 2             5   8468674       Deryk Engelland /api/v1/people/8468674
    ## 3            29   8470594     Marc-Andre Fleury /api/v1/people/8470594
    ## 4            26   8471669          Paul Stastny /api/v1/people/8471669
    ## 5            75   8471817           Ryan Reaves /api/v1/people/8471817
    ## 6            67   8474157        Max Pacioretty /api/v1/people/8474157
    ## 7            23   8474166         Alec Martinez /api/v1/people/8474166
    ## 8            22   8474207           Nick Holden /api/v1/people/8474207
    ## 9             3   8475188        Brayden McNabb /api/v1/people/8475188
    ## 10           19   8475191          Reilly Smith /api/v1/people/8475191
    ## 11           90   8475215          Robin Lehner /api/v1/people/8475215
    ## 12           15   8475750           Jon Merrill /api/v1/people/8475750
    ## 13           61   8475913            Mark Stone /api/v1/people/8475913
    ## 14           21   8476393          Nick Cousins /api/v1/people/8476393
    ## 15           71   8476448      William Karlsson /api/v1/people/8476448
    ## 16           81   8476539 Jonathan Marchessault /api/v1/people/8476539
    ## 17           35   8476861           Oscar Dansk /api/v1/people/8476861
    ## 18           20   8476905   Chandler Stephenson /api/v1/people/8476905
    ## 19           88   8477220          Nate Schmidt /api/v1/people/8477220
    ## 20           27   8477447         Shea Theodore /api/v1/people/8477447
    ## 21           28   8477478       William Carrier /api/v1/people/8477478
    ## 22           92   8477931           Tomas Nosek /api/v1/people/8477931
    ## 23           89   8477949             Alex Tuch /api/v1/people/8477949
    ## 24           37   8478097             Reid Duke /api/v1/people/8478097
    ## 25           10   8478462           Nicolas Roy /api/v1/people/8478462
    ## 26           52   8479639         Dylan Coghlan /api/v1/people/8479639
    ## 27            2   8480727       Zach Whitecloud /api/v1/people/8480727
    ## 28           18   8481522          Peyton Krebs /api/v1/people/8481522
    ##    position.code position.name position.type position.abbreviation
    ## 1              C        Center       Forward                     C
    ## 2              D    Defenseman    Defenseman                     D
    ## 3              G        Goalie        Goalie                     G
    ## 4              C        Center       Forward                     C
    ## 5              R    Right Wing       Forward                    RW
    ## 6              L     Left Wing       Forward                    LW
    ## 7              D    Defenseman    Defenseman                     D
    ## 8              D    Defenseman    Defenseman                     D
    ## 9              D    Defenseman    Defenseman                     D
    ## 10             R    Right Wing       Forward                    RW
    ## 11             G        Goalie        Goalie                     G
    ## 12             D    Defenseman    Defenseman                     D
    ## 13             R    Right Wing       Forward                    RW
    ## 14             C        Center       Forward                     C
    ## 15             C        Center       Forward                     C
    ## 16             C        Center       Forward                     C
    ## 17             G        Goalie        Goalie                     G
    ## 18             C        Center       Forward                     C
    ## 19             D    Defenseman    Defenseman                     D
    ## 20             D    Defenseman    Defenseman                     D
    ## 21             L     Left Wing       Forward                    LW
    ## 22             L     Left Wing       Forward                    LW
    ## 23             R    Right Wing       Forward                    RW
    ## 24             C        Center       Forward                     C
    ## 25             C        Center       Forward                     C
    ## 26             D    Defenseman    Defenseman                     D
    ## 27             D    Defenseman    Defenseman                     D
    ## 28             C        Center       Forward                     C
    ## 
    ## 
    ## [[1]]$.name_repair
    ## [1] "unique"

## Wrapper

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

## Analytics

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
