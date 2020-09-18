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


hockey_stats("?stats=statsSingleSeasonPlayoffs")
```

    ## [[1]]
    ## [[1]][[1]]
    ##    id                  name             link abbreviation       teamName
    ## 1   1     New Jersey Devils  /api/v1/teams/1          NJD         Devils
    ## 2   2    New York Islanders  /api/v1/teams/2          NYI      Islanders
    ## 3   3      New York Rangers  /api/v1/teams/3          NYR        Rangers
    ## 4   4   Philadelphia Flyers  /api/v1/teams/4          PHI         Flyers
    ## 5   5   Pittsburgh Penguins  /api/v1/teams/5          PIT       Penguins
    ## 6   6         Boston Bruins  /api/v1/teams/6          BOS         Bruins
    ## 7   7        Buffalo Sabres  /api/v1/teams/7          BUF         Sabres
    ## 8   8    Montréal Canadiens  /api/v1/teams/8          MTL      Canadiens
    ## 9   9       Ottawa Senators  /api/v1/teams/9          OTT       Senators
    ## 10 10   Toronto Maple Leafs /api/v1/teams/10          TOR    Maple Leafs
    ## 11 12   Carolina Hurricanes /api/v1/teams/12          CAR     Hurricanes
    ## 12 13      Florida Panthers /api/v1/teams/13          FLA       Panthers
    ## 13 14   Tampa Bay Lightning /api/v1/teams/14          TBL      Lightning
    ## 14 15   Washington Capitals /api/v1/teams/15          WSH       Capitals
    ## 15 16    Chicago Blackhawks /api/v1/teams/16          CHI     Blackhawks
    ## 16 17     Detroit Red Wings /api/v1/teams/17          DET      Red Wings
    ## 17 18   Nashville Predators /api/v1/teams/18          NSH      Predators
    ## 18 19       St. Louis Blues /api/v1/teams/19          STL          Blues
    ## 19 20        Calgary Flames /api/v1/teams/20          CGY         Flames
    ## 20 21    Colorado Avalanche /api/v1/teams/21          COL      Avalanche
    ## 21 22       Edmonton Oilers /api/v1/teams/22          EDM         Oilers
    ## 22 23     Vancouver Canucks /api/v1/teams/23          VAN        Canucks
    ## 23 24         Anaheim Ducks /api/v1/teams/24          ANA          Ducks
    ## 24 25          Dallas Stars /api/v1/teams/25          DAL          Stars
    ## 25 26     Los Angeles Kings /api/v1/teams/26          LAK          Kings
    ## 26 28       San Jose Sharks /api/v1/teams/28          SJS         Sharks
    ## 27 29 Columbus Blue Jackets /api/v1/teams/29          CBJ   Blue Jackets
    ## 28 30        Minnesota Wild /api/v1/teams/30          MIN           Wild
    ## 29 52         Winnipeg Jets /api/v1/teams/52          WPG           Jets
    ## 30 53       Arizona Coyotes /api/v1/teams/53          ARI        Coyotes
    ## 31 54  Vegas Golden Knights /api/v1/teams/54          VGK Golden Knights
    ##    locationName firstYearOfPlay    shortName                    officialSiteUrl
    ## 1    New Jersey            1982   New Jersey    http://www.newjerseydevils.com/
    ## 2      New York            1972 NY Islanders   http://www.newyorkislanders.com/
    ## 3      New York            1926   NY Rangers     http://www.newyorkrangers.com/
    ## 4  Philadelphia            1967 Philadelphia http://www.philadelphiaflyers.com/
    ## 5    Pittsburgh            1967   Pittsburgh     http://pittsburghpenguins.com/
    ## 6        Boston            1924       Boston       http://www.bostonbruins.com/
    ## 7       Buffalo            1970      Buffalo             http://www.sabres.com/
    ## 8      Montréal            1909     Montréal          http://www.canadiens.com/
    ## 9        Ottawa            1990       Ottawa     http://www.ottawasenators.com/
    ## 10      Toronto            1917      Toronto         http://www.mapleleafs.com/
    ## 11     Carolina            1979     Carolina http://www.carolinahurricanes.com/
    ## 12      Florida            1993      Florida    http://www.floridapanthers.com/
    ## 13    Tampa Bay            1991    Tampa Bay  http://www.tampabaylightning.com/
    ## 14   Washington            1974   Washington http://www.washingtoncapitals.com/
    ## 15      Chicago            1926      Chicago  http://www.chicagoblackhawks.com/
    ## 16      Detroit            1926      Detroit    http://www.detroitredwings.com/
    ## 17    Nashville            1997    Nashville http://www.nashvillepredators.com/
    ## 18    St. Louis            1967     St Louis       http://www.stlouisblues.com/
    ## 19      Calgary            1980      Calgary      http://www.calgaryflames.com/
    ## 20     Colorado            1979     Colorado  http://www.coloradoavalanche.com/
    ## 21     Edmonton            1979     Edmonton     http://www.edmontonoilers.com/
    ## 22    Vancouver            1970    Vancouver            http://www.canucks.com/
    ## 23      Anaheim            1993      Anaheim       http://www.anaheimducks.com/
    ## 24       Dallas            1967       Dallas        http://www.dallasstars.com/
    ## 25  Los Angeles            1967  Los Angeles            http://www.lakings.com/
    ## 26     San Jose            1990     San Jose           http://www.sjsharks.com/
    ## 27     Columbus            1997     Columbus        http://www.bluejackets.com/
    ## 28    Minnesota            1997    Minnesota               http://www.wild.com/
    ## 29     Winnipeg            2011     Winnipeg           http://winnipegjets.com/
    ## 30      Arizona            1979      Arizona     http://www.arizonacoyotes.com/
    ## 31        Vegas            2016        Vegas http://www.vegasgoldenknights.com/
    ##    franchiseId active               venue.name          venue.link   venue.city
    ## 1           23   TRUE        Prudential Center /api/v1/venues/null       Newark
    ## 2           22   TRUE          Barclays Center /api/v1/venues/5026     Brooklyn
    ## 3           10   TRUE    Madison Square Garden /api/v1/venues/5054     New York
    ## 4           16   TRUE       Wells Fargo Center /api/v1/venues/5096 Philadelphia
    ## 5           17   TRUE         PPG Paints Arena /api/v1/venues/5034   Pittsburgh
    ## 6            6   TRUE                TD Garden /api/v1/venues/5085       Boston
    ## 7           19   TRUE           KeyBank Center /api/v1/venues/5039      Buffalo
    ## 8            1   TRUE              Bell Centre /api/v1/venues/5028     Montréal
    ## 9           30   TRUE     Canadian Tire Centre /api/v1/venues/5031       Ottawa
    ## 10           5   TRUE         Scotiabank Arena /api/v1/venues/null      Toronto
    ## 11          26   TRUE                PNC Arena /api/v1/venues/5066      Raleigh
    ## 12          33   TRUE              BB&T Center /api/v1/venues/5027      Sunrise
    ## 13          31   TRUE             AMALIE Arena /api/v1/venues/null        Tampa
    ## 14          24   TRUE        Capital One Arena /api/v1/venues/5094   Washington
    ## 15          11   TRUE            United Center /api/v1/venues/5092      Chicago
    ## 16          12   TRUE     Little Caesars Arena /api/v1/venues/5145      Detroit
    ## 17          34   TRUE        Bridgestone Arena /api/v1/venues/5030    Nashville
    ## 18          18   TRUE        Enterprise Center /api/v1/venues/5076    St. Louis
    ## 19          21   TRUE    Scotiabank Saddledome /api/v1/venues/5075      Calgary
    ## 20          27   TRUE             Pepsi Center /api/v1/venues/5064       Denver
    ## 21          25   TRUE             Rogers Place /api/v1/venues/5100     Edmonton
    ## 22          20   TRUE             Rogers Arena /api/v1/venues/5073    Vancouver
    ## 23          32   TRUE             Honda Center /api/v1/venues/5046      Anaheim
    ## 24          15   TRUE American Airlines Center /api/v1/venues/5019       Dallas
    ## 25          14   TRUE           STAPLES Center /api/v1/venues/5081  Los Angeles
    ## 26          29   TRUE   SAP Center at San Jose /api/v1/venues/null     San Jose
    ## 27          36   TRUE         Nationwide Arena /api/v1/venues/5059     Columbus
    ## 28          37   TRUE       Xcel Energy Center /api/v1/venues/5098     St. Paul
    ## 29          35   TRUE           Bell MTS Place /api/v1/venues/5058     Winnipeg
    ## 30          28   TRUE         Gila River Arena /api/v1/venues/5043     Glendale
    ## 31          38   TRUE           T-Mobile Arena /api/v1/venues/5178    Las Vegas
    ##    venue.id   venue.timeZone.id venue.timeZone.offset venue.timeZone.tz
    ## 1        NA    America/New_York                    -4               EDT
    ## 2      5026    America/New_York                    -4               EDT
    ## 3      5054    America/New_York                    -4               EDT
    ## 4      5096    America/New_York                    -4               EDT
    ## 5      5034    America/New_York                    -4               EDT
    ## 6      5085    America/New_York                    -4               EDT
    ## 7      5039    America/New_York                    -4               EDT
    ## 8      5028    America/Montreal                    -4               EDT
    ## 9      5031    America/New_York                    -4               EDT
    ## 10       NA     America/Toronto                    -4               EDT
    ## 11     5066    America/New_York                    -4               EDT
    ## 12     5027    America/New_York                    -4               EDT
    ## 13       NA    America/New_York                    -4               EDT
    ## 14     5094    America/New_York                    -4               EDT
    ## 15     5092     America/Chicago                    -5               CDT
    ## 16     5145     America/Detroit                    -4               EDT
    ## 17     5030     America/Chicago                    -5               CDT
    ## 18     5076     America/Chicago                    -5               CDT
    ## 19     5075      America/Denver                    -6               MDT
    ## 20     5064      America/Denver                    -6               MDT
    ## 21     5100    America/Edmonton                    -6               MDT
    ## 22     5073   America/Vancouver                    -7               PDT
    ## 23     5046 America/Los_Angeles                    -7               PDT
    ## 24     5019     America/Chicago                    -5               CDT
    ## 25     5081 America/Los_Angeles                    -7               PDT
    ## 26       NA America/Los_Angeles                    -7               PDT
    ## 27     5059    America/New_York                    -4               EDT
    ## 28     5098     America/Chicago                    -5               CDT
    ## 29     5058    America/Winnipeg                    -5               CDT
    ## 30     5043     America/Phoenix                    -7               MST
    ## 31     5178 America/Los_Angeles                    -7               PDT
    ##    division.id division.name division.nameShort        division.link
    ## 1           18  Metropolitan              Metro /api/v1/divisions/18
    ## 2           18  Metropolitan              Metro /api/v1/divisions/18
    ## 3           18  Metropolitan              Metro /api/v1/divisions/18
    ## 4           18  Metropolitan              Metro /api/v1/divisions/18
    ## 5           18  Metropolitan              Metro /api/v1/divisions/18
    ## 6           17      Atlantic                ATL /api/v1/divisions/17
    ## 7           17      Atlantic                ATL /api/v1/divisions/17
    ## 8           17      Atlantic                ATL /api/v1/divisions/17
    ## 9           17      Atlantic                ATL /api/v1/divisions/17
    ## 10          17      Atlantic                ATL /api/v1/divisions/17
    ## 11          18  Metropolitan              Metro /api/v1/divisions/18
    ## 12          17      Atlantic                ATL /api/v1/divisions/17
    ## 13          17      Atlantic                ATL /api/v1/divisions/17
    ## 14          18  Metropolitan              Metro /api/v1/divisions/18
    ## 15          16       Central                CEN /api/v1/divisions/16
    ## 16          17      Atlantic                ATL /api/v1/divisions/17
    ## 17          16       Central                CEN /api/v1/divisions/16
    ## 18          16       Central                CEN /api/v1/divisions/16
    ## 19          15       Pacific                PAC /api/v1/divisions/15
    ## 20          16       Central                CEN /api/v1/divisions/16
    ## 21          15       Pacific                PAC /api/v1/divisions/15
    ## 22          15       Pacific                PAC /api/v1/divisions/15
    ## 23          15       Pacific                PAC /api/v1/divisions/15
    ## 24          16       Central                CEN /api/v1/divisions/16
    ## 25          15       Pacific                PAC /api/v1/divisions/15
    ## 26          15       Pacific                PAC /api/v1/divisions/15
    ## 27          18  Metropolitan              Metro /api/v1/divisions/18
    ## 28          16       Central                CEN /api/v1/divisions/16
    ## 29          16       Central                CEN /api/v1/divisions/16
    ## 30          15       Pacific                PAC /api/v1/divisions/15
    ## 31          15       Pacific                PAC /api/v1/divisions/15
    ##    division.abbreviation conference.id conference.name       conference.link
    ## 1                      M             6         Eastern /api/v1/conferences/6
    ## 2                      M             6         Eastern /api/v1/conferences/6
    ## 3                      M             6         Eastern /api/v1/conferences/6
    ## 4                      M             6         Eastern /api/v1/conferences/6
    ## 5                      M             6         Eastern /api/v1/conferences/6
    ## 6                      A             6         Eastern /api/v1/conferences/6
    ## 7                      A             6         Eastern /api/v1/conferences/6
    ## 8                      A             6         Eastern /api/v1/conferences/6
    ## 9                      A             6         Eastern /api/v1/conferences/6
    ## 10                     A             6         Eastern /api/v1/conferences/6
    ## 11                     M             6         Eastern /api/v1/conferences/6
    ## 12                     A             6         Eastern /api/v1/conferences/6
    ## 13                     A             6         Eastern /api/v1/conferences/6
    ## 14                     M             6         Eastern /api/v1/conferences/6
    ## 15                     C             5         Western /api/v1/conferences/5
    ## 16                     A             6         Eastern /api/v1/conferences/6
    ## 17                     C             5         Western /api/v1/conferences/5
    ## 18                     C             5         Western /api/v1/conferences/5
    ## 19                     P             5         Western /api/v1/conferences/5
    ## 20                     C             5         Western /api/v1/conferences/5
    ## 21                     P             5         Western /api/v1/conferences/5
    ## 22                     P             5         Western /api/v1/conferences/5
    ## 23                     P             5         Western /api/v1/conferences/5
    ## 24                     C             5         Western /api/v1/conferences/5
    ## 25                     P             5         Western /api/v1/conferences/5
    ## 26                     P             5         Western /api/v1/conferences/5
    ## 27                     M             6         Eastern /api/v1/conferences/6
    ## 28                     C             5         Western /api/v1/conferences/5
    ## 29                     C             5         Western /api/v1/conferences/5
    ## 30                     P             5         Western /api/v1/conferences/5
    ## 31                     P             5         Western /api/v1/conferences/5
    ##    franchise.franchiseId franchise.teamName        franchise.link
    ## 1                     23             Devils /api/v1/franchises/23
    ## 2                     22          Islanders /api/v1/franchises/22
    ## 3                     10            Rangers /api/v1/franchises/10
    ## 4                     16             Flyers /api/v1/franchises/16
    ## 5                     17           Penguins /api/v1/franchises/17
    ## 6                      6             Bruins  /api/v1/franchises/6
    ## 7                     19             Sabres /api/v1/franchises/19
    ## 8                      1          Canadiens  /api/v1/franchises/1
    ## 9                     30           Senators /api/v1/franchises/30
    ## 10                     5        Maple Leafs  /api/v1/franchises/5
    ## 11                    26         Hurricanes /api/v1/franchises/26
    ## 12                    33           Panthers /api/v1/franchises/33
    ## 13                    31          Lightning /api/v1/franchises/31
    ## 14                    24           Capitals /api/v1/franchises/24
    ## 15                    11         Blackhawks /api/v1/franchises/11
    ## 16                    12          Red Wings /api/v1/franchises/12
    ## 17                    34          Predators /api/v1/franchises/34
    ## 18                    18              Blues /api/v1/franchises/18
    ## 19                    21             Flames /api/v1/franchises/21
    ## 20                    27          Avalanche /api/v1/franchises/27
    ## 21                    25             Oilers /api/v1/franchises/25
    ## 22                    20            Canucks /api/v1/franchises/20
    ## 23                    32              Ducks /api/v1/franchises/32
    ## 24                    15              Stars /api/v1/franchises/15
    ## 25                    14              Kings /api/v1/franchises/14
    ## 26                    29             Sharks /api/v1/franchises/29
    ## 27                    36       Blue Jackets /api/v1/franchises/36
    ## 28                    37               Wild /api/v1/franchises/37
    ## 29                    35               Jets /api/v1/franchises/35
    ## 30                    28            Coyotes /api/v1/franchises/28
    ## 31                    38     Golden Knights /api/v1/franchises/38

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
