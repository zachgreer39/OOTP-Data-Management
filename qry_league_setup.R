library(tidyverse);
setwd("~/Desktop/github/OOTP-Data-Management");
##requires running qry_ootpCSV.R

leagues=function(){
  
  ootpCSV("leagues.csv") %>%
    transmute(season_year, 
              leagueID=league_id, 
              leagueDESC=abbr, 
              leagueNAME=name, 
              levelID=league_level, 
              roster_limit=rules_active_roster_limit, 
              rosterSEC_limit=rules_secondary_roster_limit, 
              rosterEXP_limit=rules_expanded_roster_limit,
              service_daysYEAR=rules_min_service_days, 
              service_arbMIN=rules_salary_arbitration_minimum_years, 
              service_faMIN=rules_fa_minimum_years, 
              season_games=rules_schedule_games_per_team,
              start_date=lubridate::ymd(start_date),
              file_date=lubridate::ymd(current_date), 
              file_name) %>% 
    arrange(levelID, leagueID)
  
};
divisions=function(){
  
  ootpCSV("divisions.csv") %>%
    left_join(ootpCSV("leagues.csv") %>% 
                transmute(league_id, 
                       leagueDESC=abbr, 
                       levelID=league_level, 
                       season_year,
                       file_date=lubridate::ymd(current_date), 
                       file_name), 
              by=c("league_id", "file_name")) %>% 
    left_join(ootpCSV("sub_leagues.csv") %>% 
                transmute(league_id, 
                          sub_league_id, 
                          subleagueDESC=abbr, 
                          file_name), 
              by=c("league_id", "sub_league_id", "file_name")) %>% 
    transmute(leagueID=league_id, 
              leagueDESC, 
              levelID, 
              subleagueID=sub_league_id, 
              subleagueDESC=ifelse(subleagueDESC=="", 
                                   leagueDESC, 
                                   subleagueDESC), 
              divisionID=division_id, 
              divisionDESC=str_trim(paste(subleagueDESC, 
                                          str_trim(str_remove(name, 
                                                              "Division")))), 
              season_year, 
              file_date,
              file_name)
  
};
users=function(){
  
  ootpCSV("human_managers.csv") %>% 
    left_join(leagues() %>% 
                transmute(league_id=leagueID, levelID, season_year, file_date), 
              by="league_id") %>% 
    transmute(userID=human_manager_id, 
              userNAME=paste(first_name, last_name),
              leagueID=league_id,
              levelID,
              teamID=team_id, 
              orgID=organization_id,
              is_commish, 
              season_year,
              file_date,
              file_name) %>% 
    mutate(levelID=ifelse(is.na(levelID), 0, levelID), 
           season_year=ifelse(is.na(season_year), 
                              max(leagues()$season_year), 
                              season_year))
  
};
nations=function(){
  
  ootpCSV("nations.csv") %>% 
    left_join(ootpCSV("cities.csv") %>% 
                transmute(capital_id=city_id, 
                          capitalNAME=name), 
              by=c("capital_id")) %>%
    transmute(nationID=nation_id, 
              nationDESC=abbreviation,
              nationNAME=short_name, 
              nationPOP=population, 
              capitalID=capital_id, 
              capitalNAME, 
              baseball_quality, 
              season_year=leagues() %>% filter(levelID==1) %>% 
                pull(season_year) %>% unique(),
              file_date=leagues() %>% pull(file_date) %>% unique(), 
              file_name)
  
};
cities=function(){
  
  ootpCSV("cities.csv") %>% 
    left_join(ootpCSV("nations.csv") %>% 
                transmute(nation_id, 
                          nationDESC=abbreviation), 
              by="nation_id") %>%
    left_join(ootpCSV("states.csv") %>% 
                transmute(state_id, 
                          nation_id, 
                          stateDESC=abbreviation, 
                          stateNAME=name), 
              by=c("state_id", "nation_id")) %>% 
    transmute(cityID=city_id, 
              cityNAME=name, 
              stateID=state_id, 
              stateDESC, 
              stateNAME,
              nationID=nation_id, 
              nationDESC, 
              lat=latitude, 
              lon=longitude, 
              cityPOP=population, 
              season_year=leagues() %>% filter(levelID==1) %>% 
                pull(season_year) %>% unique(),
              file_date=leagues() %>% pull(file_date) %>% unique(), 
              file_name)
  
};
parks=function(){
  
  ootpCSV("parks.csv") %>% 
    transmute(parkID=park_id, 
              parkNAME=name, 
              turf, 
              type, 
              capacity, 
              season_year=leagues() %>% filter(levelID==1) %>% 
                pull(season_year) %>% unique(),
              file_date=leagues() %>% pull(file_date) %>% unique(), 
              file_name)
  
};
teams=function(){
  
  orgs=function(){
    
    ootpCSV("teams.csv") %>% 
      filter(parent_team_id==0 & allstar_team==0) %>% 
      transmute(orgID=ifelse(parent_team_id==0, 
                             team_id, 
                             parent_team_id), 
                orgDESC=abbr,
                user_controlled=human_team, 
                userID=human_id)
    
  }
  
  ootpCSV("teams.csv") %>%
    filter(allstar_team==0) %>%
    mutate(orgID=ifelse(parent_team_id==0, 
                        team_id, 
                        parent_team_id)) %>%
    left_join(orgs(), 
              by="orgID") %>%
    left_join(divisions(), 
              by=c("league_id"="leagueID", 
                   "level"="levelID", 
                   "sub_league_id"="subleagueID", 
                   "division_id"="divisionID", 
                   "file_name")) %>%
    left_join(cities() %>%
                transmute(city_id=cityID, 
                          cityNAME=ifelse(nationID==206, 
                                          paste0(cityNAME, ", ", stateDESC), 
                                          paste0(cityNAME, ", ", nationDESC))), 
              by="city_id") %>% 
    left_join(parks() %>% 
                transmute(park_id=parkID, 
                          parkNAME), 
              by="park_id") %>%
    transmute(teamID=team_id, 
              teamDESC=abbr, 
              teamNAME=paste(name, nickname), 
              leagueID=league_id, 
              leagueDESC,
              levelID=level,
              subleagueID=sub_league_id, 
              subleagueDESC, 
              divisionID=division_id, 
              divisionDESC,
              orgID,
              orgDESC,
              cityID=city_id, 
              cityNAME,
              parkID=park_id, 
              parkNAME,
              user_controlled, 
              userID,
              season_year,
              file_date, 
              file_name)
  
};