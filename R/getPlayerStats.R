library(magrittr)
library(tidyverse)
library(rvest)

url<-"https://www.rugbypass.com/live/the-rugby-championship/new-zealand-vs-south-africa-at-westpac-stadium-on-15092018/2018/stats/"

html<-read_html(url)

## pull tables which are player level data
tables<-html %>% html_nodes("table") %>% html_table(fill=TRUE)

## make a function for parsing all tables in same way
parse_player_tables<-function(tables,index){
  away_ind=index+5
  key_ind=index+10
  bind_rows(
    as_tibble(tables[[index]],.name_repair = make.names)%>%rename("Number"=X)%>%
    rename_with(~str_replace(.x,"\\.","_"),everything())%>%
    filter(!is.na(Number))%>%
    mutate(Sub=if_else(Number>15,TRUE,FALSE),
          Role=case_when(
            Number <= 8 ~ "Forward",
            Number <= 15 ~ "Back",
            Number <= 20 ~ "Forward",
            TRUE ~ "Back"
          ),Team=nth(colnames(.),2),`Home/Away`="Home")%>%
    rename("Player"=2),
    as_tibble(tables[[away_ind]],.name_repair = make.names)%>%rename("Number"=X)%>%
      rename_with(~str_replace(.x,"\\.","_"),everything())%>%
      filter(!is.na(Number))%>%
      mutate(Sub=if_else(Number>15,TRUE,FALSE),
             Role=case_when(
               Number <= 8 ~ "Forward",
               Number <= 15 ~ "Back",
               Number <= 20 ~ "Forward",
               TRUE ~ "Back"
             ),Team=nth(colnames(.),2),`Home/Away`="Away")%>%
      rename("Player"=2)
  )->qq
  k<-length(colnames(qq))-4
  colnames(qq)[3:k]<-tables[[key_ind]][[2]]
  qq%<>%rename_with(~str_replace_all(.x," ","_"))
  return(qq)
}

## map over the tables and merge them all
player_stats<-map(seq(1,5),~parse_player_tables(tables,.x))%>%reduce(left_join)%>%
  mutate(across(-c("Number", "Player", "Sub", "Role", "Team", "Home/Away"),as.integer))

##### getting the team level stats from the bar/pie charts

parse_team_stats<-function(html,wide=FALSE){
pattern='[\\w\\s\\.]+'
raw_pi_df<-html%>%html_nodes(".key-stats-group-graph")%>%html_text%>%
  str_extract_all(pattern=pattern,simplify=TRUE)%>%as.data.frame%>%
  select(c(6:9,19:21))

bind_rows(
raw_pi_df%>%select(1:4)%>%mutate(Metric=case_when(
  is.na(as.numeric(V7)) ~ V7,
  is.na(as.numeric(V6)) ~ V6
),Away=case_when(
  is.na(as.numeric(V7)) ~ V8,
  is.na(as.numeric(V6)) ~ V7
),Home=case_when(
  is.na(as.numeric(V7)) ~ V9,
  is.na(as.numeric(V6)) ~ V8   
))%>%select(Metric,Home,Away)
,
raw_pi_df%>%select(5:7)%>%filter(!V19=="")%>%
  `colnames<-`(c("Metric","Away","Home"))
)->team_stats

bind_rows(
team_stats,
html%>%html_nodes(".key-stats-group")%>%html_node(".stat-bars")%>%
  html_nodes(".stat-bars-item")%>%html_text%>%
  str_match_all("(\\d+)%* (\\d+)%* ([\\w\\s%]+)")%>%
  map_df(~{
    qq<-as.tibble(.x)
    colnames(qq)<-c("Deets","Home","Away","Metric")
    qq%>%select(-Deets)%>%mutate(Metric=str_replace(Metric,"\\s+$",""))
  })
)%>%as_tibble%>%mutate(across(c(Home,Away),as.double))%>%
mutate(across(c(Home,Away),~case_when(
  Metric=="Possession" ~ as.integer(.x*100),
  TRUE ~ as.integer(.x)
)))->team_stats

if(wide){
bind_rows(
select(team_stats,Metric,Home)%>%
  pivot_wider(names_from = Metric,values_from=Home)%>%
  mutate(Team="Home"),
select(team_stats,Metric,Away)%>%
  pivot_wider(names_from = Metric,values_from=Away)%>%
  mutate(Team="Away")
)%>%relocate(Team)->team_stats
}

return(team_stats)
}

### checking if shit worked yo
Attack%>%select(-c(Number,Player,Team,Role,Sub))%>%
  mutate(across(-`Home/Away`,as.numeric))%>%group_by(`Home/Away`)%>%
  summarise(across(everything(),~sum(.x)))%>%t -> qq
colnames(qq)<-qq[1,]
qq%>%as.data.frame()%>%slice(2:n())%>%rownames_to_column("Metric")%>%
  as_tibble%>%mutate(across(-Metric,as.numeric)) -> qq

team_stats%>%as_tibble%>%mutate(across(-Metric,as.double))%>%
  mutate(Metric=case_when(
    Metric=="Metres carried" ~ "Carries_Metres",
    TRUE ~ Metric
  ))->team_stats