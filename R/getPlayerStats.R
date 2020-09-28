## url<-"https://www.rugbypass.com/live/the-rugby-championship/new-zealand-vs-south-africa-at-westpac-stadium-on-15092018/2018/stats/"
## make a function for parsing all tables in same way

#' Parse a rugbypass.com table list to extract home and away player level stats
#' @description
#' Extract the player statistics from a rugbydump match url.
#' These urls identify an individual game and end in ".../stats/"
#'
#' @param data, a rugbypass.com url to retrieve data from. Can aslo be previously extracted html
#' @param is_html, is the data a url or extracted html. Default is `FALSE`` i.e. the data variable holds a url
#' @returns
#' Retruns tibble with containing player level statistics from rugbypass.com
#'
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom tibble "as_tibble"
#' @import dplyr
#' @importFrom rvest "html_nodes" "html_table"
#' @importFrom xml2 "read_html"
#' @importFrom stringr "str_replace" "str_replace_all"
#' @importFrom purrr "map" "reduce"
#' @export
get_player_stats<-function(data,is_html=FALSE){

  if(!is_html){
    html<-xml2::read_html(data)
  }else{
    html<-data
  }
  ## pull tables which are player level data
  tables<-html %>% rvest::html_nodes("table") %>% rvest::html_table(fill=TRUE)
  ## map over the tables and merge them all
  player_stats<-purrr::map(seq(1,5),~parse_player_tables(tables,.x))%>%purrr::reduce(~dplyr::left_join(.x,.y,by = c("Number", "Player", "Sub", "Role", "Team", "Home/Away")))%>%
    dplyr::mutate(dplyr::across(-c("Number", "Player", "Sub", "Role", "Team", "Home/Away"),as.integer))%>%
    dplyr::relocate(`Home/Away`)
  return(player_stats)
}

parse_player_tables<-function(tables,index){
  away_ind=index+5
  key_ind=index+10
  dplyr::bind_rows(
    tibble::as_tibble(tables[[index]],.name_repair = make.names)%>%rename("Number"=X)%>%
    dplyr::rename_with(~stringr::str_replace(.x,"\\.","_"),dplyr::everything())%>%
    dplyr::filter(!is.na(Number))%>%
    dplyr::mutate(Sub=dplyr::if_else(Number>15,TRUE,FALSE),
          Role=dplyr::case_when(
            Number <= 8 ~ "Forward",
            Number <= 15 ~ "Back",
            Number <= 20 ~ "Forward",
            TRUE ~ "Back"
          ),Team=dplyr::nth(colnames(.),2),`Home/Away`="Home")%>%
    dplyr::rename("Player"=2),
    tibble::as_tibble(tables[[away_ind]],.name_repair = make.names)%>%dplyr::rename("Number"=X)%>%
      dplyr::rename_with(~stringr::str_replace(.x,"\\.","_"),dplyr::everything())%>%
      dplyr::filter(!is.na(Number))%>%
      dplyr::mutate(Sub=if_else(Number>15,TRUE,FALSE),
             Role=dplyr::case_when(
               Number <= 8 ~ "Forward",
               Number <= 15 ~ "Back",
               Number <= 20 ~ "Forward",
               TRUE ~ "Back"
             ),Team=dplyr::nth(colnames(.),2),`Home/Away`="Away")%>%
      dplyr::rename("Player"=2)
  )->extracted_data
  k<-length(colnames(extracted_data))-4
  colnames(extracted_data)[3:k]<-tables[[key_ind]][[2]]
  extracted_data %<>% dplyr::rename_with(~stringr::str_replace_all(.x," ","_"))
  return(extracted_data)
}

##### getting the team level stats from the bar/pie charts

parse_team_stats<-function(html,wide=TRUE){
pattern='[\\w\\s\\.]+'
raw_pi_df<-html%>%html_nodes(".key-stats-group-graph")%>%html_text%>%
  str_extract_all(pattern=pattern,simplify=TRUE)%>%as.data.frame%>%
  select(c(6:9,19:21))
suppressWarnings(
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
)

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
