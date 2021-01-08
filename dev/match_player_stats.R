hh<-read_html('https://www.rugbypass.com/live/the-rugby-championship/new-zealand-vs-south-africa-at-westpac-stadium-on-15092018/2018/stats/')%>%
    html_nodes("table")%>%html_table(fill=TRUE)
my_hash<-hh[11:length(hh)]%>%
  map(~(.x%>%set_names(c("key","value"))))%>%
  bind_rows()

return_col_name<-function(col,index){
    attack<-list(
      'T' = 'Tries','M' = 'Metres_Carried','C' = 'Runs',
      'DB' = 'Defenders_Beaten','CB' = 'Clean_Breaks','P' = 'Passes',
      'O' = 'Offloads',
      'TC' = 'Turnovers_Conceded',
      'TA' = 'Try_Assist',
      'Pts' = 'Points')
    defense<-list(
      'T' = 'Tackles',
      'MT' = 'Missed_Tackles',
      'TW' = 'Turnovers_Won')
    kicking<-list(
      'K' = 'Kicks_From_Hand',
      'C' = 'Conversion_Goals',
      'PG' = 'Penalty_Goals',
      'DG' = 'Drop_Goals_Converted')
    set_plays<-list(
      'TW' = 'Lineout_Throw_Won_Clean',
      'LW' = 'Lineouts_Won',
      'LS' = 'Lineout_Won_Steal')
    discipline<-list(
      'PC' = 'Penalties_Conceded',
      'RC' = 'Red_Cards',
      'YC' = 'Yellow_Cards')
    hash<-switch(index,
      attack,
      defense,
      kicking,
      set_plays,
      discipline
    )
    hash[col]%>%unlist%>%unname
}

bind_rows(
imap(hh[1:5],~{
  .x%>%rename(Number=1)%>%
    mutate(Team=dplyr::nth(colnames(.),2),`Home_or_Away`="Home")%>%
    filter(!is.na(Number))%>%rename(Player=2)%>%
    rename_with(return_col_name,-c(Team,Home_or_Away,Player,Number),.y)})%>%
  reduce(left_join,by=c("Player","Number","Team","Home_or_Away"))%>%as_tibble,
imap(hh[6:10],~{
  .x%>%rename(Number=1)%>%
    mutate(Team=dplyr::nth(colnames(.),2),`Home_or_Away`="Away")%>%
    filter(!is.na(Number))%>%rename(Player=2)%>%
    rename_with(return_col_name,-c(Team,Home_or_Away,Player,Number),.y)})%>%
  reduce(left_join,by=c("Player","Number","Team","Home_or_Away"))%>%as_tibble
)%>%
relocate(Home_or_Away,Team)

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
