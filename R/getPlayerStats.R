#' Parse a rugbypass.com url to retrieve player level stats
#' @description
#' Extract the player statistics from a rugbypass match url.
#' These urls identify an individual game and end in ".../stats/"
#'
#' @param data a rugbypass.com url to retrieve data from. Can aslo be previously extracted html
#' @param is_html is the data a url or extracted html. Default is \code{FALSE} i.e. the data variable holds a url
#' @returns
#' Returns tibble with containing player level statistics from rugbypass.com
#'
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom tibble "as_tibble"
#' @importFrom dplyr "left_join" "mutate" "relocate" "across" "rename_with" "filter" "everything" "if_else" "case_when" "nth" "rename" "coalesce"
#' @importFrom rvest "html_nodes" "html_table"
#' @importFrom xml2 "read_html"
#' @importFrom stringr "str_replace" "str_replace_all"
#' @importFrom purrr "map" "reduce" "modify_if"
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
  player_stats<-bind_rows(
    imap(tables[1:5],~{
      .x%>%rename(Number=1)%>%
        mutate(Team=dplyr::nth(colnames(.),2),`Home_or_Away`="Home")%>%
        filter(!is.na(Number))%>%rename(Player=2)%>%
        rename_with(return_col_name,-c(Team,Home_or_Away,Player,Number),.y)})%>%
      reduce(left_join,by=c("Player","Number","Team","Home_or_Away"))%>%as_tibble,
    imap(tables[6:10],~{
      .x%>%rename(Number=1)%>%
        mutate(Team=dplyr::nth(colnames(.),2),`Home_or_Away`="Away")%>%
        filter(!is.na(Number))%>%rename(Player=2)%>%
        rename_with(return_col_name,-c(Team,Home_or_Away,Player,Number),.y)})%>%
      reduce(left_join,by=c("Player","Number","Team","Home_or_Away"))%>%as_tibble
    )%>%
    relocate(Home_or_Away,Team)
  return(player_stats)
}

return_col_name<-function(col,index){
  attack<-list('T' = 'Tries','M' = 'Metres_Carried','C' = 'Runs',
    'DB' = 'Defenders_Beaten','CB' = 'Clean_Breaks','P' = 'Passes',
    'O' = 'Offloads','TC' = 'Turnovers_Conceded','TA' = 'Try_Assist',
    'Pts' = 'Points')
  defense<-list('T' = 'Tackles','MT' = 'Missed_Tackles','TW' = 'Turnovers_Won')
  kicking<-list('K' = 'Kicks_From_Hand','C' = 'Conversion_Goals',
    'PG' = 'Penalty_Goals','DG' = 'Drop_Goals_Converted')
  set_plays<-list('TW' = 'Lineout_Won_Own_Throw',
                  'LS' = 'Lineout_Won_Steal')
  discipline<-list('PC' = 'Penalties_Conceded','RC' = 'Red_Cards',
                   'YC' = 'Yellow_Cards')
  hash<-switch(index,
               attack,
               defense,
               kicking,
               set_plays,
               discipline
  )
  hash[cols]%>%purrr::modify_if(is.null,~NA_character_)%>%
    unlist%>%unname%>%dplyr::coalesce(cols)
}
