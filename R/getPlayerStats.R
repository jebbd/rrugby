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
#' @importFrom dplyr "left_join" "mutate" "relocate" "across" "rename_with" "filter" "everything" "if_else" "case_when" "nth" "rename"
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
  player_stats<-purrr::map(seq(1,5),~parse_player_tables(tables,.x))%>%
    purrr::reduce(~dplyr::left_join(.x,.y,by = c("Number", "Player", "Sub", "Role", "Team", "Home/Away")))%>%
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
