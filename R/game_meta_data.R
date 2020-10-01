#' Parse a rugbypass.com url to retrieve team level stats
#' @description
#' Extract the team statistics from a rugbypassmatch url.
#' These urls identify an individual game and end in ".../stats/"
#'
#' @param data a rugbypass.com url to retrieve data from. Can aslo be previously extracted html
#' @param is_html is the data a url or extracted html. Default is `FALSE`` i.e. the data variable holds a url
#' @param wide return the data in wide or long format. Deafult is `TRUE` for wide data
#' @returns
#' Returns tibble with containing player level statistics from rugbypass.com
#'
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom tibble "as_tibble"
#' @import dplyr
#' @importFrom rvest "html_nodes" "html_table"
#' @importFrom xml2 "read_html"
#' @importFrom stringr "str_replace" "str_replace_all" "str_extract_all"
#' @importFrom purrr "map" "reduce" "map_dfr" "possibly"
#' @importFrom glue "glue"
#' @importFrom lubridate "dmy"
#' @export
rugby_stadiums<-readr::read_csv("~/Desktop/my_r_packages/rrugby/data/stadiums.csv")

get_game_metadata<-function(url,stadiums){
extract_venue(url)->pattern
pull_venue(pattern,stadiums)->venue

## check if it's the official name of Ellis Park which I don't like
if(stringr::str_detect(pattern,"emirates")){
  pull_venue(pattern="ellis park",stadiums)->venue
} else if(purrr::is_empty(venue)){
  stringr::str_to_title(pattern)->venue
} else if(str_detect(pattern,"murrayfield")){
  venue<-"Murrayfield"
}

date<-stringr::str_extract(url,"\\-on\\-\\w+")%>%stringr::str_replace("on\\-","")%>%
  lubridate::dmy(.)


html<-possible_read(url)
if(!is.na(html)){
  html%>%rvest::html_nodes(".title-menu span")%>%rvest::html_text()%>%purrr::pluck(1)->competition
  html%>%rvest::html_nodes(".title-menu span")%>%rvest::html_text()%>%purrr::pluck(2)%>%
    stringr::str_split(" v ")%>%purrr::flatten()->teams
  html%>%rvest::html_nodes('.live-match-centre-header .score .away')%>%
    rvest::html_text()%>%as.numeric()->away_score
  html%>%rvest::html_nodes('.live-match-centre-header .score .home')%>%
    rvest::html_text()%>%as.numeric()->home_score
  glue::glue("{home_score} - {away_score}")->score
}else{
  teams<-list(NA_character_,NA_character_)
  competition<-NA_character_
  venue<-NA_character_
  score<-NA_character_
}
return(
  tibble::tibble(Address=url,Date=date,Stadium=venue,Competition=competition,Home=teams[[1]],Away=teams[[2]],Score=score)
)
}

extract_venue<-function(url){
  stringr::str_extract(url,"at\\-[-\\w]+\\-on\\-")%>%
    stringr::str_replace("at\\-","")%>%
    stringr::str_replace("-on-","")%>%
    stringr::str_replace_all("-"," ")->pattern
  return(pattern)
}

pull_venue<-function(pattern,stadiums){
  dplyr::filter(stadiums,stringr::str_detect(Stadium,stringr::regex(paste0("^",pattern),ignore_case = T)))->df
  if(nrow(df)==1){
    return(dplyr::pull(df,Stadium))
  }else{
    dplyr::filter(df,active)->df
    if(nrow(df)==1){
      return(dplyr::pull(df,Stadium))
    }else{
      return(stringr::str_to_title(pattern))
    }
  }
}

possible_read<-purrr::possibly(xml2::read_html,otherwise=NA_character_)
