#' Parse a rugbypass.com url to retrieve team level stats
#' @description
#' Extract the team statistics from a rugbypassmatch url.
#' These urls identify an individual game and end in ".../stats/"
#'
#' @param url rugbypass.com url to retrieve data from. Can aslo be previously extracted html
#' @returns
#' Returns tibble with containing player level statistics from rugbypass.com
#'
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom tibble "as_tibble" "tibble"
#' @import dplyr
#' @import stringr
#' @importFrom rvest "html_nodes" "html_table"
#' @importFrom xml2 "read_html"
#' @importFrom purrr "map" "reduce" "map_dfr" "possibly" "is_empty"
#' @importFrom glue "glue"
#' @importFrom lubridate "dmy"
#' @export
get_game_metadata<-function(url){
extract_venue(url)->pattern
pull_venue(pattern,rugby_stadiums)->venue

## check if it's the official name of Ellis Park which I don't like
if(stringr::str_detect(pattern,"emirates")){
  pull_venue(pattern="ellis park",rugby_stadiums)->venue
}else if(purrr::is_empty(venue)){
  stringr::str_to_title(pattern)->venue
}else if(stringr::str_detect(pattern,"murrayfield")){
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
  html%>%rvest::html_nodes(".key-stats-group")%>%rvest::html_node(".stat-bars")%>%
    rvest::html_children()%>%html_nodes(" div .away")%>%
    rvest::html_attrs()%>%purrr::reduce(dplyr::bind_rows)%>%
    dplyr::slice(1)%>%pull(style)%>%stringr::str_extract("#[:alnum:]+")->away_strip
  html%>%rvest::html_nodes(".key-stats-group")%>%rvest::html_node(".stat-bars")%>%
    rvest::html_children()%>%html_nodes(" div .home")%>%
    rvest::html_attrs()%>%purrr::reduce(dplyr::bind_rows)%>%
    dplyr::slice(1)%>%pull(style)%>%stringr::str_extract("#[:alnum:]+")->home_strip
}else{
  teams<-list(NA_character_,NA_character_)
  competition<-NA_character_
  venue<-NA_character_
  score<-NA_character_
  away_strip<-NA_character_
  home_strip<-NA_character_
}
return(
  tibble::tibble(Address=url,Date=date,Stadium=venue,Competition=competition,
                 Home=teams[[1]],Away=teams[[2]],Score=score,
                 Home_strip=home_strip,Away_strip=Away_strip)
)
}

extract_venue<-function(url){
  stringr::str_extract(url,"at\\-[-\\w]+\\-on\\-")%>%
    stringr::str_replace("at\\-","")%>%
    stringr::str_replace("-on-","")%>%
    stringr::str_replace_all("-"," ")->pattern
  return(pattern)
}

pull_venue<-function(pattern,rugby_stadiums){
  dplyr::filter(rugby_stadiums,stringr::str_detect(Stadium,stringr::regex(glue::glue("^{pattern}"),ignore_case = T)))->df
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
