#' Parse a rugbypass.com url to retrieve team level stats
#' @description
#' Extract the team statistics from a rugbypassmatch url.
#' These urls identify an individual game and end in ".../stats/"
#'
#' @param data a rugbypass.com url to retrieve data from. Can aslo be previously extracted html
#' @param is_html is the data a url or extracted html. Default is `FALSE`` i.e. the data variable holds a url
#' @returns
#' Returns tibble with containing game meta data (e.g. data, venue, score) from rugbypass.com
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
get_game_metadata<-function(data,is_html=FALSE){
  if(!is_html){
    if(!validate_rugbypass_url(data)){
      stop(glue::glue("{data} is not a valid rugbypass match url"))
    }

    venue<-suppressMessages(format_venue(address))
    date<-stringr::str_extract(data,"\\-on\\-\\w+")%>%stringr::str_replace("on\\-","")%>%
      lubridate::dmy(.)
    html<-possibly_read_html(data)
    address<-data
  }else{

    date<-data%>%rvest::html_nodes(".title-menu")%>%
            rvest::html_text()%>%stringr::str_extract("\\d{2} [:alpha:]+ \\d{4}")%>%
            lubridate::dmy()
    extract_game_url(data)->address
    venue<-suppressMessages(format_venue(address))
    html<-data
  }

  if(!is.na(html)){
    html%>%rvest::html_nodes(".title-menu span")%>%rvest::html_text()%>%purrr::pluck(1)->competition
    html%>%rvest::html_nodes(".title-menu span")%>%rvest::html_text()%>%purrr::pluck(2)%>%
      stringr::str_split(" v ")%>%purrr::flatten()->teams
    html%>%rvest::html_nodes('.live-match-centre-header .score .away')%>%
      rvest::html_text()%>%as.numeric()->away_score
    html%>%rvest::html_nodes('.live-match-centre-header .score .home')%>%
      rvest::html_text()%>%as.numeric()->home_score
    glue::glue("{home_score}-{away_score}")->score
    html%>%rvest::html_nodes(".key-stats-group")%>%rvest::html_node(".stat-bars")%>%
      rvest::html_children()%>%html_nodes(" div .away")%>%
      rvest::html_attrs()%>%purrr::reduce(dplyr::bind_rows)%>%
      dplyr::slice(1)%>%pull(style)%>%stringr::str_extract("#[:alnum:]+")->away_strip
    html%>%rvest::html_nodes(".key-stats-group")%>%rvest::html_node(".stat-bars")%>%
      rvest::html_children()%>%html_nodes(" div .home")%>%
      rvest::html_attrs()%>%purrr::reduce(dplyr::bind_rows)%>%
      dplyr::slice(1)%>%pull(style)%>%stringr::str_extract("#[:alnum:]+")->home_strip
  }else{
    stop("Data is not readable")
  }

  return(
    tibble::tibble(url=address,Date=date,Stadium=venue,Competition=competition,
               Home=teams[[1]],Away=teams[[2]],Score=score,
               Home_strip=home_strip,Away_strip=away_strip)
  )
}
