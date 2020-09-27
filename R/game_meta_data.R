library(tidyverse)
library(lubridate)

url<-"https://www.rugbypass.com/live/the-rugby-championship/new-zealand-vs-south-africa-at-westpac-stadium-on-15092018/2018/stats/"

urls<-read_lines("~/Desktop/my_r_packages/rrugby/international_urls.txt")
stadiums<-read_csv("~/Desktop/my_r_packages/rrugby/stadiums.csv")

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
date
possible_read<-purrr::possibly(xml2::read_html,otherwise=NA_character_)
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

future::plan(multisession,workers=8)
game_meta<-furrr::future_map_dfr(urls,~get_game_metadata(.x,stadiums))

write_csv(game_meta,"~/Desktop/my_r_packages/rrugby/game_meta_data.csv")
