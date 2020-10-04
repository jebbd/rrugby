validate_rugbypass_url<-function(url){
  live_match<-stringr::str_detect(url,"rugbypass.com/live/")
  stats_page<-stringr::str_detect(url,"/stats/$")
  return(all(c(live_match,stats_page)))
}

extract_venue<-function(url){
  stringr::str_extract(url,"at\\-[-\\w]+\\-on\\-")%>%
    stringr::str_replace("at\\-","")%>%
    stringr::str_replace("-on-","")%>%
    stringr::str_replace_all("-"," ")->pattern
  return(pattern)
}

pull_venue<-function(pattern){
  dplyr::filter(rugby_stadiums,stringr::str_detect(Stadium,stringr::regex(paste0("^",pattern),ignore_case = T)))->df
  if(nrow(df)==1){
    return(dplyr::pull(df,Stadium))
  }else{
    dplyr::filter(df,active)->df
    if(nrow(df)==1){
      return(dplyr::pull(df,Stadium))
    }else{
      message("No match found, returning formatted input")
      return(stringr::str_to_title(pattern))
    }
  }
}

possibly_read_html<-purrr::possibly(xml2::read_html,otherwise=NA_character_)

extract_game_url<-function(html){
  html%>%rvest::html_nodes("link")%>%
    rvest::html_attr("href")%>%
    purrr::keep(validate_rugbypass_url)->url
  return(url)
}

format_venue<-function(url){
  extract_venue(url)->pattern
  pull_venue(pattern)->venue
  ## check if it's the official name of Ellis Park which I don't like
  if(stringr::str_detect(pattern,"emirates")){
    suppressMessages(pull_venue(pattern="ellis park")->venue)
  }else if(purrr::is_empty(venue)){
    stringr::str_to_title(pattern)->venue
  }else if(stringr::str_detect(pattern,"murrayfield")){
    venue<-"Murrayfield"
  }
  return(venue)
}


