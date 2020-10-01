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
#' @importFrom purrr "map" "reduce" "map_dfr"
#' @importFrom glue "glue"
#' @export
get_team_stats<-function(data,is_html=FALSE,wide=TRUE){
  if(!is_html){
    html<-xml2::read_html(data)
  }else{
    html<-data
  }
  html%>%rvest::html_nodes(".title-menu span")%>%rvest::html_text()%>%purrr::pluck(2)%>%
    stringr::str_split(" v ")%>%purrr::flatten()->teams

  pattern<-'[\\w\\s\\.]+'
  raw_pi_df<-html%>%rvest::html_nodes(".key-stats-group-graph")%>%rvest::html_text()%>%
    stringr::str_extract_all(pattern=pattern,simplify=TRUE)%>%
    tibble::as_tibble(.name_repair = ~glue::glue("V{1:25}"))%>%
    dplyr::select(c(6:9,19:21))

  suppressWarnings(
    dplyr::bind_rows(
      raw_pi_df%>%dplyr::select(1:4)%>%dplyr::mutate(Metric=dplyr::case_when(
        is.na(as.numeric(V7)) ~ V7,
        is.na(as.numeric(V6)) ~ V6
      ),Away=dplyr::case_when(
        is.na(as.numeric(V7)) ~ V8,
        is.na(as.numeric(V6)) ~ V7
      ),Home=dplyr::case_when(
        is.na(as.numeric(V7)) ~ V9,
        is.na(as.numeric(V6)) ~ V8
      ))%>%dplyr::select(Metric,Home,Away)
      ,
      raw_pi_df%>%dplyr::select(5:7)%>%dplyr::filter(!V19=="")%>%
        `colnames<-`(c("Metric","Away","Home"))
    )->team_stats
  )

  dplyr::bind_rows(
    team_stats,
    html%>%rvest::html_nodes(".key-stats-group")%>%rvest::html_node(".stat-bars")%>%
      rvest::html_nodes(".stat-bars-item")%>%rvest::html_text()%>%
      stringr::str_match_all("(\\d+)%* (\\d+)%* ([\\w\\s%]+)")%>%
      purrr::map_dfr(~{
        unfmt<-tibble::as_tibble(.x)
        colnames(unfmt)<-c("Deets","Home","Away","Metric")
        unfmt%>%dplyr::select(-Deets)%>%
          dplyr::mutate(Metric=stringr::str_replace(Metric,"\\s+$",""))
      })
  )%>%
    tibble::as_tibble()%>%dplyr::mutate(dplyr::across(c(Home,Away),as.double))%>%
    dplyr::mutate(dplyr::across(c(Home,Away),~dplyr::case_when(
      Metric=="Possession" ~ as.integer(.x*100),
      TRUE ~ as.integer(.x)
    )))%>%
    mutate(Metric=case_when(
      Metric=="Kicks in play" ~ "% Kicks in play",
      TRUE ~ Metric
    ))->team_stats

  if(wide){
    team_stats%<>%dplyr::distinct()%>%
      tidyr::pivot_longer(-Metric,names_to="Home/Away",values_to="Value")%>%
      tidyr::pivot_wider(names_from=Metric,values_from=Value)%>%
      tibble::add_column(Team=unlist(teams))%>%dplyr::relocate(Team)
  }else{
    colnames(team_stats)<-c("Metric",unlist(teams))
  }

  return(dplyr::distinct(team_stats))
}
