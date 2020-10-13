#' Scrape a rugbypass.com url
#' @description
#' Extract data from a rugbypass match url.
#' These urls identify an individual game and end in ".../stats/"
#'
#' @param urls rugbypass.com url to retrieve data from.
#' @param as_list return data in 3 element list: \code{meta_data}, \code{team_stats} and \code{player_stats}; Default is \code{FALSE}
#' @returns
#' Returns nested tibble (or list) containing game, team and player level statistics from rugbypass.com
#'
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom tidyr "nest"
#' @importFrom polite "bow" "scrape"
#' @importFrom dplyr "mutate" "left_join"
#' @importFrom purrr "reduce"
#' @export
scrape_rugbypass<-function(url,as_list=FALSE){
  bow(url)->session
  possibly_scrape(session)->html

  get_game_metadata(html,is_html = TRUE)->gg
  get_player_stats(html,is_html = TRUE)->pp
  get_team_stats(html,is_html = TRUE)->tt

  if(as_list){
    return(
      list(meta_data=gg,team_stats=tt,player_stats=pp)
    )
  }else{
    gg%>%mutate(team_stats=list(tt),player_stats=list(pp))%>%return()
  }
}
