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
    pp%>%dplyr::mutate(url=url)%>%
      tidyr::nest(data = c(`Home/Away`, Number, Player, Tries, Carries_Metres, Runs, Defenders_Beaten,
                           Clean_Breaks, Passes, Offload, Turnovers_Conceded, Try_Assist,
                           Points, Sub, Role, Team, Tackles, Missed_Tackles, Turnover_Won,
                           Kicks_From_Hand, Conversion_Goals, Penalty_Goals, Drop_Goals_Converted,
                           Lineout_Throw_Won_Clean, Lineouts_Won, Lineout_Won_Steal,
                           Penalties_Conceded, Red_Cards, Yellow_Cards))%>%
      dplyr::rename(player_stats=data)->pp

    tt%>%dplyr::mutate(url=url)%>%
      tidyr::nest(data = c(Team, `Home/Away`, Possession, Passes, Tackles,
                           `% Kicks in play`, `Rucks won`, Tries, Conversions,
                           `Rucks lost`, `Metres carried`, Carries, `Defenders beaten`,
                           `Clean breaks`, `Missed tackles`, `Turnovers won`,
                           `Conversions missed`, `Penalty goals`, `Penalty goals missed`,
                           `Drop goals`, `Rucks won %`, `Mauls won`, `Lineouts won`,
                           `Lineouts lost`, `Lineouts won %`, `Scrums won`,
                           `Scrums lost`,`Scrums won %`, `Penalties conceded`,
                           `Red cards`, `Yellow cards`))%>%
      dplyr::rename(team_stats=data)->tt

    purrr::reduce(list(gg,tt,pp),~dplyr::left_join(.x,.y,by="url"))%>%return()
  }
}
