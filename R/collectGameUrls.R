#' Collect game urls for a competition by year/season
#' @description
#' Extract game urls from a rugbypass competition landing page.
#' These urls identify an individual game and end in ".../stats/"
#'
#' @param competitions a vector of competitions to search, check \code{rugby_comps} tibble for accepted values
#' @param years a vector of years ; cannot be used with from and to
#' @param from a year to start searching from
#' @param to a year to search until ; from and to must be used together
#' @param season if set to \code{TRUE} the competition is assumed to span multiple years, usually for Northern hemisphere competitions
#' @returns
#' returns a list of rugbypass.com game urls
#'
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom polite "bow" "scrape"
#' @importFrom tibble "as_tibble" "tibble"
#' @import dplyr
#' @import stringr
#' @importFrom rvest "html_nodes" "html_attr"
#' @importFrom xml2 "read_html"
#' @importFrom purrr "map" "compact" "map_chr"
#' @importFrom glue "glue"
#' @export
collect_game_urls<-function(competitions,years=NULL,from=NULL,to=NULL,season=FALSE){
  if(is.null(from) & is.null(years) & is.null(to)){
    stop("Must provide a date range via `years`, or by `from` - `to`")
  }else if(!is.null(from) & !is.null(to) & is.null(years)){
    years<-seq(from, to, by=1)
  }else if((!is.null(from) | !is.null(to)) & !is.null(years)){
    message("Only provide one of years or from-to, defaulting to `years`")
  }else{
    stop("Improper date range provided")
  }

  if(to<from){
    stop("`to` cannot be earlier than `from`")
  }

  if(season){
    purrr::map_chr(years,~glue::glue("{.x-1}-{.x}"))->years
  }

  map(competitions,~{
    comp<-.x
    map(years,~{
      url<-glue::glue("https://www.rugbypass.com/{comp}/matches/{.x}/")
      polite::bow(url)%>%
        possibly_scrape()->html
        if(!is.na(html)){
          html%>%rvest::html_nodes(".link-box")%>%
            rvest::html_attr('href')%>%return()
        }else{
          return(NULL)
        }
    })
  })%>%unlist%>%purrr::compact%>%unique%>%return()
}
