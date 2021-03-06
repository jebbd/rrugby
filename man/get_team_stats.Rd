% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getTeamStats.R
\name{get_team_stats}
\alias{get_team_stats}
\title{Parse a rugbypass.com url to retrieve team level stats}
\usage{
get_team_stats(data, is_html = FALSE, wide = TRUE)
}
\arguments{
\item{data}{a rugbypass.com url to retrieve data from. Can aslo be previously extracted html}

\item{is_html}{is the data a url or extracted html. Default is \code{FALSE} i.e. the data variable holds a url}

\item{wide}{return the data in wide or long format. Deafult is \code{TRUE} for wide data}
}
\value{
Returns tibble with containing player level statistics from rugbypass.com
}
\description{
Extract the team statistics from a rugbypass match url.
These urls identify an individual game and end in ".../stats/"
}
