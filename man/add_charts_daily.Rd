% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/daytrading.R
\name{add_charts_daily}
\alias{add_charts_daily}
\title{Create plotly chart for daily time series data}
\usage{
add_charts_daily(cap, tmData)
}
\arguments{
\item{cap}{is the value of portfolio}

\item{tmData}{is daily option day trading data frame with the following fields:
'time','value','yield','ntrades','winners','pcwinners','contrtrade','roi','duration'}
}
\value{
plotly picture
}
\description{
Create plotly chart for daily time series data
}
