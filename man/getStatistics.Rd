% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/daytrading.R
\name{getStatistics}
\alias{getStatistics}
\title{Gets trade statistics}
\usage{
getStatistics(TRADES, WD, cap, startDate, endDate, curDate, level = "global")
}
\arguments{
\item{TRADES}{The data frame of trades}

\item{WD}{is the frame of market working dates with the following fields:
"dates","dayOfWeek","workDates"}

\item{cap}{The initial capital}

\item{startDate}{is the first global trade date}

\item{endDate}{is the last global trade date}

\item{curDate}{is the current date}

\item{level}{= 'global', 'year' or 'month'}
}
\value{
The list of statistics that includes:
total P or L,total commision,yield,yield percentage,number of trades,
number of winners, winners percentage,number of contracts,maximal win and lost,
average capital at risk,average ROI,average trade duration,
longest and shortest trade durations, number of trading days,
number of trading days used, percentage of trading days used
}
\description{
Gets statistics for a set of trades
}
