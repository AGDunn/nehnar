% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reading.R
\name{book_finish_rate}
\alias{book_finish_rate}
\title{Show the number of books read and not-yet-finished on a give date}
\usage{
book_finish_rate(target_date = Sys.Date(), my_notes = NULL)
}
\arguments{
\item{target_date}{a date; default is Sys.Date()}

\item{my_notes}{a dataframe of read attempts; default NULL}
}
\value{
a one-row tibble of two columns: the number of books finished on
  target_date, then the number unfinished on target_date.
}
\description{
Given a target date, returns a tibble which shows how many read-throughs
of books were finished on before that date and how many were 'in 
progress' (i.e. unfinished) on that date.
}
\keyword{book}
\keyword{notes,}
\keyword{summary}
