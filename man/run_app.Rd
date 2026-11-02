\name{run_app}
\alias{run_app}
\title{Launch CodebookR}
\usage{run_app(launch.browser = TRUE, host = "127.0.0.1", port = NULL)}
\arguments{
\item{launch.browser}{Whether to open the application in a browser.}
\item{host}{Interface on which to listen. Defaults to the local computer.}
\item{port}{Port to use, or NULL for Shiny's default selection.}
}
\description{Launch the local Shiny application for researcher-approved codebook-assisted preprocessing. AI use is optional and requires the ellmer package and a user-provided API key.}
\value{The value returned by \code{shiny::runApp()}, invisibly.}
\examples{if (interactive()) run_app()}
