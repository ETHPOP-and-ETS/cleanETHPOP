% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_fertilityR.R
\name{clean_fertility}
\alias{clean_fertility}
\title{clean_fertility}
\usage{
clean_fertility(
  dir_path = "D:/data/ETHPOP/Leeds2/Fertility",
  m_to_100_f = 105,
  rtn = TRUE,
  save_to_file = TRUE,
  save_name = "clean_fertility",
  save_folder = here::here("output_data"),
  save_format = "csv"
)
}
\arguments{
\item{dir_path}{raw data location}

\item{rtn}{TRUE/FALSE}

\item{save_to_file}{TRUE/FALSE}

\item{save_name}{string}

\item{save_folder}{output data location}

\item{save_format}{csv or RData}
}
\value{

}
\description{
clean_fertility
}
\examples{
  clean_fertility()

}
