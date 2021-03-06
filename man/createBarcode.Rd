% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/labels.R
\name{createBarcode}
\alias{createBarcode}
\title{Create labels with barcode}
\usage{
createBarcode(
  designs,
  measure_date,
  site_idx,
  measure_traits,
  sample_num = 1,
  measure_traits_map = NULL
)
}
\arguments{
\item{designs}{A data.frame of design file, which has to include several columns}

\item{measure_date}{Date of measurement}

\item{site_idx}{Index of site}

\item{measure_traits}{trait list}

\item{sample_num}{Number of sample in each plot}

\item{measure_traits_map}{a trait map}
}
\description{
Create labels with barcode
}
