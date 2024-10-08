% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quantjtbd-package.R
\name{get_jtbd_scores.comparison}
\alias{get_jtbd_scores.comparison}
\title{Calculate JTBD scores for comparison}
\usage{
get_jtbd_scores.comparison(data_frame, segmentation_column)
}
\arguments{
\item{data_frame}{The data frame containing JTBD data}

\item{segmentation_column}{The name of the column used for segmentation}
}
\value{
An updated master table with JTBD scores for each segment
}
\description{
This function combines the functionality of get_jtbd_var_values.list and get_jtbd_scores
to simplify the process of calculating JTBD scores for comparison.
}
