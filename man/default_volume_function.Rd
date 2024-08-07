% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/default_volume_function.R
\name{default_volume_function}
\alias{default_volume_function}
\title{Default volume function}
\usage{
default_volume_function(x, SpParams = NULL)
}
\arguments{
\item{x}{A data frame with columns 'DBH', 'Height' and 'N' or a \code{\link[medfate]{forest}} object}

\item{SpParams}{A data frame with species parameters (not used in the default function but will be called)}
}
\value{
A function amenable for wood volume estimation.
}
\description{
Example function for estimating wood volume (in m3/ha) from a tree table or forest object.
}
\details{
Users should define their own functions taking into account that:
\itemize{
\item{Input should be named 'x' and consist of a tree table with tree records as rows
and columns 'DBH' (cm), 'Height' (cm), and 'N' (ind./ha).}
\item{Output should be a numeric vector of length equal to the number of tree records in 'x'}
}
}
