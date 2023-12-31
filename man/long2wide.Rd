% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/long2wide.R
\name{long2wide}
\alias{long2wide}
\title{Reshapes the data}
\usage{
long2wide(data, ID = "ID", separator = "_", hemisphere = "prefix", start, end)
}
\arguments{
\item{data}{The long format data.}

\item{ID}{The column of identifiers.}

\item{separator}{A character vector that separates characters in the variable names.}

\item{hemisphere}{The character vector that indicates whether a hemisphere indicator in the variable names is the prefix or suffix. At this point, only a "prefix" option is available.}

\item{start}{The column that specifies the starting point of a set of variables to be reshaped.}

\item{end}{The column that specifies the endpoint of a set of variables to be reshaped.}
}
\value{
The wide format data
}
\description{
long2wide() is data-reshaping function for long format data.
This function mainly targets the brain structure data that contains the data from the left and right hemispheres
}
\examples{

data(long)

long2wide(
data=long,
ID="ID",
separator="_",
hemisphere="prefix",
start="region",
end="rh")
}
