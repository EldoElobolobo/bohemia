% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_fake_locations.R
\name{generate_fake_locations}
\alias{generate_fake_locations}
\title{Generate fake locations}
\usage{
generate_fake_locations(n = 1000, n_clusters = 10, sd = 0.02)
}
\arguments{
\item{n}{The number of points}

\item{n_clusters}{The number of clusters. If 0, points will be completely random; if greater than 0, points will be distributed in clusters (ideal for simulating villages, for example)}

\item{sd}{The standard deviation of the distance from cluster centroid to each point. Note that clusters are distributed in a 0-1 (x and y) space.}
}
\value{
A \code{tibble} of \code{n} points with x and y values
}
\description{
Generate some random x y locations for testing spatial methods
}
