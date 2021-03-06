% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/meanIntensityImage.R
\name{meanIntensityImage}
\alias{meanIntensityImage}
\alias{meanintensityImage}
\alias{meanIntensityimage}
\alias{meanintensityimage}
\title{meanIntensityImage}
\usage{
meanIntensityImage(image = NULL, cilium_color = NULL)
}
\arguments{
\item{image}{An three-dimensional array of numbers between 0 and 1}

\item{cilium_color}{A character (color of the staining of the cilia)}
}
\description{
Get tmean intensity of cilia
}
\details{
By using an x-y-3(rgb)-representation of an image, you can
extract the layer of the cilia and calculate the mean intensity
disregarding all pixels that are black.
}
\author{
Kai Budde
}
