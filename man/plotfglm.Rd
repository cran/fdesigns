\name{plot.fglm}

\alias{plot.fglm}

\title{Plot of \code{fglm} Objects}

\usage{
\method{plot}{fglm}(x, pf = NULL, \dots)
}

\arguments{
\item{x}{An object of class \code{"fglm"}.}
\item{pf}{An integer specifying which profile factor to plot. Must be between \code{1} and \code{x$npf}. If \code{NULL} (default), the function will prompt the user interactively.}
\item{...}{Additional arguments to be passed to other methods.}
}

\details{
The function plots the design functions corresponding to the selected profile factor. If \code{pf = NULL}, 
the user is asked interactively which profile factor to plot.
The value needs to be between 1 and the argument npf from the function \link[fdesigns]{pfglm}. 
}

\value{
Plots are produced on the active graphics device. No object is returned.

Specifically, \code{n.runs} plots are drawn, showing the optimal functions 
of the selected profile factor. The x-axis represents time, the y-axis 
represents the values of the profile factor function, and the plot title 
indicates the run number corresponding to each curve.
}

\description{
Plot of \code{"fglm"} objects. For the choice of a profile factor, the optimal functions are plotted.
}

\note{
For examples see \link[fdesigns]{pfglm}.
}

\author{
Damianos Michaelides <\email{dm3g15@soton.ac.uk}>, Antony Overstall, Dave Woods
}




