\name{rcategorical}
\alias{rcategorical}

\title{
Generate Random Draws from a Discrete Set of Labels with Associated Probabilities
}
\description{
This is basically only a wrapper for the \code{sample} function, to make the code more consistent. It uses weighted sampling with replacement.
}
\usage{
rcategorical(n, labels, probs)
}
\arguments{
  \item{n}{
How many draws to make. Passed to the \code{size} argument of the \code{sample} function.
  }
  \item{labels}{
A vector of labels to draw from. Passed to the \code{x} argument of the \code{sample} function.
  }
  \item{probs}{
A numeric vector of probabilities, must sum to one. Passed to the \code{probs} argument of the \code{sample} function.
  }
}
\examples{
\dontrun{
rcategorical(n=5, labels=c("A", "B", "C"), probs=c(0.1, 0.2, 0.7))
}
}
