\name{ReplaceInTemplate}

\alias{ReplaceInTemplate}

\title{Replace Values in a Template Text}

\description{
This function replaces keys within special markups in a template text with specified values.
Pieces of \R code can be put into the markups of the template text, and are evaluated during the replacement.
}

\usage{ReplaceInTemplate(text, replacement)}

\arguments{
\item{text}{\code{character}; a vector of character strings, the template text.}
\item{replacement}{\code{list}; the values to replace in \code{text}.}
}

\details{
Keys are enclosed into markups of the form \code{$(KEY)} and \code{@\{CODE\}}.
}

\value{
A vector of character strings after key replacement.
}

\author{J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center}

\references{
This code was derived from the \href{http://cran.r-project.org/web/packages/sensitivity/}{sensitivity}\code{::template.replace} function.
}

\examples{
text <- c("Hello $(name)!", "$(a) + $(b) = @{$(a) + $(b)}",
          "pi = @{format(pi, digits = 5)}")
replacement <- list(name = "world", a = 1, b = 2)
cat(ReplaceInTemplate(text, replacement), sep = "\n")
}

\keyword{IO}
