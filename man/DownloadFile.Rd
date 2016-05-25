\name{DownloadFile}

\alias{DownloadFile}

\title{Download File from the Internet}

\description{
This function downloads a file from the Internet.
}

\usage{
DownloadFile(url, dest.dir = tempdir(), mode = NULL, extract = TRUE,
             max.attempts = 10L, wait.time = 30)
}

\arguments{
\item{url}{\code{character}; the URL (or FTP) of a resource to be downloaded.}
\item{dest.dir}{\code{character}; the directory where the downloaded file is saved.}
\item{mode}{\code{character}; the mode with which to write the file, such as \code{"w"}, \code{"wb"} (binary), \code{"a"} (append) and \code{"ab"}.}
\item{extract}{\code{logical}; if \code{TRUE}, an attempt is made to extract files from the file archive.}
\item{max.attempts}{\code{integer}; the maximum number of attempts to download a file.}
\item{wait.time}{\code{numeric}; the time to wait between download attempts, in seconds.}
}

\details{
This function requires package \pkg{RCurl}.
}

\value{
Returns the file path(s) to the downloaded file (or uncompressed files).
}

\author{J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center}

\seealso{
\code{CFILE}, \code{curlPerform}
}

\examples{\dontrun{
url <- paste0("https://raw.githubusercontent.com/jfisher-usgs/",
              "wrv/master/inst/extdata/alluvium.extent.zip")
files <- DownloadFile(url)
unlink(files)}
}

\keyword{IO}
