% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{gage.disch}
\alias{gage.disch}
\title{Daily Mean Discharge at Streamgages}
\format{
An object of data.frame class with 8,315 records and the following variables:
  \describe{
    \item{Date}{date during which discharge was averaged.}
    \item{13135500}{daily mean discharge in cubic meters per day, recorded at the USGS
      \href{https://waterdata.usgs.gov/id/nwis/uv/?site_no=13135500}{13135500}
      Big Wood River near Ketchum streamgage.}
    \item{13139510}{daily mean discharge in cubic meters per day, recorded at the USGS
      \href{https://waterdata.usgs.gov/id/nwis/uv/?site_no=13139510}{13139510}
      Big Wood River at Hailey streamgage.}
    \item{13140800}{daily mean discharge in cubic meters per day, recorded at the USGS
      \href{https://waterdata.usgs.gov/id/nwis/uv/?site_no=13140800}{13140800}
      Big Wood River at Stanton Crossing near Bellevue streamgage.}
  }
}
\source{
National Water Information System (\href{https://waterdata.usgs.gov/nwis}{NWIS}),
  accessed on January 8, 2015
}
\usage{
gage.disch
}
\description{
The daily mean discharge at streamgages in the Big Wood River Valley, Idaho.
Discharge records bracket the 1992-2014 time period and are based on
records with quality assurance code of approved (\sQuote{A}).
}
\examples{
str(gage.disch)

col <- c("red", "blue", "green")
ylab <- paste("Discharge in cubic", c("meters per day", "acre-foot per year"))
inlmisc::PlotGraph(gage.disch, ylab = ylab, col = col, lty = 1:3,
                   conversion.factor = 0.29611, seq.date.by = "year")
leg <- sprintf("USGS \%s", names(gage.disch)[-1])
legend("topright", leg, col = col, lty = 1:3, inset = 0.02, cex = 0.7,
       box.lty = 1, bg = "#FFFFFFE7")

graphics.off()

}
\keyword{datasets}
