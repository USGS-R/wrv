\name{gage.disch}

\alias{gage.disch}

\docType{data}

\title{Daily Mean Discharge at Streamgages}

\description{
The daily mean discharge at streamgages in the Big Wood River Valley, Idaho.
Discharge records bracket the 1992-2014 time period and are based on records with quality assurance code of approved (\sQuote{A}).
}

\usage{gage.disch}

\format{
A \code{data.frame} object with 8,315 records and the following variables:
\describe{
  \item{Date}{is the date during which discharge was averaged.}
  \item{13135500}{is the daily mean discharge in cubic meters per day, recorded at the USGS \href{http://waterdata.usgs.gov/id/nwis/uv/?site_no=13135500}{13135500} Big Wood River near Ketchum streamgage.}
  \item{13139510}{is the daily mean discharge in cubic meters per day, recorded at the USGS \href{http://waterdata.usgs.gov/id/nwis/uv/?site_no=13139510}{13139510} Big Wood River at Hailey streamgage.}
  \item{13140800}{is the daily mean discharge in cubic meters per day, recorded at the USGS \href{http://waterdata.usgs.gov/id/nwis/uv/?site_no=13140800}{13140800} Big Wood River at Stanton Crossing near Bellevue streamgage.}
}
}

\source{
National Water Information System (\href{http://waterdata.usgs.gov/nwis}{NWIS}), accessed on January 8, 2015
}

\examples{
str(gage.disch)

col <- c("red", "blue", "green")
ylab <- paste("Discharge in cubic", c("meters per day", "acre-foot per year"))
PlotGraph(gage.disch, ylab = ylab, col = col, lty = 1:3, conversion.factor = 0.29611)
leg <- sprintf("USGS \%s", names(gage.disch)[-1])
legend("topright", leg, col = col, lty = 1:3, inset = 0.02, cex = 0.7,
       box.lty = 1, bg = "#FFFFFFE7")

graphics.off()
}

\keyword{datasets}
