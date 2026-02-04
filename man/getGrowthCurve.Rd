\name{getGrowthCurve}
\alias{getGrowthCurve}
\title{A function that predicts a growth curve for a domain of interest using 
  a fitted growth model.}
\description{
A function for predicting growths (m3/ha/year) by age class.
\enumerate{
\item 95\% confidence intervals of means or 
\item 95\% confidence intervals for individual observations 
}in the classes of the 
variable on the x-axis. Plot of the first type is useful for analyzing the fit of the assumed fixed part and plots of 
type b can be used to analyze the homogeneity of residuals. 
}

\usage{
getGrowthCurve(	
  tmax = 300,
	region = "Central-South",
	fert = "average",
	L2 = "Alpine coniferous",
	NFI = "NNFI-1418/NNFI-1923",
	type = c("Net", "Gross")[1], 
	mods = NULL,
	curvetype = "cur",
	regions = c("Central-South","East","Finnmark","North","West"),    
	ferts = c("average","good","poor"),
	L2s= c("Alpine coniferous","boreal","Broadleaved forest",                                             
			"Hemiboreal, nemoral coniferous and mixed broadleaved-coniferous",
			"Mire and swamp forest", "Plantations and self-sown exotic forest"),
	NFIs= c("NNFI-1115/NNFI-1418","NNFI-1418/NNFI-1923"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tmax}{ 
}
  \item{region}{
}
  \item{fert}{

}
  \item{L2}{

}
  \item{NFI}{

}
  \item{type}{
}
  \item{mods}{ Growth models.
}
  \item{curvetype}{
}
  \item{regions}{
}
  \item{ferts}{
}
  \item{L2s}{
}
  \item{NFIs}{
}
}
\details{

}
\value{

}
\references{
Mehtatalo, Lauri and Lappi, Juha 2020. Biometry for Forestry and Environmental Data: with examples in R. 
New York: Chapman and Hall/CRC. 426 p. \doi{10.1201/9780429173462}
}
\author{
Lauri Mehtatalo <\email{lauri.mehtatalo@uef.fi}>
}

\examples{

}

\keyword{ graphs} 
