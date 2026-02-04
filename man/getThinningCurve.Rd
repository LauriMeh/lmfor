\name{getThinningCurve}
\alias{getThinningCurve}
\title{A function that predicts a thinning curve for a domain of interest using 
  a fitted growth model (cumulative) and volume model.}
\description{
A function to derive thinning levels (m3/ha/year) by age class. The thinning levels 
are derived based on the cumulative net growth curve and volume curve 
for the domain of interest.
}

\usage{
getThinningCurve(	
		tmax_class = 300,
		region = NULL,
		fert = NULL,
		L2 = NULL,
		NFI = NULL,
		netgrowthmod = growthNorway$Net,
		volmodel = volNorway,
		retrees = TRUE,
		domain_stra = list(
			region = NULL,
			fert = NULL,
			L2 = NULL,
			NFI = NULL),
		volf = NULL,
		growf = NULL
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tmax_class}{ 
	Maximum age class.
}
  \item{region}{
	Region of interest.
}
  \item{fert}{
	Fertility class.
}
  \item{L2}{
	Forest type definition, for example L1 forest type.
}
  \item{NFI}{
	NFI campaign.
}
  \item{netgrowthmod}{ 
	Net growth model object.
}
  \item{volmodel}{
	Volume model object.
}
  \item{retrees}{
	Retention trees. Logical.
}
  \item{domain_stra}{
	A list defining all possible stratifications.
}
  \item{volf}{
	User-defined volume function for the prediction phase. 
	This must be equivalent to the function used in the model fitting. Default is NULL.
}
  \item{growf}{
	User-defined growth function for the prediction phase.
	This must be equivalent to the function used in the model fitting. Default is NULL.
}
}
\details{

}
\value{

}
\references{
}
\author{
Lauri Mehtatalo <\email{lauri.mehtatalo@luke.fi}>
}

\examples{

}

\keyword{ graphs} 
