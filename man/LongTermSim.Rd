\name{LongTermSim}
\alias{LongTermSim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Age class simulator
}
\description{
A simulator that projects development of forest resources based on a age distribution and a growth curve. 
}
\usage{
LongTermSim(age = NULL, 
            area = NULL, 
            growth = NULL, 
            growthG = NA, 
            protectionLimit = 120, #thinning=0.005,
            cutCriterion = NULL, 
            target = NULL, 
            simLength = 2, 
            startyear = 2020,
            productSink = 0.05, 
            thinningRule = c(0.019, 100), 
            cutSuo = FALSE, 
            mortality = 0.01,
            phi = 1, 
            above = 0, 
            shift = 30, 
            areasuo = NA)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{age}{
    A vector of age classes. The current implementation uses a class width of 1 year.
  }

  \item{area}{
    A vector of areas covered by each age class. The areas must be given in hectares.
  }

  \item{growth}{
    A vector of net growths per year by age class. 
    The growth curve is typically estimated using permanent national forest inventory plots.
  }

  \item{growthG}{
    A vector of gross growths per year by age class. 
    The growth curve is typically estimated using permanent national forest inventory plots. 
    Set NA (default) if gross growths are not of interest.
  }
  
  \item{protectionLimit}{
    A scalar value (years) that determines the oldest class that can be clear-cut.                          
  }
  

  \item{CutCriterion}{
    A string argument determining the criterion for cuttings. The possible alternatives are:
    "RotationLength", "ClearcutArea", "HarvestVolume", "ProportionOfGrowth" and "AbsoluteSink".
  }

  \item{target}{
    A target value for CutCriterion. For example, HarvestVolume requires 
    target cutting level in Mm3 per year. Note that thinnings will always 
    be carried out regardless of the selected cutting target. 
    A vector with the length determined by simLength or scalar.
    If cuttingcriterion is harvestvolume, thinnings will always be carried out according to thinninRule.
   
  }

    \item{simLength}{
      The length of the simulation in years.
  }

    \item{startyear}{
      The start year of the simulation.
  }

    \item{productSink}{
      TO BE ADDED
  }
    \item{thinningRule}{
      Thinning rule by age class. The input vector has same length as length(age) 
      or alternatively a vector of two elements can be used. In case of two elemements,
      the first indicates the proportion of harvested volume per age class and 
      the second element refers to the max. age 
      at which thinnings can be carried out, e.g. c(0.019, 100)
  }
    \item{cutSuo}{

  }
    \item{mortality}{
      Mortality rate in terms of annual growth per age class.
  }
    \item{phi}{
      The rate parameter associated with the exponential distributions used for 
      the scheduling of final fellings. Default 1. See more details ?dexp.
  }

    \item{above}{
      The proportion of final fellings carried out as thinnings from above. Default 0.
  }
    \item{shift}{
      How many years do forests get younger after a thinning from above? Default 30 years.
      This parameter is used when above > 0. Integer.
  }

    \item{areasuo}{
      The Age distribution of protected forests.
  }
}
\details{

}

\value{
  A list of components (TO BE MODIFIED!)
  \item{ikajakTalous}{}
  \item{netgrowths}{}
  \item{grossgrowths}{}
  \item{hakkuumaara}{}
  \item{nielut}{}
  \item{nielutCum}{}
  \item{growth}{}
  \item{growthCurve}{}
  \item{removals}{}
  \item{volume}{}
  \item{volumeSuo}{}
  \item{kasvuTal}{}
  \item{kasvuSuo}{}
  \item{mortSuo}{}
  \item{varTal}{}
  \item{varSuo}{}
  \item{nieTal}{}
  \item{nieSuo}{}
  \item{overcut}{}
}

\references{
Mehtätalo, L., Räty M., and Mehtätalo, J. 2025. A new growth curve and fit to the National Forest Inventory data of Finland. Ecological modelling, 501: 111006 , \doi{10.1016/j.ecolmodel.2024.111006}
}

\author{
Lauri Mehtatalo <\email{lauri.mehtatalo@luke.fi}>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
# Load required datasets
 
# Fit a growthcurve

# Get growth curve for a domain of interest
netgrowth <- c(0, getGrowthCurve(max(ika), 
                                  maakunta = "North Karelia",
                                  kasvupaikka = "Medium",
                                  suo = "Mineral soil", 
                                  type = "Net"))
# Simulate                                  
sim_out <- LongTermSim(age = ika,
                        area = ala, 
                        growth = netgrowth, 
                        growthG = NA,
                        protectionLimit = max(ika),
                        cutCriterion = "HarvestVolume",
                        target = 3.24, # Cuttings Mm3/year
                        simLength = 500,
                        startyear = 2021,
                        productSink = 0.05)
}

\keyword{forest biometrics}
