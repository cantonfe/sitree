\name{fl}
\alias{fl}
\docType{data}
\title{Plot Data}
\description{
Plot data
}
\usage{data(fl)}
\format{
	The list contains the following elements
	\describe{
		\item{plot.id}{unique ID for each stand that corresponds to plot.id
			in the \emph{tr} dataset}
		\item{SI.m}{Site index in m}
		\item{SI.spp}{Species code for which SI.m is measured}
		\item{prop.plot}{Proportion of the plot covered by this stand
			(0-full plot, 2-20\%, 3-30\%, 4-40\%, ..., 8-80\%)}
		\item{ha2plot}{multiplier to convert from "per ha" to "per plot",
			e.g. basal area per ha to basal area per plot}
		\item{tree2ha}{multiplier to convert numbers "per tree" to "per ha",
			e.g. basal area per tree to basal area per ha}
		\item{soil.depth. 1234}{soil depth 1(shallow)-4(deep)}
		\item{land.use}{land use classification}
		\item{land.type}{land type classification}
		\item{veg.type}{vegetation type}
		\item{subplot.size.m2}{size of the subplot correponding to the stand
			in square meters}
		\item{plot.size.m2}{Plot size in square meters}
		\item{kom}{Municipality code}
		\item{region}{region where the plot lies}
		\item{skidding.distance.100m}{skidding distance in 100m}
		\item{slope.per}{slope in percentatge}
		\item{lat.det}{latitude in degrees}
		\item{alt.m}{altitude in meters}
		\item{utm.s33.ov.m}{UTM E-W coordinates of the plot}
		\item{utm.s33.snv.m}{UTM S-N coordinates of the plot}
		\item{stand.age.years}{data frame with stand age in years, only the
			first column, t0, is filled up}
		\item{management}{data frame will management codes for each period}
		}
	}

\source{
	This dataset is derived from the Norwegian National Forest Inventory
}
\examples{
data(fl)
str(fl) 
}
\keyword{datasets}
