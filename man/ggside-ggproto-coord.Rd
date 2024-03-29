% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/side-coord-cartesian.R
\name{ggside_coord}
\alias{ggside_coord}
\alias{ggside_coord.default}
\alias{ggside_coord.CoordCartesian}
\alias{ggside_coord.CoordSide}
\alias{ggside_coord.CoordTrans}
\alias{ggside_coord.CoordFixed}
\title{Coord Compatible with ggside}
\usage{
ggside_coord(coord)

\method{ggside_coord}{default}(coord)

\method{ggside_coord}{CoordCartesian}(coord)

\method{ggside_coord}{CoordSide}(coord)

\method{ggside_coord}{CoordTrans}(coord)

\method{ggside_coord}{CoordFixed}(coord)
}
\arguments{
\item{coord}{coord ggproto Object to replace}
}
\description{
S3 class that converts old Coord into one that
is compatible with ggside. Can also update
ggside on the object. Typically, the new ggproto
will inherit from the object being replaced.
}
