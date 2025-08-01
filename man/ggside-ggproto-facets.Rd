% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-side-facet.R, R/side-facet_.R,
%   R/ggside-ggproto.r
\name{check_scales_collapse}
\alias{check_scales_collapse}
\alias{sidePanelLayout}
\alias{ggside_facet}
\alias{ggside-ggproto-facets}
\title{Extending base ggproto classes for ggside}
\usage{
check_scales_collapse(data, params)

sidePanelLayout(layout, ggside)

ggside_facet(facet, ggside)
}
\arguments{
\item{data}{data passed through ggproto object}

\item{params}{parameters passed through ggproto object}

\item{layout}{layout computed by inherited ggproto Facet compute_layout method}

\item{ggside}{ggside object to update}

\item{facet}{Facet ggproto Object to replace}
}
\value{
ggproto object that can be added to a ggplot object
}
\description{
\code{check_scales_collapse} is a helper function that
is meant to be called after the inherited Facet's
compute_layout method

\code{sidePanelLayout} is a helper function that
is meant to be called after the inherited Facet's
compute_layout method and after \code{check_scales_collapse}

S3 class that converts old Facet into one that
is compatible with ggside. Can also update
ggside on the object. Typically, the new ggproto
will inherit from the object being replaced.
}
\section{Extended Facets}{


The following is a list \link[ggplot2]{ggplot2} facets that are
available to use by ggside base.

\itemize{
\item \link[ggplot2]{FacetNull} -> FacetSideNull
\item \link[ggplot2]{FacetGrid} -> FacetSideGrid
\item \link[ggplot2]{FacetWrap} -> FacetSideWrap
}
}

