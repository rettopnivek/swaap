% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/R03-Recode_variables.R
\name{swaap_recode.quality}
\alias{swaap_recode.quality}
\title{Recode Quality Control Items}
\usage{
swaap_recode.quality(dtf_data)
}
\arguments{
\item{dtf_data}{A data frame, assumed to
follow the standardized format for the
school-wide assessment data.}
}
\value{
A data frame with the additional variables
\code{'QLT.DBL.ProportionCompleted.Total'},
\code{'QLT.LGC.AttentionChecks.MetAll'}, and
\code{'QLT.LGC.AttentionChecks.MetAtLeastOne'}.
}
\description{
Renames quality control items for easy selection.
}
\author{
Kevin Potter
}
