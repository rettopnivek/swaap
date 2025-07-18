% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/R03-Recode_variables.R
\name{swaap_recode.misc}
\alias{swaap_recode.misc}
\title{Recode Miscellaneous Items}
\usage{
swaap_recode.misc(dtf_data)
}
\arguments{
\item{dtf_data}{A data frame, assumed to
follow the standardized format for the
school-wide assessment data.}
}
\value{
A data frame with the additional variables
\code{'SBJ.CHR.PrescribedMedicationHealth'},
\code{'SBJ.LGC.CloseConnection.<Type>'}, and
\code{'SBJ.LGC.SoughtHelp.<Type>'}.
}
\description{
Renames miscellaneous items.
}
\author{
Kevin Potter
}
