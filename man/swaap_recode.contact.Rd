% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/R03-Recode_variables.R
\name{swaap_recode.contact}
\alias{swaap_recode.contact}
\title{Recode Contact Information Items}
\usage{
swaap_recode.contact(dtf_data)
}
\arguments{
\item{dtf_data}{A data frame, assumed to
follow the standardized format for the
school-wide assessment data.}
}
\value{
A data frame with the additional variables
\code{'SBJ.CHR.Contact.DateOfBirth'},
\code{'SBJ.CHR.Contact.GuardianName'},
\code{'SBJ.CHR.Contact.GuardianRelation'}, and
\code{'SBJ.CHR.Contact.GuardianPhone'}.
}
\description{
Renames contact information items for easy selection.
}
\author{
Kevin Potter
}
