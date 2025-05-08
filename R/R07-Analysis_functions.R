# Analysis functions
# Written by Kevin Potter
# email: kpotter5@mgh.harvard.edu
# Please email me directly if you
# have any questions or comments
# Last updated 2025-05-08




# swaap_analysis.schools <- function(
#     dtf_data ) {
#
#   dtf_school <- dtf_data |>
#     dplyr::group_by(
#       SSS.CHR.DataSet,
#       SSS.INT.SchoolCode,
#       SSS.INT.Grade
#     ) |>
#     dplyr::summarise(
#       SSS.INT.SchoolEnrollment =
#         unique( SSS.INT.SchoolEnrollment )
#     )
#
#   return( dtf_school )
# }
