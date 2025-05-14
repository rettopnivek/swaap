# Code to prepare internal data sets
# Written by Kevin Potter
# email: kpotter5@mgh.harvard.edu
# Please email me directly if you
# have any questions or comments
# Last updated 2025-04-11

# Table of contents
# 1) swaap_internal.linking_items_marginal_rates
#   1.1) Include missing values
#     1.1.1) Kindergarten year
#     1.1.2) Sex at birth
#     1.1.3) Year and month of birth
#     1.1.4) Middle initial
#     1.1.5) Eye color
#     1.1.6) Older siblings
#     1.1.7) Street name
#   1.2) Exclude missing values
#     1.2.1) Kindergarten year
#     1.2.2) Sex at birth
#     1.2.3) Year and month of birth
#     1.2.4) Middle initial
#     1.2.5) Eye color
#     1.2.6) Older siblings
#     1.2.7) Street name
# ?) Development code

#### 1) swaap_internal.linking_items_marginal_rates ####

if ( FALSE ) {

  # Load in 2023 Fall data
  chr_wd <- getwd()
  chr_path <- paste0(
    "E:/CAM/CAM_R_projects/CAM_analyses/Dropbox/SWA-2015/",
    "SWA-Collaboration/Meghan_Costello/",
    "SWA-Preliminary_analysis_2025_05"
  )
  setwd( chr_path )
  targets::tar_load( dtf_SRV.Y23F )
  setwd( chr_wd )

  # Prep data
  dtf_data <- swaap::swaap_data.merge(
    list( Y23F = dtf_SRV.Y23F ),
    int_grades = 6:12
  )

  chr_LQ <- swaap::swaap_select.linking()[-(1:2)]
  chr_LQ_orig <- swaap::swaap_select.linking( lgc_original = T)[-(1:2)]

  swaap_internal.linking_items_marginal_rates <- lapply(
    seq_along(chr_LQ), function(i) {

      print( chr_LQ[i] )

      dtf_freq <- dtf_data |>
        dplyr::group_by_at(
          c( 'SSS.INT.Grade',
             chr_LQ[i] )
        ) |>
        dplyr::summarise(
          Freq = length( IDN.CHR.Record.ID ),
          .groups = 'drop'
        ) |>
        data.frame()
      colnames(dtf_freq) <- c(
        'Grade',
        'Values',
        'Freq'
      )

      return(dtf_freq)
    }
  )
  names(swaap_internal.linking_items_marginal_rates) <- chr_LQ_orig

  usethis::use_data(
    swaap_internal.linking_items_marginal_rates,
    overwrite = TRUE,
    internal = TRUE
  )

  # Close 'Function to help create responses'
}

#### 2) swaap_internal.linking_item_discrepancies ####

swaap_internal.linking_item_discrepancies <- list(
  marginal_rates = data.frame(
    Item = c(
      'SBJ.INT.Link.SchoolCode',
      'SBJ.INT.Link.SchoolID',
      'SBJ.INT.Link.KindergartenYearEst',
      'SBJ.CHR.Link.Sex',
      'SBJ.CHR.Link.DateOfBirth',
      'SBJ.CHR.Link.MiddleInitial',
      'SBJ.CHR.Link.EyeColor',
      'SBJ.CHR.Link.OlderSiblings',
      'SBJ.CHR.Link.Streetname'
    ),
    N = c(
      337,
      121,
      336,
      334,
      329,
      326,
      335,
      322,
      326
    ),
    Matched = c(
      336,
      108,
      325,
      329,
      318,
      295,
      275,
      265,
      259
    )
  )
)

#### ?) Development code ####



