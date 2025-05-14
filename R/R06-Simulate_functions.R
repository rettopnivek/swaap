# Simulation functions
# Written by Kevin Potter
# email: kpotter5@mgh.harvard.edu
# Please email me directly if you
# have any questions or comments
# Last updated 2025-05-14

# Table of contents
# 1) swaap_simulate
# L) Link
#   L.D) swaap_simulate.link.debug
#     L.D.1) Initialize data
#     L.D.2) Standard linking + no links
#     L.D.3) Dissimilarity = 1 [Base]
#     L.D.4) Dissimilarity = 1 [Add]
#     L.D.5) Duplicate records [Base]
#     L.D.6) Duplicate records [Add]
#     L.D.7) Subset dissimilarity = 0
#     L.D.8) Dissimilarity off by 1
#     L.D.9) Duplicate records w/ NA [Base]
#     L.D.10) Duplicate records w/ NA [Add]
#     L.D.11) Test of priority [School ID over questions]
#     L.D.12) Link using different items by time point
#     L.D.13) Special cases for duplicates
#     L.D._) Final processing
#   L.D) swaap_simulate.link.demo
#     L.D.1) Initialize data
#     L.D.2) Standard linking + no links
#   L.D) swaap_simulate.link.duplicates
#     L.D.1) Initialize data
#     L.D.2) Unique
#     L.D.3) Duplicates
#     L.D.4) Duplicate SID + unique LQ
#     L.D.5) Unique SID + duplicate LQ
#     L.D.6) LQ off by 1
#   L.R) swaap_simulate.link.realistic
#   L.S) swaap_simulate.link.setup
# S) SBIRT
#   S.D) swaap_simulate.SBIRT.design

#### 1) swaap_simulate ####
#' Simulate data
#'
#' Function to simulate assorted data relevant to
#' the school-wide assessment project.
#'
#' @param chr_type A character string, the type
#'   of data to simulate.
#' @param chr_subtype A character string, the sub-type
#'   of data to simulate.
#' @param ... Additional arguments to pass to the
#'   internal data generation functions.
#'
#' @author Kevin Potter
#'
#' @returns A data frame.
#'
#' @examples
#' dtf_debug <- swaap_simulate( 'link' )
#'
#' @export

swaap_simulate <- function(
    chr_type,
    chr_subtype = '',
    ... ) {

  lst_types <- list(
    link = c(
      'link',
      'Link',
      'linking',
      'Linking',
      'linking code',
      'Linking code'
    )
  )

  lst_subtypes <- list(
    link = list(
      debug = c(
        'debug',
        'Debug'
      ),
      demo = c(
        'demo',
        'Demo',
        'demonstration',
        'Demonstration'
      ),
      duplicates = c(
        'duplicates',
        'Duplicates',
        'duplicate',
        'Duplicate',
        'dup',
        'Dup'
      )
    )
  )

  # Data generation [Linking code]
  if ( chr_type %in% lst_types$link ) {

    # Default subtype
    if ( chr_subtype == '' ) chr_subtype <- 'debug'

    # Data for debugging
    if ( chr_subtype %in% lst_subtypes$link$debug ) {

      return(
        swaap_simulate.link.debug(
          ...
        )
      )

      # Close 'Data for debugging'
    }

    # Data for demonstration purposes
    if ( chr_subtype %in% lst_subtypes$link$demo ) {

      return(
        swaap_simulate.link.demo(
          ...
        )
      )

      # Close 'Data for debugging'
    }

    # Data for duplicate records
    if ( chr_subtype %in% lst_subtypes$link$duplicates ) {

      return(
        swaap_simulate.link.duplicates(
          ...
        )
      )

      # Close 'Data for debugging'
    }

    # Close 'Data generation [Linking code]'
  }

  chr_error <- paste0(
    "Check argument 'chr_type' & 'chr_subtype'; options include:\n",
    "  chr_type = 'link'\n",
    "    chr_subtype = 'debug'\n",
    "    chr_subtype = 'demo'\n",
    "    chr_subtype = 'duplicate'\n"
  )

  stop(
    chr_error
  )

}

#### L) Link ####

#### L.D) swaap_simulate.link.debug ####

swaap_simulate.link.debug <- function(
    int_RNG_seed = 20250415 ) {

  #### L.D.1) Initialize data ####

  set.seed( int_RNG_seed )

  lst_setup <- swaap_simulate.link.setup()

  dtf_long <- lst_setup$design[
    rep( 1, 140 ),
  ]

  dtf_long$IDX.INT.Origin.LASID <- c(
    # Standard linking
    1, rep( NA, 7 ),
    1, rep( NA, 7 ),
    1, rep( NA, 7 ),
    # No links
    2, rep( NA, 7 ),
    3, rep( NA, 7 ),
    4, rep( NA, 7 ),
    # Specific tests
    # + Dissimilarity = 1 [Base]
    rep( NA, 3 ) |> rep(6),
    # + Dissimilarity = 1 [Add]
    rep( NA, 3 ) |> rep(6),
    # + Duplicate records [Base]
    rep( NA, 3 ),
    # + Duplicate records [Add]
    rep( NA, 3 ),
    # + Subset dissimilarity = 0
    rep( NA, 6 ) |> rep(2),
    # + Dissimilarity off by 1
    rep( NA, 6 ) |> rep(2),
    # + Duplicate records w/ NA [Base]
    rep( NA, 3 ),
    # + Duplicate records w/ NA [Add]
    rep( NA, 3 ),
    # + Test of priority [School ID over questions]
    c( 5, 5 ),
    # + Link using different items by time point
    c( 6, NA, 6 ),
    # + Special cases for duplicates
    #   Case 1
    rep( NA, 4 ),
    #   Case 2
    rep( NA, 4 ),
    #   Case 3
    rep( NA, 3 ),
    #   Case 4
    c( 7, 7, NA, NA )
  )
  dtf_long$SSS.INT.School.Code <- c(
    # Standard linking
    rep( 1, 8*3 ),
    # No links
    rep( 1, 8*3 ),
    # Specific tests
    # + Dissimilarity = 1 [Base]
    c(1, 1, 1) |> rep(6),
    # + Dissimilarity = 1 [Add]
    c(1, 1, 1) |> rep(6),
    # + Duplicate records [Base]
    rep( 1, 3 ),
    # + Duplicate records [Add]
    rep( 1, 3 ),
    # + Subset dissimilarity = 0
    rep( 1, 6 ) |> rep(2),
    # + Dissimilarity off by 1
    rep( 1, 6 ) |> rep(2),
    # + Duplicate records w/ NA [Base]
    rep( 1, 3 ),
    # + Duplicate records w/ NA [Add]
    rep( 1, 3 ),
    # + Test of priority [School ID over questions]
    c( 1, 1 ),
    # + Link using different items by time point
    c( 1, 1, 1 ),
    # + Special cases for duplicates
    #   Case 1
    rep( 1, 4 ),
    #   Case 2
    rep( 1, 4 ),
    #   Case 3
    rep( 1, 3 ),
    #   Case 4
    rep( 1, 4 )
  )
  dtf_long$SSS.INT.TimePoint <- c(
    # Standard linking
    rep( 0, 8 ),
    rep( 1, 8 ),
    rep( 2, 8 ),
    # No links
    rep( 0, 8 ),
    rep( 1, 8 ),
    rep( 2, 8 ),
    # Specific tests
    # + Dissimilarity = 1 [Base]
    c(0, 0, 1) |> rep(6),
    # + Dissimilarity = 1 [Add]
    c(0, 1, 1) |> rep(6),
    # + Duplicate records [Base]
    c(0, 0, 1),
    # + Duplicate records [Add]
    c(0, 1, 1),
    # + Subset dissimilarity = 0
    c( 0, 1 ) |> rep(6),
    # + Dissimilarity off by 1
    c( 0, 1 ) |> rep(6),
    # + Duplicate records w/ NA [Base]
    c(0, 0, 1),
    # + Duplicate records w/ NA [Add]
    c(0, 1, 1),
    # + Test of priority [School ID over questions]
    c( 0, 1 ),
    # + Link using different items by time point
    c( 0, 1, 2 ),
    # + Special cases for duplicates
    #   Case 1
    c( 0, 1, 2, 2 ),
    #   Case 2
    c( 0, 0, 1, 2 ),
    #   Case 3
    c( 0, 2, 2 ),
    #   Case 4
    c( 0, 1, 2, 2 )
  )
  dtf_long$LNK.LGC.True.Linkable <- c(
    # Standard linking
    rep( TRUE, 8*3 ),
    # No links
    rep( FALSE, 8*3 ),
    # Specific tests
    # + Dissimilarity = 1 [Base]
    c(TRUE, FALSE, TRUE) |> rep(6),
    # + Dissimilarity = 1 [Add]
    c(TRUE, FALSE, TRUE) |> rep(6),
    # + Duplicate records [Base]
    rep( TRUE, 3 ),
    # + Duplicate records [Add]
    rep( TRUE, 3 ),
    # + Subset dissimilarity = 0
    c( FALSE, FALSE ) |> rep(6),
    # + Off-by-one error
    c( TRUE, TRUE ) |> rep(6),
    # + Duplicate records w/ NA [Base]
    rep( TRUE, 3 ),
    # + Duplicate records w/ NA [Add]
    rep( TRUE, 3 ),
    # + Test of priority [School ID over questions]
    c( TRUE, TRUE ),
    # + Link using different items by time point
    rep( TRUE, 3 ),
    # + Special cases for duplicates
    #   Case 1
    rep( TRUE, 4 ),
    #   Case 2
    c( FALSE, TRUE, TRUE, TRUE ),
    #   Case 3
    rep( TRUE, 3 ),
    #   Case 4
    rep( TRUE, 4 )
  )

  # Update variables based on time point
  dtf_long$SSS.INT.SurveyYear <- c(
    2023, 2024, 2024
  )[dtf_long$SSS.INT.TimePoint + 1]
  dtf_long$SSS.INT.Grade <- c(
    9, 9, 10
  )[dtf_long$SSS.INT.TimePoint + 1]
  dtf_long$SSS.DTM.SurveyStart <- c(
    lst_setup$dates['Y23F'],
    lst_setup$dates['Y24S'],
    lst_setup$dates['Y24F']
  )[dtf_long$SSS.INT.TimePoint + 1]
  dtf_long$SSS.INT.LongitudinalWave <- 1

  dtf_long$IDX.CHR.Origin.ID <-
    paste0(
      'Fake', 1:nrow(dtf_long)
    )

  chr_linking_questions <- lst_setup$linking_questions$variables

  # Kindergarten year calculated from year and grade
  dtf_long$SBJ.INT.Link.KindergartenYearEst <-
    c( 2023, 2023, 2024 )[ # Use start of school year
      dtf_long$SSS.INT.TimePoint + 1
    ] - dtf_long$SSS.INT.Grade

  #### L.D.2) Standard linking + no links ####

  # Standard linking + no links
  for ( l in 1:6 ) {

    dtf_possible <- lst_setup$linking_questions$possible[[
      chr_linking_questions[l+1]
    ]]

    int_unique <- c(
      # Standard linking
      2:8,
      # No link
      (8*3 + 1):(8*6)
    )

    dtf_long[[ chr_linking_questions[l+1] ]][int_unique] <-
      swaap_simulate.link.sample(
        dtf_possible[[1]],
        int_size = length(int_unique),
        lgc_replace = TRUE,
        int_freq = NULL # dtf_possible[[2]]
    )

    # Copy cases that should be linked
    dtf_long[[ chr_linking_questions[l+1] ]][2:8 + 8] <-
      dtf_long[[ chr_linking_questions[l+1] ]][2:8]
    dtf_long[[ chr_linking_questions[l+1] ]][2:8 + 8*2] <-
      dtf_long[[ chr_linking_questions[l+1] ]][2:8]

    # Close 'Standard linking'
  }

  # Track actual links
  dtf_long$LNK.INT.True.ID[ 1:8 ] <- 1:8
  dtf_long$LNK.INT.True.ID[ 1:8 + 8 ] <- 1:8
  dtf_long$LNK.INT.True.ID[ 1:8 + 8*2 ] <- 1:8

  # Label test type
  dtf_long$LNK.CHR.True.TestType[ 1:(8*3) ] <-
    'Standard linking'
  dtf_long$LNK.CHR.True.TestType[ (8*3) + 1:(8*3) ] <-
    'Standard no link'

  # Update indices
  int_old <- 8*6
  int_ID_old <- 8

  #### L.D.3) Dissimilarity = 1 [Base] ####

  int_ID <- c(
    c( 1, 2, 1 ),
    c( 1, 2, 1 ) + 2,
    c( 1, 2, 1 ) + 2*2,
    c( 1, 2, 1 ) + 2*3,
    c( 1, 2, 1 ) + 2*4,
    c( 1, 2, 1 ) + 2*5
  )
  int_new <- seq_along(int_ID)
  lst_new <- lapply(
    1:6, function(l) {
      1:3 + 3*(l-1)
    }
  )

  # Loop over variables to differ
  for ( v in 1:6 ) {

    # Loop over linking items
    for (l in 1:6) {

      dtf_possible <- lst_setup$linking_questions$possible[[
        chr_linking_questions[l+1]
      ]]

      # Item to differ
      if ( l == v ) {

        dtf_long[[ chr_linking_questions[l+1] ]][
          int_old + lst_new[[v]]
        ] <- swaap_simulate.link.sample(
          dtf_possible[[1]],
          int_size = 2,
          lgc_replace = FALSE,
          int_freq = NULL # dtf_possible[[2]]
        )[c(1, 2, 1)]

        # Close 'Item to differ'
      } else {

        dtf_long[[ chr_linking_questions[l+1] ]][
          int_old + lst_new[[v]]
        ] <- swaap_simulate.link.sample(
          dtf_possible[[1]],
          int_size = 1,
          lgc_replace = FALSE,
          int_freq = NULL # dtf_possible[[2]]
        )

        # Close else for 'Item to differ'
      }

      # Close 'Loop over linking items'
    }

    # Close 'Loop over variables to differ'
  }

  # Update ID
  dtf_long$LNK.INT.True.ID[int_old + int_new] <-
    int_ID_old + int_ID

  # Label test type
  dtf_long$LNK.CHR.True.TestType[int_old + int_new] <-
    'Dissimilarity = 1 [Base]'

  # Update indices
  int_old <- int_old + max(int_new)
  int_ID_old <- int_ID_old + max(int_ID)

  #### L.D.4) Dissimilarity = 1 [Add] ####

  int_ID <- c(
    c( 1, 2, 1 ),
    c( 1, 2, 1 ) + 2,
    c( 1, 2, 1 ) + 2*2,
    c( 1, 2, 1 ) + 2*3,
    c( 1, 2, 1 ) + 2*4,
    c( 1, 2, 1 ) + 2*5
  )
  int_new <- seq_along(int_ID)
  lst_new <- lapply(
    1:6, function(l) {
      1:3 + 3*(l-1)
    }
  )

  # Loop over variables to differ
  for ( v in 1:6 ) {

    # Loop over linking items
    for (l in 1:6) {

      dtf_possible <- lst_setup$linking_questions$possible[[
        chr_linking_questions[l+1]
      ]]

      # Item to differ
      if ( l == v ) {

        dtf_long[[ chr_linking_questions[l+1] ]][
          int_old + lst_new[[v]]
        ] <- swaap_simulate.link.sample(
          dtf_possible[[1]],
          int_size = 2,
          lgc_replace = FALSE,
          int_freq = NULL # dtf_possible[[2]]
        )[c(1, 2, 1)]

        # Close 'Item to differ'
      } else {

        dtf_long[[ chr_linking_questions[l+1] ]][
          int_old + lst_new[[v]]
        ] <- swaap_simulate.link.sample(
          dtf_possible[[1]],
          int_size = 1,
          lgc_replace = FALSE,
          int_freq = NULL # dtf_possible[[2]]
        )

        # Close else for 'Item to differ'
      }

      # Close 'Loop over linking items'
    }

    # Close 'Loop over variables to differ'
  }

  # Update ID
  dtf_long$LNK.INT.True.ID[int_old + int_new] <-
    int_ID_old + int_ID

  # Label test type
  dtf_long$LNK.CHR.True.TestType[int_old + int_new] <-
    'Dissimilarity = 1 [Add]'

  # Update indices
  int_old <- int_old + max(int_new)
  int_ID_old <- int_ID_old + max(int_ID)

  #### L.D.5) Duplicate records [Base] ####

  int_ID <- 1
  int_new <- 1:3

  # Loop over linking items
  for (l in 1:6) {

    dtf_possible <- lst_setup$linking_questions$possible[[
      chr_linking_questions[l+1]
    ]]

    dtf_long[[ chr_linking_questions[l+1] ]][
      int_old + int_new
    ] <- swaap_simulate.link.sample(
      dtf_possible[[1]],
      int_size = 1,
      lgc_replace = FALSE,
      int_freq = NULL # dtf_possible[[2]]
    )

    # Close 'Loop over linking items'
  }

  # Update ID
  dtf_long$LNK.INT.True.ID[int_old + int_new] <-
    int_ID_old + int_ID

  # Label test type
  dtf_long$LNK.CHR.True.TestType[int_old + int_new] <-
    'Duplicate records [Base]'

  # Update indices
  int_old <- int_old + max(int_new)
  int_ID_old <- int_ID_old + max(int_ID)

  #### L.D.6) Duplicate records [Add] ####

  int_ID <- 1
  int_new <- 1:3

  # Loop over linking items
  for (l in 1:6) {

    dtf_possible <- lst_setup$linking_questions$possible[[
      chr_linking_questions[l+1]
    ]]

    dtf_long[[ chr_linking_questions[l+1] ]][
      int_old + int_new
    ] <- swaap_simulate.link.sample(
      dtf_possible[[1]],
      int_size = 1,
      lgc_replace = FALSE,
      int_freq = NULL # dtf_possible[[2]]
    )

    # Close 'Loop over linking items'
  }

  # Update ID
  dtf_long$LNK.INT.True.ID[int_old + int_new] <-
    int_ID_old + int_ID

  # Label test type
  dtf_long$LNK.CHR.True.TestType[int_old + int_new] <-
    'Duplicate records [Add]'

  # Update indices
  int_old <- int_old + max(int_new)
  int_ID_old <- int_ID_old + max(int_ID)

  #### L.D.7) Subset dissimilarity = 0 ####

  int_ID <- 1:12
  lst_new <- list(
    1:2,
    3:4,
    5:6,
    7:8,
    9:10,
    11:12
  )

  # Loop over variables to differ
  for ( v in 1:6 ) {

    # Loop over linking items
    for (l in 1:6) {

      dtf_possible <- lst_setup$linking_questions$possible[[
        chr_linking_questions[l+1]
      ]]

      # Item to differ
      if ( l == v ) {

        dtf_long[[ chr_linking_questions[l+1] ]][
          int_old + lst_new[[v]]
        ] <- swaap_simulate.link.sample(
          dtf_possible[[1]],
          int_size = 2,
          lgc_replace = FALSE,
          int_freq = NULL # dtf_possible[[2]]
        )

        # Close 'Item to differ'
      } else {

        dtf_long[[ chr_linking_questions[l+1] ]][
          int_old + lst_new[[v]]
        ] <- swaap_simulate.link.sample(
          dtf_possible[[1]],
          int_size = 1,
          lgc_replace = FALSE,
          int_freq = NULL # dtf_possible[[2]]
        )

        # Close else for 'Item to differ'
      }

      # Close 'Loop over linking items'
    }

    # Close 'Loop over variables to equate'
  }

  # Update ID
  int_new <- unlist(lst_new)
  dtf_long$LNK.INT.True.ID[int_old + int_new] <-
    0

  # Label test type
  dtf_long$LNK.CHR.True.TestType[int_old + int_new] <-
    'Subset dissimilarity = 0'

  # Update indices
  int_old <- int_old + max(int_new)
  int_ID_old <- int_ID_old + max(int_ID)

  #### L.D.8) Dissimilarity off by 1 ####

  int_ID <- rep( 1:6, each = 2 )
  lst_new <- list(
    1:2,
    3:4,
    5:6,
    7:8,
    9:10,
    11:12
  )

  # Loop over variables to differ
  for ( v in 1:6 ) {

    # Loop over linking items
    for (l in 1:6) {

      dtf_possible <- lst_setup$linking_questions$possible[[
        chr_linking_questions[l+1]
      ]]

      # Item to differ
      if ( l == v ) {

        dtf_long[[ chr_linking_questions[l+1] ]][
          int_old + lst_new[[v]]
        ] <- swaap_simulate.link.sample(
          dtf_possible[[1]],
          int_size = 2,
          lgc_replace = FALSE,
          int_freq = NULL # dtf_possible[[2]]
        )

        # Close 'Item to differ'
      } else {

        dtf_long[[ chr_linking_questions[l+1] ]][
          int_old + lst_new[[v]]
        ] <- swaap_simulate.link.sample(
          dtf_possible[[1]],
          int_size = 1,
          lgc_replace = FALSE,
          int_freq = NULL # dtf_possible[[2]]
        )

        # Close else for 'Item to differ'
      }

      # Close 'Loop over linking items'
    }

    # Close 'Loop over variables to equate'
  }

  # Update ID
  int_new <- unlist(lst_new)
  dtf_long$LNK.INT.True.ID[int_old + int_new] <-
    int_ID_old + int_ID

  # Label test type
  dtf_long$LNK.CHR.True.TestType[int_old + int_new] <-
    'Dissimilarity off by 1'

  # Update indices
  int_old <- int_old + max(int_new)
  int_ID_old <- int_ID_old + max(int_ID)

  #### L.D.9) Duplicate records w/ NA [Base] ####

  int_ID <- 1
  int_new <- 1:3

  # Select variable to be missing
  int_missing <- sample( 1:6, size = 1 )

  # Loop over linking items
  for (l in 1:6) {

    dtf_possible <- lst_setup$linking_questions$possible[[
      chr_linking_questions[l+1]
    ]]

    dtf_long[[ chr_linking_questions[l+1] ]][
      int_old + int_new
    ] <- swaap_simulate.link.sample(
      dtf_possible[[1]],
      int_size = 1,
      lgc_replace = FALSE,
      int_freq = NULL # dtf_possible[[2]]
    )

    # Set missing value in 2nd base record
    if ( l == int_missing ) {

      dtf_long[[ chr_linking_questions[l+1] ]][
        int_old + int_new
      ][2] <- NA

      # Close 'Set missing value in 2nd base record'
    }

    # Close 'Loop over linking items'
  }

  # Update ID
  dtf_long$LNK.INT.True.ID[int_old + int_new] <-
    int_ID_old + int_ID

  # Label test type
  dtf_long$LNK.CHR.True.TestType[int_old + int_new] <-
    'Duplicate records w/ NA [Base]'

  # Update indices
  int_old <- int_old + max(int_new)
  int_ID_old <- int_ID_old + max(int_ID)

  #### L.D.10) Duplicate records w/ NA [Add] ####

  int_ID <- 1
  int_new <- 1:3

  # Select variable to be missing
  int_missing <- sample( 1:6, size = 1 )

  # Loop over linking items
  for (l in 1:6) {

    dtf_possible <- lst_setup$linking_questions$possible[[
      chr_linking_questions[l+1]
    ]]

    dtf_long[[ chr_linking_questions[l+1] ]][
      int_old + int_new
    ] <- swaap_simulate.link.sample(
      dtf_possible[[1]],
      int_size = 1,
      lgc_replace = FALSE,
      int_freq = NULL # dtf_possible[[2]]
    )

    # Set missing value in 1st add record
    if ( l == int_missing ) {

      dtf_long[[ chr_linking_questions[l+1] ]][
        int_old + int_new
      ][2] <- NA

      # Close 'Set missing value in 1st add record'
    }

    # Close 'Loop over linking items'
  }

  # Update ID
  dtf_long$LNK.INT.True.ID[int_old + int_new] <-
    int_ID_old + int_ID

  # Label test type
  dtf_long$LNK.CHR.True.TestType[int_old + int_new] <-
    'Duplicate records w/ NA [Add]'

  # Update indices
  int_old <- int_old + max(int_new)
  int_ID_old <- int_ID_old + max(int_ID)

  #### L.D.11) Test of priority [School ID over questions] ####

  int_ID <- 1
  int_new <- 1:2

  # Loop over linking items
  for (l in 1:6) {

    dtf_possible <- lst_setup$linking_questions$possible[[
      chr_linking_questions[l+1]
    ]]

    dtf_long[[ chr_linking_questions[l+1] ]][
      int_old + int_new
    ] <- swaap_simulate.link.sample(
      dtf_possible[[1]],
      int_size = 2,
      lgc_replace = FALSE,
      int_freq = NULL # dtf_possible[[2]]
    )

    # Close 'Loop over linking items'
  }

  # Update ID
  dtf_long$LNK.INT.True.ID[int_old + int_new] <-
    int_ID_old + int_ID

  # Label test type
  dtf_long$LNK.CHR.True.TestType[int_old + int_new] <-
    'Test of priority [School ID over questions]'

  # Update indices
  int_old <- int_old + max(int_new)
  int_ID_old <- int_ID_old + max(int_ID)

  #### L.D.12) Link using different items by time point ####

  int_ID <- 1
  int_new <- 1:3

  # Loop over linking items
  for (l in 1:6) {

    dtf_possible <- lst_setup$linking_questions$possible[[
      chr_linking_questions[l+1]
    ]]

    dtf_long[[ chr_linking_questions[l+1] ]][
      int_old + int_new[-1]
    ] <- swaap_simulate.link.sample(
      dtf_possible[[1]],
      int_size = 1,
      lgc_replace = FALSE,
      int_freq = NULL # dtf_possible[[2]]
    )

    # Close 'Loop over linking items'
  }

  # Update ID
  dtf_long$LNK.INT.True.ID[int_old + int_new] <-
    int_ID_old + int_ID

  # Label test type
  dtf_long$LNK.CHR.True.TestType[int_old + int_new] <-
    'Link using different items by time point'

  # Update indices
  int_old <- int_old + max(int_new)
  int_ID_old <- int_ID_old + max(int_ID)

  #### L.D.13) Special cases for duplicates ####

  # Case 1

  int_ID <- 1
  int_new <- 1:4

  # Loop over linking items
  for (l in 1:6) {

    dtf_possible <- lst_setup$linking_questions$possible[[
      chr_linking_questions[l+1]
    ]]

    dtf_long[[ chr_linking_questions[l+1] ]][
      int_old + int_new
    ] <- swaap_simulate.link.sample(
      dtf_possible[[1]],
      int_size = 1,
      lgc_replace = FALSE,
      int_freq = NULL # dtf_possible[[2]]
    )

    # Close 'Loop over linking items'
  }

  # Update ID
  dtf_long$LNK.INT.True.ID[int_old + int_new] <-
    int_ID_old + int_ID

  # Label test type
  dtf_long$LNK.CHR.True.TestType[int_old + int_new] <-
    'Special cases for duplicates'

  # Update indices
  int_old <- int_old + max(int_new)
  int_ID_old <- int_ID_old + max(int_ID)

  # Case 2

  int_ID <- 1
  int_new <- 1:4

  # Loop over linking items
  for (l in 1:6) {

    dtf_possible <- lst_setup$linking_questions$possible[[
      chr_linking_questions[l+1]
    ]]

    dtf_long[[ chr_linking_questions[l+1] ]][
      int_old + int_new
    ] <- swaap_simulate.link.sample(
      dtf_possible[[1]],
      int_size = 1,
      lgc_replace = FALSE,
      int_freq = NULL # dtf_possible[[2]]
    )

    # One item off
    if (l == 5) {

      chr_val <- swaap_simulate.link.sample(
        dtf_possible[[1]],
        int_size = 2,
        lgc_replace = FALSE,
        int_freq = NULL # dtf_possible[[2]]
      )

      dtf_long[[ chr_linking_questions[l+1] ]][
        int_old + int_new
      ] <- c(
        chr_val[1], NA, chr_val[2], chr_val[2]
      )

      # Close 'One item off'
    }

    # Close 'Loop over linking items'
  }

  # Update ID
  dtf_long$LNK.INT.True.ID[int_old + int_new] <-
    c( 0,
       int_ID_old + int_ID,
       int_ID_old + int_ID,
       int_ID_old + int_ID )

  # Label test type
  dtf_long$LNK.CHR.True.TestType[int_old + int_new] <-
    'Special cases for duplicates'

  # Update indices
  int_old <- int_old + max(int_new)
  int_ID_old <- int_ID_old + max(int_ID)

  # Case 3

  int_ID <- 1
  int_new <- 1:3

  # Loop over linking items
  for (l in 1:6) {

    dtf_possible <- lst_setup$linking_questions$possible[[
      chr_linking_questions[l+1]
    ]]

    dtf_long[[ chr_linking_questions[l+1] ]][
      int_old + int_new
    ] <- swaap_simulate.link.sample(
      dtf_possible[[1]],
      int_size = 1,
      lgc_replace = FALSE,
      int_freq = NULL # dtf_possible[[2]]
    )

    # Close 'Loop over linking items'
  }

  # Update ID
  dtf_long$LNK.INT.True.ID[int_old + int_new] <-
    int_ID_old + int_ID

  # Label test type
  dtf_long$LNK.CHR.True.TestType[int_old + int_new] <-
    'Special cases for duplicates'

  # Update indices
  int_old <- int_old + max(int_new)
  int_ID_old <- int_ID_old + max(int_ID)

  # Case 4

  int_ID <- 1
  int_new <- 1:4

  # Loop over linking items
  for (l in 1:6) {

    dtf_possible <- lst_setup$linking_questions$possible[[
      chr_linking_questions[l+1]
    ]]

    dtf_long[[ chr_linking_questions[l+1] ]][
      int_old + int_new
    ] <- swaap_simulate.link.sample(
      dtf_possible[[1]],
      int_size = 1,
      lgc_replace = FALSE,
      int_freq = NULL # dtf_possible[[2]]
    )

    # Close 'Loop over linking items'
  }

  # Update ID
  dtf_long$LNK.INT.True.ID[int_old + int_new] <-
    int_ID_old + int_ID

  # Label test type
  dtf_long$LNK.CHR.True.TestType[int_old + int_new] <-
    'Special cases for duplicates'

  # Update indices
  int_old <- int_old + max(int_new)
  int_ID_old <- int_ID_old + max(int_ID)


  #### L.D._) Final processing ####

  # Ensure unlinkable true ID is 0
  dtf_long$LNK.INT.True.ID[
    !dtf_long$LNK.LGC.True.Linkable
  ] <- 0

  # Ensure IDs increment by 1
  int_old_IDs <- dtf_long$LNK.INT.True.ID |>
    unique() |> sort()
  int_new_IDs <- as.numeric(
    as.factor( int_old_IDs )
  ) - 1
  # Loop over IDs
  for ( i in seq_along(int_old_IDs) ) {

    dtf_long$LNK.INT.True.ID[
      dtf_long$LNK.INT.True.ID == int_old_IDs[i]
    ] <- int_new_IDs[i]

    # Close 'Loop over IDs'
  }

  dtf_long$LNK.INT.True.TestType <- as.numeric(
    as.factor( dtf_long$LNK.CHR.True.TestType )
  )

  return( dtf_long )
}

#### L.D) swaap_simulate.link.demo ####

swaap_simulate.link.demo <- function(
    int_RNG_seed = 20250415 ) {

  #### L.D.1) Initialize data ####

  set.seed( int_RNG_seed )

  lst_setup <- swaap_simulate.link.setup()

  dtf_long <- lst_setup$design[
    rep( 1, 54 ),
  ]

  chr_linking_questions <- lst_setup$linking_questions$variables

  dtf_long$IDX.INT.Origin.LASID <- c(
    # Standard linking
    1, rep( NA, 8 ),
    1, rep( NA, 8 ),
    1, rep( NA, 8 ),
    # No links
    2, rep( NA, 8 ),
    3, rep( NA, 8 ),
    4, rep( NA, 8 )
  )
  dtf_long$SSS.INT.School.Code <- c(
    # Standard linking
    rep( 1, 9*3 ),
    # No links
    rep( 1, 9*3 )
  )
  dtf_long$SSS.INT.SurveyYear <- c(
    # Standard linking
    rep( 2023, 18 ),
    rep( 2024, 9 ),
    # No links
    rep( 2023, 18 ),
    rep( 2024, 9 )
  )
  dtf_long$SSS.INT.TimePoint <- c(
    # Standard linking
    rep( 0, 9 ),
    rep( 1, 9 ),
    rep( 2, 9 ),
    # No links
    rep( 0, 9 ),
    rep( 1, 9 ),
    rep( 2, 9 )
  )
  dtf_long$SSS.INT.Grade <- c(
    # Standard linking
    rep( 9, 18 ),
    rep( 10, 9 ),
    # No links
    rep( 9, 18 ),
    rep( 10, 9 )
  )
  # Extra variables for true linked status
  dtf_long$LNK.LGC.True.Linkable <- c(
    # Standard linking
    rep( TRUE, 9*3 ),
    # No links
    rep( FALSE, 9*3 )
  )
  dtf_long$LNK.CHR.True.TestType <- 'Demostration'
  dtf_long$LNK.INT.True.TestType <- 1
  dtf_long$SSS.INT.LongitudinalWave <- 1

  dtf_long$IDX.CHR.Origin.ID <-
    paste0(
      'Fake', 1:nrow(dtf_long)
    )

  dtf_long$SSS.DTM.SurveyStart <- c(
    lst_setup$dates['Y23F'],
    lst_setup$dates['Y24S'],
    lst_setup$dates['Y24F']
  )[dtf_long$SSS.INT.TimePoint + 1]

  #### L.D.2) Standard linking + no links ####

  # Standard linking + no links
  for ( l in 1:7 ) {

    dtf_possible <- lst_setup$linking_questions$possible[[
      chr_linking_questions[l]
    ]]

    int_unique <- c(
      # Standard linking
      2:9,
      # No link
      (9*3 + 1):(9*6)
    )

    dtf_long[[ chr_linking_questions[l] ]][int_unique] <-
      swaap_simulate.link.sample(
        dtf_possible[[1]],
        int_size = length(int_unique),
        lgc_replace = TRUE,
        int_freq = NULL # dtf_possible[[2]]
      )

    # Copy cases that should be linked
    dtf_long[[ chr_linking_questions[l] ]][2:9 + 9] <-
      dtf_long[[ chr_linking_questions[l] ]][2:9]
    dtf_long[[ chr_linking_questions[l] ]][2:9 + 9*2] <-
      dtf_long[[ chr_linking_questions[l] ]][2:9]

    # Close 'Standard linking'
  }

  # Track actual links
  dtf_long$LNK.INT.True.ID[ 1:9 ] <- 1:9
  dtf_long$LNK.INT.True.ID[ 1:9 + 9 ] <- 1:9
  dtf_long$LNK.INT.True.ID[ 1:9 + 9*2 ] <- 1:9

  # Ensure unlinkable true ID is 0
  dtf_long$LNK.INT.True.ID[
    is.na( dtf_long$LNK.INT.True.ID )
  ] <- 0

  return(dtf_long)
}

#### L.D) swaap_simulate.link.duplicates ####

swaap_simulate.link.duplicates <- function(
    int_RNG_seed = 20250415 ) {

  #### L.D.1) Initialize data ####

  set.seed( int_RNG_seed )

  lst_setup <- swaap_simulate.link.setup()

  dtf_long <- lst_setup$design[
    rep( 1, 70 ),
  ]

  chr_linking_questions <- lst_setup$linking_questions$variables

  # 1 [SID]
  # 2 [7 LQ]
  # 3 - 9 [6 of 7 LQ]

  dtf_long$IDX.INT.Origin.LASID <- c(
    # Duplicates
    # + T0
    1, 1, rep( NA, 8*2 ),
    # + T1
    1, 1, rep( NA, 8*2 ),
    # Unique
    # + T0
    2, rep( NA, 8 ),
    # + T1
    3, rep( NA, 8 ),
    # Duplicate SID + unique LQ
    4, 4,
    # Unique SID + duplicate LQ
    5, 6,
    # LQ off by 1
    rep( NA, 6*2 )
  )
  dtf_long$SSS.INT.School.Code <- c(
    # Duplicates
    # + T0
    1, 1, rep( 1, 8*2 ),
    # + T1
    1, 1, rep( 1, 8*2 ),
    # Unique
    # + T0
    1, rep( 1, 8 ),
    # + T1
    1, rep( 1, 8 ),
    # Duplicate SID + unique LQ
    1, 1,
    # Unique SID + duplicate LQ
    1, 1,
    # LQ off by 1
    rep( 1, 6*2 )
  )
  dtf_long$SSS.INT.SurveyYear <- c(
    # Duplicates
    # + T0
    rep( 2023, 2 + 8*2 ),
    # + T1
    rep( 2024, 2 + 8*2 ),
    # Unique
    # + T0
    rep( 2023, 9 ),
    # + T1
    rep( 2024, 9 ),
    # Duplicate SID + unique LQ
    2023, 2023,
    # Unique SID + duplicate LQ
    2023, 2023,
    # LQ off by 1
    rep( 2023, 6*2 )
  )
  dtf_long$SSS.INT.TimePoint <- c(
    # Duplicates
    # + T0
    rep( 0, 9*2 ),
    # + T1
    rep( 1, 9*2 ),
    # Unique
    # + T0
    rep( 0, 9 ),
    # + T1
    rep( 1, 9 ),
    # Duplicate SID + unique LQ
    0, 0,
    # Unique SID + duplicate LQ
    0, 0,
    # LQ off by 1
    rep( 0, 6*2 )
  )
  dtf_long$SSS.INT.Grade <- c(
    # Duplicates
    # + T0
    rep( 9, 9*2 ),
    # + T1
    rep( 10, 9*2 ),
    # Unique
    # + T0
    rep( 9, 9 ),
    # + T1
    rep( 10, 9 ),
    # Duplicate SID + unique LQ
    9, 9,
    # Unique SID + duplicate LQ
    9, 9,
    # LQ off by 1
    rep( 9, 6*2 )
  )
  # Extra variables for true linked status
  dtf_long$LNK.LGC.True.Linkable <- c(
    # Duplicates
    # + T0
    rep( TRUE, 9*2 ),
    # + T1
    rep( TRUE, 9*2 ),
    # Unique
    # + T0
    rep( FALSE, 9 ),
    # + T1
    rep( FALSE, 9 ),
    # Duplicate SID + unique LQ
    FALSE, FALSE,
    # Unique SID + duplicate LQ
    FALSE, FALSE,
    # LQ off by 1
    rep( FALSE, 6*2 )
  )
  dtf_long$LNK.CHR.True.TestType <- c(
    # Duplicates
    # + T0
    rep( 'Duplicates [SID]', 2 ),
    rep( 'Duplicates [LQ]', 8*2 ),
    # + T1
    rep( 'Duplicates [SID]', 2 ),
    rep( 'Duplicates [LQ]', 8*2 ),
    # Unique
    # + T0
    'Unique [SID]', rep( 'Unique [LQ]', 8 ),
    # + T1
    'Unique [SID]', rep( 'Unique [LQ]', 8 ),
    # Duplicate SID + unique LQ
    'Duplicate SID + unique LQ' |> rep(2),
    # Unique SID + duplicate LQ
    'Unique SID + duplicate LQ' |> rep(2),
    # LQ off by 1
    rep( 'LQ off by 1', 6*2 )
  )
  dtf_long$LNK.INT.True.TestType <- as.numeric(
    as.factor( dtf_long$LNK.CHR.True.TestType )
  )
  dtf_long$SSS.INT.LongitudinalWave <- 1

  dtf_long$IDX.CHR.Origin.ID <-
    paste0(
      'Fake', 1:nrow(dtf_long)
    )

  dtf_long$SSS.DTM.SurveyStart <- c(
    lst_setup$dates['Y23F'],
    lst_setup$dates['Y24F']
  )[dtf_long$SSS.INT.TimePoint + 1]

  # Additional variable for completion rates
  dtf_long$QLT.DBL.ProportionCompleted.Total <- .9

  #### L.D.2) Unique ####

  int_pos <- which(
    dtf_long$LNK.CHR.True.TestType %in% 'Unique [LQ]'
  )
  int_missing <- rep( 0:7, 2 )

  # Unique
  for ( l in 1:7 ) {

    dtf_possible <- lst_setup$linking_questions$possible[[
      chr_linking_questions[l]
    ]]

    dtf_long[[ chr_linking_questions[l] ]][int_pos] <-
      swaap_simulate.link.sample(
        dtf_possible[[1]],
        int_size = 8*2,
        lgc_replace = TRUE,
        int_freq = NULL # dtf_possible[[2]]
      )

    # Set one value to missing
    dtf_long[[ chr_linking_questions[l] ]][
      int_pos
    ][int_missing %in% l] <- NA

    # Close 'Duplicates'
  }

  dtf_long$LNK.INT.True.ID <- 0

  #### L.D.3) Duplicates ####

  int_pos <- which(
    dtf_long$LNK.CHR.True.TestType %in% 'Duplicates [LQ]'
  )
  int_rep <- rep(
    1:8, each = 2
  ) |> rep( 2 )

  # Duplicates
  for ( l in 1:7 ) {

    dtf_possible <- lst_setup$linking_questions$possible[[
      chr_linking_questions[l]
    ]]

    dtf_long[[ chr_linking_questions[l] ]][int_pos] <-
      swaap_simulate.link.sample(
        dtf_possible[[1]],
        int_size = 8,
        lgc_replace = TRUE,
        int_freq = NULL # dtf_possible[[2]]
      )[int_rep]

    # Set one value to missing
    dtf_long[[ chr_linking_questions[l] ]][
      int_pos
    ][(int_rep - 1) %in% l] <- NA

    # Close 'Duplicates'
  }

  # Track actual links
  dtf_long$LNK.INT.True.ID[
    dtf_long$LNK.CHR.True.TestType %in% 'Duplicates [SID]'
  ] <- 1
  dtf_long$LNK.INT.True.ID[int_pos] <- int_rep + 1

  # Additional variable for completion rates
  dtf_long$QLT.DBL.ProportionCompleted.Total[
    dtf_long$LNK.CHR.True.TestType %in% 'Duplicates [SID]'
  ] <- c( .9, .5, .9, .5 )
  dtf_long$QLT.DBL.ProportionCompleted.Total[
    int_pos
  ] <- c( .9, .5 ) |> rep( 8*2 )

  #### L.D.4) Duplicate SID + unique LQ ####

  int_pos <- which(
    dtf_long$LNK.CHR.True.TestType %in%
      'Duplicate SID + unique LQ'
  )

  # Unique LQ
  for ( l in 1:7 ) {

    dtf_possible <- lst_setup$linking_questions$possible[[
      chr_linking_questions[l]
    ]]

    dtf_long[[ chr_linking_questions[l] ]][int_pos] <-
      swaap_simulate.link.sample(
        dtf_possible[[1]],
        int_size = 2,
        lgc_replace = TRUE,
        int_freq = NULL # dtf_possible[[2]]
      )

    # Close 'Unique LQ'
  }

  # Track actual links
  dtf_long$LNK.INT.True.ID[int_pos] <- max(
    dtf_long$LNK.INT.True.ID
  ) + 1

  # Additional variable for completion rates
  dtf_long$QLT.DBL.ProportionCompleted.Total[
    dtf_long$LNK.CHR.True.TestType %in% 'Duplicate SID + unique LQ'
  ] <- c( .9, .5 )

  #### L.D.5) Unique SID + duplicate LQ ####

  int_pos <- which(
    dtf_long$LNK.CHR.True.TestType %in%
      'Unique SID + duplicate LQ'
  )

  # Duplicates
  for ( l in 1:7 ) {

    dtf_possible <- lst_setup$linking_questions$possible[[
      chr_linking_questions[l]
    ]]

    dtf_long[[ chr_linking_questions[l] ]][int_pos] <-
      swaap_simulate.link.sample(
        dtf_possible[[1]],
        int_size = 1,
        lgc_replace = TRUE,
        int_freq = NULL # dtf_possible[[2]]
      )

    # Close 'Duplicates'
  }

  # Track actual links
  dtf_long$LNK.INT.True.ID[int_pos] <- max(
    dtf_long$LNK.INT.True.ID
  ) + 1

  # Additional variable for completion rates
  dtf_long$QLT.DBL.ProportionCompleted.Total[
    dtf_long$LNK.CHR.True.TestType %in% 'Duplicate SID + unique LQ'
  ] <- c( .9, .5 )

  #### L.D.6) LQ off by 1 ####

  int_pos <- which(
    dtf_long$LNK.CHR.True.TestType %in% 'LQ off by 1'
  )
  int_rep <- rep(
    1:6, each = 2
  )

  dtf_long[[ chr_linking_questions[1] ]][int_pos] <- 2014

  # Duplicates
  for ( l in 2:7 ) {

    dtf_possible <- lst_setup$linking_questions$possible[[
      chr_linking_questions[l]
    ]]

    dtf_long[[ chr_linking_questions[l] ]][int_pos] <-
      swaap_simulate.link.sample(
        dtf_possible[[1]],
        int_size = 6,
        lgc_replace = TRUE,
        int_freq = NULL # dtf_possible[[2]]
      )[int_rep]

    # Set one value to be different
    lgc_diff <-
      !( dtf_possible[[1]] %in% dtf_long[[ chr_linking_questions[l] ]][
        int_pos
      ][(int_rep+1) %in% l][1] )
    dtf_long[[ chr_linking_questions[l] ]][
      int_pos
    ][(int_rep+1) %in% l][1] <- swaap_simulate.link.sample(
      dtf_possible[[1]][lgc_diff],
      int_size = 1,
      lgc_replace = TRUE,
      int_freq = NULL # dtf_possible[[2]][lgc_diff]
    )

    # Close 'Duplicates'
  }

  # Track actual links
  dtf_long$LNK.INT.True.ID[int_pos] <-
    rep( (1:6) + max( dtf_long$LNK.INT.True.ID), each = 2 )

  # Additional variable for completion rates
  dtf_long$QLT.DBL.ProportionCompleted.Total[
    int_pos
  ] <- c( .9, .5 ) |> rep( 6 )

  return(dtf_long)
}

#### L.R) swaap_simulate.link.realistic ####

swaap_simulate.link.realistic <- function() {




}

#### L.S) swaap_simulate.link.sample ####

swaap_simulate.link.sample <- function(
    vec_values,
    int_size,
    lgc_replace = FALSE,
    int_freq = NULL ) {

  # Special case of single value
  if ( length(vec_values) == 1 ) {

    vec_out <- rep( vec_values, int_size )

    # Close 'Special case of single value'
  } else {

    # Equal or more
    if ( length(vec_values) >= int_size ) {

      vec_out <- sample(
        vec_values,
        size = int_size,
        replace = lgc_replace,
        prob = int_freq
      )

      # Close 'Equal or more'
    } else {

      # Replacement TRUE by necessity
      vec_out <- sample(
        vec_values,
        size = int_size,
        replace = TRUE,
        prob = int_freq
      )

      # Close else for 'Equal or more'
    }

    # Close else for 'Special case of single value'
  }

  return( vec_out )
}

#### L.S) swaap_simulate.link.setup ####

swaap_simulate.link.setup <- function() {

  lst_output <- list(
    dates = c(
      Y23F = '2023-10-01',
      Y24S = '2024-05-01',
      Y24F = '2024-10-01',
      Y25S = '2025-05-01',
      Y25F = '2025-10-01'
    ),
    linking_questions = list(
      variables = c(
        "SBJ.INT.Link.KindergartenYearEst",
        "SBJ.FCT.Sex",
        "SBJ.DTM.Dob",
        "SBJ.FCT.Link.MiddleInitial",
        "SBJ.FCT.Link.EyeColor",
        "SBJ.FCT.Link.OlderSiblings",
        "SBJ.CHR.Link.Streetname"
      ),
      possible = list(
        SBJ.INT.Link.KindergartenYearEst =
          swaap:::swaap_internal.linking_items_marginal_rates[[
            'SBJ.INT.Link.KindergartenYearEst'
          ]] |> dplyr::filter( !is.na(Values) & Grade %in% 9 ) |>
          dplyr::select( Values, Freq ),
        SBJ.FCT.Sex =
          swaap:::swaap_internal.linking_items_marginal_rates[[
            'SBJ.FCT.Sex'
          ]] |> dplyr::filter( !is.na(Values) & Grade %in% 9 ) |>
          dplyr::select( Values, Freq ),
        SBJ.DTM.Dob =
          swaap:::swaap_internal.linking_items_marginal_rates[[
            'SBJ.DTM.Dob'
          ]] |> dplyr::filter( !is.na(Values) & Grade %in% 9 ) |>
          dplyr::select( Values, Freq ),
        SBJ.FCT.Link.MiddleInitial =
          swaap:::swaap_internal.linking_items_marginal_rates[[
            'SBJ.FCT.Link.MiddleInitial'
          ]] |> dplyr::filter( !is.na(Values) & Grade %in% 9 ) |>
          dplyr::select( Values, Freq ),
        SBJ.FCT.Link.EyeColor =
          swaap:::swaap_internal.linking_items_marginal_rates[[
            'SBJ.FCT.Link.EyeColor'
          ]] |> dplyr::filter( !is.na(Values) & Grade %in% 9 ) |>
          dplyr::select( Values, Freq ),
        SBJ.FCT.Link.OlderSiblings =
          swaap:::swaap_internal.linking_items_marginal_rates[[
            'SBJ.FCT.Link.OlderSiblings'
          ]] |> dplyr::filter( !is.na(Values) & Grade %in% 9 ) |>
          dplyr::select( Values, Freq ),
        SBJ.CHR.Link.Streetname =
          swaap:::swaap_internal.linking_items_marginal_rates[[
            'SBJ.CHR.Link.Streetname'
          ]] |> dplyr::filter( !is.na(Values) & Grade %in% 9 ) |>
          dplyr::select( Values, Freq )
      )
    ),
    design = data.frame(
      # Identifiers
      IDX.CHR.Origin.ID = '',
      IDX.INT.Origin.LASID = NA,
      # Session details
      SSS.INT.School.Code = NA,
      SSS.INT.SurveyYear = NA,
      SSS.DTM.SurveyStart = '',
      SSS.INT.TimePoint = NA,
      SSS.INT.LongitudinalWave = NA,
      SSS.INT.Grade = NA,
      # Linking questions [Original]
      SBJ.INT.Link.KindergartenYearEst = NA,
      SBJ.FCT.Sex = NA,
      SBJ.DTM.Dob = NA,
      SBJ.FCT.Link.MiddleInitial = NA,
      SBJ.FCT.Link.EyeColor = NA,
      SBJ.FCT.Link.OlderSiblings = NA,
      SBJ.CHR.Link.Streetname = NA,
      # Extra linking variables for true status
      LNK.LGC.True.Linkable = NA,
      LNK.INT.True.ID = NA,
      LNK.CHR.True.TestType = '',
      LNK.INT.True.TestType = NA
    )
  )

  return( lst_output )
}

#### S) SBIRT ####

#### S.D) swaap_simulate.SBIRT.design ####

swaap_simulate.SBIRT.design <- function() {

}

swaap_simulate.linkable <- function(
    dtf_data,
    int_wave = 1,
    int_repetitions = 30 ) {

  dtf_schools <- dtf_data |>
    dplyr::filter(
      SSS.INT.LongitudinalWave %in% int_wave
    ) |>
    dplyr::group_by(
      Wave = SSS.INT.LongitudinalWave,
      School = SSS.INT.SchoolCode
    ) |>
    dplyr::summarise(
      Enrolled = max(
        unique( SSS.INT.SchoolEnrollment ),
        sum( SSS.INT.TimePoint == 0 )
      ),
      Surveyed = sum(
        SSS.INT.TimePoint == 0
      ),
      Surveyed_with_SID = sum(
        !is.na( IDN.CHR.LocallyAssignedSchool.ID ) &
          SSS.INT.TimePoint == 0
      ),
      .groups = 'drop'
    ) |>
    data.frame()

  int_ID <- sum(
    dtf_schools$Enrolled
  )
  int_S <- lapply(
    1:nrow(dtf_schools), function(s) {

      rep( dtf_schools$School[s], dtf_schools$Enrolled[s] )

    }
  ) |> unlist()
  num_R <- lapply(
    1:nrow(dtf_schools), function(s) {

      rep( dtf_schools$Surveyed[s] /
             dtf_schools$Enrolled[s],
           dtf_schools$Enrolled[s] )

    }
  ) |> unlist()
  num_RwI <- lapply(
    1:nrow(dtf_schools), function(s) {

      rep( dtf_schools$Surveyed_with_SID[s] /
             dtf_schools$Surveyed[s],
           dtf_schools$Enrolled[s] )

    }
  ) |> unlist()

  int_TPs <- 0:4
  int_Gs <- c( 9, 9, 10, 10, 11 )
  int_TP <- length(int_TPs)
  int_R <- int_ID*int_TP
  # Loop over repetitions
  for (iter in 1:int_repetitions ) {

    dtf_sim <- data.frame(
      SSS.INT.SchoolCode = rep(
        int_S, int_TP
      ),
      IDN.INT.Row = 1:int_R,
      IDN.CHR.Record.ID = paste0( 'Fake', (1:int_R) ),
      SSS.INT.TimePoint = rep(
        int_TPs, each = int_ID
      ),
      SSS.INT.Grade = rep(
        int_Gs, each = int_ID
      ),
      SSS.INT.Link.SchoolCode = 1,
      IDN.CHR.LocallyAssignedSchool.ID = rep(
        1:int_ID, int_TP
      ),
      SSS.DBL.ProbSurveyed = rep(
        num_R, int_TP
      ),
      SSS.DBL.ProbSurveyedWithSID = rep(
        num_RwI, int_TP
      ),
      SSS.INT.SurveyedWithSID = 0
    )

    dtf_sim$SSS.INT.Surveyed <- rbinom(
      nrow(dtf_sim), 1, dtf_sim$SSS.DBL.ProbSurveyed
    )

    lgc_surveyed <- dtf_sim$SSS.INT.Surveyed %in% 1
    dtf_sim$SSS.INT.SurveyedWithSID[
      lgc_surveyed
    ] <- rbinom(
      sum(lgc_surveyed),
      1,
      dtf_sim$SSS.DBL.ProbSurveyedWithSID[lgc_surveyed]
    )

    int_surveyed_at_baseline <- dtf_sim$IDN.CHR.LocallyAssignedSchool.ID[
      dtf_sim$SSS.INT.TimePoint == 0 &
        dtf_sim$SSS.INT.SurveyedWithSID %in% 1
    ]

    lst_estimated_linkage <- lapply(
      int_TPs[-1], function(i) {

        int_num <- sum(
          int_surveyed_at_baseline %in%
            dtf_sim$IDN.CHR.LocallyAssignedSchool.ID[
              dtf_sim$SSS.INT.TimePoint %in% (1:i) &
                dtf_sim$SSS.INT.SurveyedWithSID
            ]
        )

        int_denom <- sum(
          dtf_sim$SSS.INT.Surveyed %in% 1 &
            dtf_sim$SSS.INT.TimePoint %in% 0
        )

        return(
          c(
            int_num, int_denom, int_num/int_denom
          )
        )

      }
    )

    num_values <- unlist(
      lst_estimated_linkage
    )

    # Initialize output
    if ( iter == 1 ) {

      mat_results <- matrix(
        NA, int_repetitions,
        length( num_values )
      )

      # Close 'Initialize output'
    }

    mat_results[iter, ] <- num_values

    # Close 'Loop over repetitions'
  }

  # Useful summaries

  int_pos <- seq( 3, ncol(mat_results), 3 )
  num_mean <- colMeans( mat_results[, int_pos] )
  num_SD <- apply(
    mat_results[, int_pos],
    2,
    sd
  )
  num_LB <-
    num_mean + num_SD/sqrt(int_repetitions)*qt(.025, int_repetitions - 1)
  num_UB <-
    num_mean + num_SD/sqrt(int_repetitions)*qt(.975, int_repetitions - 1)

  lst_output <- list(
    summary = data.frame(
      Time = 1:4,
      Cumulative_linked = paste0(
        round( num_mean*100, 1 ), '% (',
        round( num_LB*100, 1 ), '% to ',
        round( num_UB*100, 1 ), '%)'
      )
    ),
    results = mat_results
  )

  return( lst_output )
}
