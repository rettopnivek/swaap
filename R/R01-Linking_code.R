# School-wide assessment linking code
# Written by...
#   Michael Pascale
#   Kevin Potter
# Maintained by...
#   Kevin Potter
# Email:
#   kpotter5@mgh.harvard.edu
# Please email me directly if you
# have any questions or comments
# Last updated: 2025-10-10

# TO DO:
# - Add tests for swaap_link
#   * Exact matching (school)
#   * Exact matching (district)
#   * fastLink (school)
#   * fastLink (district)
#   * fastLink (contact info)
# - Add exact matching component to fastLink
# - Confirm linking works with districts
# - Add adaptive process for linking via districts
#   for middle to high school transition
# - Create 'true' links data set to test linking

# Table of contents
# 1) Internal functions
#   1.1) swaap_link.internal.assign_IDs
#     1.1.1) Setup
#     1.1.2) Identify links
#   1.2) swaap_link.internal.via_dissimilarity
#     1.2.1) Setup
#     1.2.2) Identify rows for linking
#     1.2.3) Match over all records and items
#     1.2.4) Compute dissimilarity scores
#   1.3) swaap_link.internal.via_group_by
#     1.3.1) Setup
#     1.3.2) Match records via items
#     1.3.3) Identify linked rows
#   1.4) swaap_link.internal.via_fastLink
#     1.4.1) Setup
#     1.4.2) Identify rows for linking
#     1.4.3) Run fastLink
# 2) swaap_link
#   2.1) Setup
#     2.1.1) fun_copy_prior
#   2.2) Link records
#   2.3) Assign IDs
#   2.4) Final trimming of duplicates
# 3) Input functions
#   3.1) swaap_link.input.sets
#     3.1.1) Default - SBIRT
#     3.1.2) Default - survey
#     3.1.3) Duplicates - SBIRT
#     3.1.4) Duplicates - survey
#     3.1.5) Check input
#   3.2) swaap_link.input.items
#   3.3) swaap_link.input.combos
#     3.3.1) Exact matching
#     3.3.2) fastLink
#   3.4) swaap_link.input.missing
# 4) Helper functions
#   4.1) swaap_link.parameters
#   4.2) swaap_link.rows
#   4.3) swaap_link.trim
#   4.4) swaap_link.trim_rule
#     3.5.1) List of defined rules
#     4.4.2) Rules for trimming duplicates
#       4.4.2.1) fun_rule.completed
#       4.4.2.2) fun_rule.outcome_and_completed
#       4.4.2.3) fun_rule.duplicate_times
#     4.4.3) Return specified rule
#   4.5) swaap_link.similarity
#   4.6) swaap_link.sets
#   3.8) swaap_link.timepoints
#   3.9) swaap_link.linked_over
# 5) Report functions
#   5.1) swaap_link.report.by_ID
#   5.2) swaap_link.report
#     5.2.1) Setup
#   #5.2) Linkage patterns [Overall]
#   #5.3) Linkage patterns [Groups]
#   #5.4) Any linked [Overall]
#     5.2.?) ...
#   5.3) swaap_link.report.discrepant
#     5.3.1) Setup
#     5.3.2) Plotting
#       5.3.2.1) Panel 1
#       5.3.2.2) Panel 2
#       5.3.2.3) Panel 3
#       5.3.2.4) Panel 4
#   5.4) swaap_link.report.comparison


#### 1) Internal functions ####

#### 1.1) swaap_link.internal.assign_IDs ####
# Assign Identifiers Based on Linkage
#
# @param 'dtf_long' A data frame, assumed to be standard processed
#   school-wide assessment data with the columns
#   \code{'SSS.INT.TimePoint'} and \code{'SSS.INT.LongitudinalWave'}
#   as well as the linking code items. The data frame is
#   assumed to have been run through either the function
#   'swaap_link.internal.via_dissimilarity',
#   'swaap_link.internal.via_group_by', or
#   'swaap_link.internal.via_fastLink' beforehand.
# @param 'lgc_progress' A logical value; if TRUE displays the
#   progress of the function using section labels.
# @param 'lgc_progress_bar' A logical value; if TRUE displays the
#   progress of the function using a progress bar.
#
# @author Kevin Potter
#
# @returns A data frame.

swaap_link.internal.assign_IDs <- function(
    dtf_long,
    lgc_progress,
    lgc_progress_bar ) {

  #### 1.1.1) Setup ####

  if (lgc_progress) message( '    Setup for assigning IDs' )

  lgc_linked <- dtf_long$LNK.CHR.Rows != ""
  chr_old_ID <- dtf_long$IDN.CHR.Linked.ID

  # No links
  if ( !any(lgc_linked) ) {

    warning(
      "No links found"
    )

    return( dtf_long )

    # Close 'No links'
  }

  #### 1.1.2) Identify links ####

  if (lgc_progress) message( '    Identify links' )

  # Extract unique linked pairs
  chr_linked_rows <- dtf_long$LNK.CHR.Rows[lgc_linked]

  chr_linked_rows <- lapply(
    chr_linked_rows, function(s) {
      chr_out <- strsplit( s, split = ';', fixed = TRUE )[[1]]
      chr_out <- chr_out[ chr_out != '' ]
      return(chr_out )
    }
  ) |> unlist() |> unique()
  mat_linked_rows <- sapply(
    chr_linked_rows, function(r) {
      strsplit( r, split = ',', fixed = TRUE )[[1]] |> as.numeric()
    }
  )

  int_all_rows <- unique( as.vector( mat_linked_rows[1:2, ] ) ) |> sort()

  lst_all_links <- rep(
    list(NULL), length(int_all_rows)
  )

  int_to_check <- int_all_rows
  int_inc <- 1

  # Loop over possible links
  for (i in seq_along(int_all_rows)) {

    lgc_col <-
      mat_linked_rows[1, ] %in% int_all_rows[i] |
      mat_linked_rows[2, ] %in% int_all_rows[i]
    int_all_combos <-
      mat_linked_rows[1:2, lgc_col] |>
      as.vector() |> unique() |> sort()
    lgc_col <-
      mat_linked_rows[1, ] %in% int_all_combos |
      mat_linked_rows[2, ] %in% int_all_combos

    # First time
    if ( i == 1 ) {

      int_rows_to_consider <- as.vector(
        mat_linked_rows[1:2, lgc_col]
      ) |> unique() |> sort()

      lst_all_links[[int_inc]] <- int_rows_to_consider

      int_to_check <- int_to_check[
        !int_to_check %in% lst_all_links[[int_inc]]
      ]
      int_inc <- int_inc + 1

      # Close 'First time'
    } else {

      # Check if row has not already been included
      if ( int_all_rows[i] %in% int_to_check) {

        int_rows_to_consider <- as.vector(
          mat_linked_rows[1:2, lgc_col]
        ) |> unique() |> sort()

        lst_all_links[[int_inc]] <- int_rows_to_consider

        int_to_check <- int_to_check[
          !int_to_check %in% lst_all_links[[int_inc]]
        ]
        int_inc <- int_inc + 1

        # Close 'Check if row has not already been included'
      }

      # Close else for 'First time'
    }

    # Close 'Loop over possible links'
  }

  # Remove empty slots
  lst_all_links <- lst_all_links[
    !sapply( lst_all_links, is.null )
  ]
  int_L <- length( lst_all_links )

  # Set up progress bar
  if ( lgc_progress_bar & int_L > 1 ) {

    message('')
    obj_pb <- txtProgressBar(
      min = 1, max = length( lst_all_links ), style = 3
    )

    # Close 'Set up progress bar'
  }

  # Loop over possible links
  for ( l in seq_along(lst_all_links) ) {

    int_freq <-
      dtf_long$SSS.INT.TimePoint[ lst_all_links[[l]] ] |> table()

    # Check for duplicates
    if ( any(int_freq > 1 ) ) {

      dtf_long$IDN.CHR.Linked.ID[ lst_all_links[[l]] ] <-
        gsub(
          'UID', 'DID', dtf_long$IDN.CHR.Linked.ID[ lst_all_links[[l]] ]
        )[1]
      dtf_long$LNK.LGC.Duplicates[ lst_all_links[[l]] ] <- TRUE

      # Close 'Check for duplicates'
    } else {

      dtf_long$IDN.CHR.Linked.ID[ lst_all_links[[l]] ] <-
        gsub(
          'UID', 'LID', dtf_long$IDN.CHR.Linked.ID[ lst_all_links[[l]] ]
        )[1]
      dtf_long$LNK.LGC.NoIssues[ lst_all_links[[l]] ] <- TRUE

      # Close else for 'Check for duplicates'
    }

    # Update the progress bar
    if (lgc_progress_bar & int_L > 1)
      setTxtProgressBar(obj_pb, l)

    # Close 'Loop over possible links'
  }
  if (lgc_progress_bar & int_L > 1) close(obj_pb)

  return( dtf_long )
}

#### 1.2) swaap_link.internal.via_dissimilarity ####
# Link Records via Dissimiliarity Scores
#
# Internal function to conduct record linkage using
# dissimilarity scores.
#
# @param 'dtf_long' A data frame, assumed to be standard processed
#   school-wide assessment data with the columns
#   \code{'SSS.INT.TimePoint'} and \code{'SSS.INT.LongitudinalWave'}
#   as well as the linking code items.
# @param 'lst_sets' A list of lists, with each sublist specifying
#   'Base' and 'Add' logical vectors for the pair of data subsets
#   in 'dtf_long' to link over (e.g., 'Base' would subset the first
#   time point and 'Add' would subset the second time point).
# @param 'lst_items' A list of character vectors, one vector for
#   each set defined in 'lst_sets'.
# @param 'lst_combos' A list of lists, where each sublist consists of
#   an integer vector indexing the combination of linking items to
#   consider in order of priority. One sublist of integer vectors must
#   be defined for each set defined by 'lst_sets'. For a
#   given sublist, indices apply to the character vector defined
#   for the relevant set in 'lst_items'.
# @param 'lst_missing' A list of lists, where each sublist consists of
#   an integer vector indicating which items should be checked for
#   missingness when linking using a given combo.
# @param 'lgc_progress' A logical value; if TRUE displays the
#   progress of the function using section labels.
# @param 'lgc_progress_bar' A logical value; if TRUE displays the
#   progress of the function using a progress bar.
#
# @author Kevin Potter
#
# @returns A data frame with additional columns with details on
# record linkage.

swaap_link.internal.via_dissimilarity <- function(
    dtf_long,
    lst_sets,
    lst_items,
    lst_combos,
    lst_missing,
    lgc_progress,
    lgc_progress_bar ) {

  #### 1.2.1) Setup ####

  int_prog <- 0

  # Create progress bar parameters
  if (lgc_progress_bar) {

    int_combo <- sapply(
      seq_along(lst_combos), function(s) {
        length(lst_combos[[s]])
      }
    )

    int_total <- sum( length(lst_sets)*2 + sum(int_combo) )

    obj_pb <- txtProgressBar(
      min = 1, max = int_total, style = 3
    )

    # Close 'Create progress bar parameters'
  }

  # Loop over sets
  for ( s in seq_along(lst_sets) ) {

    if (lgc_progress)
      message( paste0( '    Set: ', names(lst_sets)[s] ) )

    #### 1.2.2) Identify rows for linking ####

    if ( lgc_progress )
      message( '    Identify rows for linking' )

    lgc_base <-
      lst_sets[[s]]$Base

    lgc_add <-
      lst_sets[[s]]$Add

    # Check if assessing for duplicates
    lgc_duplicates <- all( lgc_base == lgc_add )

    # Indicate that linkage was attempted
    dtf_long$LNK.LGC.Attempted[
      lgc_base | lgc_add
    ] <- TRUE
    # Update method
    dtf_long$LNK.CHR.Method[
      lgc_base | lgc_add
    ] <- 'dissimilarity'

    # All possible linking items
    chr_all_items <-
      lst_items |> unlist() |> unique() |> sort()
    int_items <- length(chr_all_items)

    int_total_comparisons <-
      sum(lgc_base)*sum(lgc_add)

    # Row indices for comparison pairs
    mat_rows <- matrix(
      NA, int_total_comparisons, 2
    )
    colnames(mat_rows) <- c( 'base', 'add' )
    mat_rows[, 1] <- rep(
      which(lgc_base), each = sum(lgc_add)
    )
    mat_rows[, 2] <- rep(
      which(lgc_add), sum(lgc_base)
    )

    #### 1.2.3) Match over all records and items ####

    if ( lgc_progress )
      message( '      Compute matches over linking items' )

    # Update progress bar
    int_prog <- int_prog + 1
    if (lgc_progress_bar)
      setTxtProgressBar(obj_pb, int_prog)

    mat_items_base <- matrix(
      NA, int_total_comparisons, int_items
    )
    colnames(mat_items_base) <- chr_all_items
    mat_items_add <- mat_items_base

    # Loop over items
    for ( i in seq_along(chr_all_items) ) {

      if ( lgc_progress )
        message( paste0( '      + Copying (', i, ')' ) )

      mat_items_base[, i] <- rep(
        dtf_long[[ chr_all_items[i] ]][lgc_base],
        each = sum(lgc_add)
      )

      mat_items_add[, i] <- rep(
        dtf_long[[ chr_all_items[i] ]][lgc_add],
        sum(lgc_base)
      )

      # Close 'Loop over items'
    }

    if ( lgc_progress )
      message( paste0( '      + Matching' ) )

    mat_matches <-
      mat_items_base == mat_items_add
    colnames(mat_matches) <- chr_all_items

    # Missing cases
    mat_missing <- is.na( mat_matches )

    # Update progress bar
    int_prog <- int_prog + 1
    if (lgc_progress_bar)
      setTxtProgressBar(obj_pb, int_prog)

    # Exclude comparisons to same record
    if (lgc_duplicates) {

      mat_matches[
        mat_rows[, 1] == mat_rows[, 2],
      ] <- FALSE

      # Close 'Exclude comparisons to same record'
    }

    # Track which pairs have been matched already
    lgc_already <- rep( FALSE, int_total_comparisons )

    # Clean up workspace
    rm( mat_items_base, mat_items_add)
    # Force garbage collection
    gc()

    #### 1.2.4) Compute dissimilarity scores ####

    mat_diss_scores <- matrix(
      NA,
      int_total_comparisons,
      length(lst_combos[[s]])
    )

    # Loop over combos
    for ( j in seq_along(lst_combos[[s]] ) ) {

      if ( lgc_progress )
        message( paste0( '      + Dissimilarity scores (', j, ')' ) )

      chr_current_items <- lst_items[[s]][
        lst_combos[[s]][[j]]
      ]
      mat_diss_scores[, j] <-
        length(chr_current_items) -
        rowSums( mat_matches[, chr_current_items] )

      # Avoid linking if larger subset not missing
      if ( length( lst_missing[[s]][[j]] ) > 0 ) {

        chr_any_missing <-
          lst_items[[s]][
            lst_missing[[s]][[j]]
          ]
        mat_diss_scores[, j] <-
          mat_diss_scores[, j] +
          ( ( length( chr_any_missing ) -
                rowSums( as.matrix( mat_missing[, chr_any_missing] ) ) ) -
              length(chr_current_items) )

        # Close 'Avoid linking if larger subset not missing'
      }

      lgc_zero <-
        mat_diss_scores[, j] %in% 0 &
        !lgc_already

      # Any matches
      if ( any(lgc_zero) ) {

        lgc_already[lgc_zero] <- TRUE

        mat_pairs <- mat_rows[lgc_zero, ]

        # Make sure is matrix
        if ( is.null( dim(mat_pairs) ) ) {

          mat_pairs <- rbind( mat_pairs )

          # Close 'Make sure is matrix'
        }

        # Check for duplicate matches
        int_unique_base <- table( mat_pairs[, 1] )
        int_unique_add <- table( mat_pairs[, 2] )

        int_unique_base <- as.numeric(
          names(int_unique_base)[int_unique_base == 1]
        )
        int_unique_add <- as.numeric(
          names(int_unique_add)[int_unique_add == 1]
        )

        lgc_unique <-
          mat_pairs[, 1] %in% int_unique_base &
          mat_pairs[, 2] %in% int_unique_add

        # Update data set

        # Loop over base and add subsets
        for ( k in 1:2 ) {

          int_rows <- mat_pairs[lgc_unique, k]
          dtf_long$LNK.CHR.Rows[int_rows] <- paste0(
            dtf_long$LNK.CHR.Rows[int_rows],
            mat_pairs[lgc_unique, 1],
            ',',
            mat_pairs[lgc_unique, 2],
            ';'
          )
          dtf_long$LNK.LGC.Preliminary[int_rows] <- TRUE

          # Close 'Loop over base and add subsets'
        }

        # Duplicate matches
        if ( any(!lgc_unique) ) {

          # Loop over individual pairs
          for ( p in which(!lgc_unique) ) {

            # Update data set
            int_rows <- mat_pairs[p, 1]

            # Loop over base and add subsets
            for ( k in 1:2 ) {

              int_rows <- mat_pairs[p, k]
              dtf_long$LNK.CHR.Rows[int_rows] <- paste0(
                dtf_long$LNK.CHR.Rows[int_rows],
                mat_pairs[p, 1],
                ',',
                mat_pairs[p, 2],
                ';'
              )
              dtf_long$LNK.LGC.Duplicates[int_rows] <- TRUE

              # Close 'Loop over base and add subsets'
            }

            # Close 'Loop over individual pairs'
          }

          # Close 'Duplicate matches'
        }

        # Close 'Any matches'
      }

      # Update progress bar
      if (lgc_progress_bar)
        setTxtProgressBar(obj_pb, int_prog + j)

      # Close 'Loop over combos'
    }
    int_prog <- int_prog + length(lst_combos[[s]])

    # Clean up workspace
    rm(
      mat_matches,
      mat_missing, lgc_already,
      mat_diss_scores, lgc_zero
    )
    # Force garbage collection to reduce memory load
    gc()

    # Close 'Loop over sets'
  }

  return( dtf_long )
}

#### 1.3) swaap_link.internal.via_group_by ####
# Link Records via Grouping Factors
#
# Internal function to conduct record linkage using
# the 'group_by' function from the 'dplyr' R package.
#
# @param 'dtf_long' A data frame, assumed to be standard processed
#   school-wide assessment data with the columns
#   \code{'SSS.INT.TimePoint'} and \code{'SSS.INT.LongitudinalWave'}
#   as well as the linking code items.
# @param 'lst_sets' A list of lists, with each sublist specifying
#   'Base' and 'Add' logical vectors for the pair of data subsets
#   in 'dtf_long' to link over (e.g., 'Base' would subset the first
#   time point and 'Add' would subset the second time point).
# @param 'lst_items' A list of character vectors, one vector for
#   each set defined in 'lst_sets'.
# @param 'lgc_progress' A logical value; if TRUE displays the
#   progress of the function using section labels.
# @param 'lgc_progress_bar' A logical value; if TRUE displays the
#   progress of the function using a progress bar.
#
# @author Kevin Potter
#
# @returns A data frame.

swaap_link.internal.via_group_by <- function(
    dtf_long,
    lst_sets,
    lst_items,
    lgc_progress,
    lgc_progress_bar ) {

  #### 1.3.1) Setup ####

  chr_items <- unique( unlist( lst_items ) )

  # Loop over sets
  for ( s in seq_along(lst_sets) ) {

    if (lgc_progress)
      message( paste0( '    Set: ', names(lst_sets)[s] ) )

    if (lgc_progress) message( '    Identify rows' )

    lgc_base <-
      lst_sets[[s]]$Base
    lgc_add <-
      lst_sets[[s]]$Add

    lgc_duplicates <- FALSE
    if ( all( lgc_base == lgc_add ) )
      lgc_duplicates <- TRUE

    lgc_all <- lgc_base | lgc_add

    # Update indicator for attempting linkage
    dtf_long$LNK.LGC.Attempted[lgc_all] <- TRUE
    dtf_long$LNK.CHR.Method[lgc_all] <- 'group_by'

    #### 1.3.2) Match records via items ####

    if (lgc_progress) message( '    Group by items' )

    int_TP <- unique(dtf_long$SSS.INT.TimePoint[lgc_all]) |> sort()
    int_times <- rep_len( int_TP, 10 ) |> sort()
    int_index <- lapply(
      seq_along( int_TP ), function(i) {
        return( 1:sum( int_times == int_TP[i] ) )
      }
    ) |> unlist()

    dtf_patterns <- dtf_long[lgc_all, ] |>
      dplyr::group_by_at(
        chr_items
      ) |>
      dplyr::summarise(
        Distinct = dplyr::n_distinct(
          SSS.INT.TimePoint
        ),
        Records = length( SSS.INT.TimePoint ),
        RW_0 =
          IDN.INT.Row[ SSS.INT.TimePoint %in% int_times[1] ][int_index[1]],
        RW_1 =
          IDN.INT.Row[ SSS.INT.TimePoint %in% int_times[2] ][int_index[2]],
        RW_2 =
          IDN.INT.Row[ SSS.INT.TimePoint %in% int_times[3] ][int_index[3]],
        RW_3 =
          IDN.INT.Row[ SSS.INT.TimePoint %in% int_times[4] ][int_index[4]],
        RW_4 =
          IDN.INT.Row[ SSS.INT.TimePoint %in% int_times[5] ][int_index[5]],
        RW_5 =
          IDN.INT.Row[ SSS.INT.TimePoint %in% int_times[6] ][int_index[6]],
        RW_6 =
          IDN.INT.Row[ SSS.INT.TimePoint %in% int_times[7] ][int_index[7]],
        RW_7 =
          IDN.INT.Row[ SSS.INT.TimePoint %in% int_times[8] ][int_index[8]],
        RW_8 =
          IDN.INT.Row[ SSS.INT.TimePoint %in% int_times[9] ][int_index[9]],
        RW_9 =
          IDN.INT.Row[ SSS.INT.TimePoint %in% int_times[10] ][int_index[10]],
        .groups = 'drop'
      ) |>
      data.frame()

    # Check for missing linking items
    dtf_patterns$Missing <- sapply(
      1:nrow(dtf_patterns), function(r) {
        sum(
          is.na( dtf_patterns[r, chr_items ] )
        )
      }
    )

    #### 1.3.3) Identify linked rows ####

    if (lgc_progress) message( '    Identify links' )

    # Duplicates
    if ( lgc_duplicates ) {

      dtf_linked <- dtf_patterns |>
        dplyr::filter(
          Distinct == 1 &
            Missing %in% 0 &
            Records > 1
        )

      # Close 'Duplicates'
    } else {

      dtf_linked <- dtf_patterns |>
        dplyr::filter(
          Distinct > 1 &
            Missing %in% 0 &
            Distinct == Records
        )

      # Close else for 'Duplicates'
    }

    # Any successful links
    if ( nrow(dtf_linked) > 0 ) {

      # Progress bar
      if ( lgc_progress_bar & nrow(dtf_linked) > 1 ) {

        obj_pb <- txtProgressBar(
          min = 1, max = nrow(dtf_linked), style = 3
        )

        # Close 'Progress bar'
      }

      # Loop over linked cases
      for ( r in 1:nrow(dtf_linked) ) {

        int_rows <- as.numeric(
          dtf_linked[r, paste0( 'RW_', 0:9 )]
        )
        int_rows <- int_rows[ !is.na(int_rows) ]

        # If at least two rows
        if ( length(int_rows) > 1 ) {

          dtf_long$LNK.CHR.Rows[int_rows] <- paste0(
            dtf_long$LNK.CHR.Rows[int_rows],
            paste(
              paste0( int_rows[1], ',', int_rows[-1] ),
              collapse = ';'
            ) |> paste0(';')
          )
          dtf_long$LNK.LGC.Preliminary[int_rows] <- TRUE

          # Close 'If at least two rows'
        }

        # Update the progress bar
        if (lgc_progress_bar & nrow(dtf_linked) > 1)
          setTxtProgressBar(obj_pb, r)

        # Close 'Loop over linked cases'
      }
      if (lgc_progress_bar & nrow(dtf_linked) > 1) close(obj_pb)

      # Close 'Any successful links'
    }

    # Close 'Loop over sets'
  }

  return( dtf_long )
}

#### 1.4) swaap_link.internal.via_fastLink ####
# Link Records Using fastLink Function
#
# Internal function to conduct record linkage using
# the fastLink package tools.
#
# @param 'dtf_long' A data frame, assumed to be standard processed
#   school-wide assessment data with the columns
#   \code{'SSS.INT.TimePoint'} and \code{'SSS.INT.LongitudinalWave'}
#   as well as the linking code items.
# @param 'lst_sets' A list of lists, with each sublist specifying
#   'Base' and 'Add' logical vectors for the pair of data subsets
#   in 'dtf_long' to link over (e.g., 'Base' would subset the first
#   time point and 'Add' would subset the second time point).
# @param 'lst_items' A list of character vectors, one vector for
#   each set defined in 'lst_sets'.
# @param 'lst_combos' A list of lists, where each sublist consists
#   of integer indices for the items to pass to the 'stringdist',
#   'numeric', and 'partial' arguments of the fastLink function.
# @param 'lst_fastLink_args' A list of additional arguments to
#   pass to the fastLink function.
# @param 'lgc_progress' A logical value; if TRUE displays the
#   progress of the function using section labels.
# @param 'lgc_progress_bar' A logical value; if TRUE displays the
#   progress of the function using a progress bar.
#
# @author Kevin Potter
#
# @returns A data frame.

swaap_link.internal.via_fastLink <- function(
    dtf_long,
    lst_sets,
    lst_items,
    lst_combos,
    lst_fastLink_args,
    lgc_progress,
    lgc_progress_bar ) {

  #### 1.4.1) Setup ####

  int_prog <- 0

  # Create progress bar parameters
  if (lgc_progress_bar) {

    obj_pb <- txtProgressBar(
      min = 1, max = length(lst_sets), style = 3
    )

    # Close 'Create progress bar parameters'
  }

  # Loop over sets
  for ( s in seq_along(lst_sets) ) {

    if (lgc_progress)
      message( paste0( '    Set: ', names(lst_sets)[s] ) )

    chr_prob <- paste0( 'LNK.DBL.PostProb.Set', s )
    dtf_long[[ chr_prob ]] <- NA

    #### 1.4.2) Identify rows for linking ####

    if ( lgc_progress )
      message( '    Identify rows for linking' )

    lgc_base <-
      lst_sets[[s]]$Base

    lgc_add <-
      lst_sets[[s]]$Add

    # Check if assessing for duplicates
    lgc_duplicates <- all( lgc_base == lgc_add )

    # Indicate that linkage was attempted
    dtf_long$LNK.LGC.Attempted[
      lgc_base | lgc_add
    ] <- TRUE
    # Update method
    dtf_long$LNK.CHR.Method[
      lgc_base | lgc_add
    ] <- 'fastLink'

    #### 1.4.3) Run fastLink ####

    if (lgc_progress)
      message( paste0( '    Run fastLink' ) )

    chr_items <- lst_items[[s]]
    # print( chr_items )

    lst_args <- list(
      dfA = dtf_long[lgc_base, ],
      dfB = dtf_long[lgc_add, ],
      varnames = chr_items
    )

    # If combo input provided
    if ( !is.null(lst_combos) ) {


      # Items for string distance match
      if ( !is.null( lst_combos[[s]]$stringdist ) ) {

        lst_args$stringdist.match <- chr_items[
          lst_combos[[s]]$stringdist
        ]

        # Close 'Items for string distance match'
      }

      # Items for partial match
      if ( !is.null( lst_combos[[s]]$partial ) ) {

        lst_args$partial.match <- chr_items[
          lst_combos[[s]]$partial
        ]

        # Close 'Items for partial match'
      }

      # Items for numeric match
      if ( !is.null( lst_combos[[s]]$numeric ) ) {

        lst_args$numeric.match <- chr_items[
          lst_combos[[s]]$numeric
        ]

        # Close 'Items for numeric match'
      }

      # Close 'If combo input provided'
    }

    # If additional arguments provided
    if ( !is.null(lst_fastLink_args) ) {

      # Loop over elements
      for ( l in seq_along(lst_fastLink_args) ) {

        # Add argument
        if ( names(lst_fastLink_args)[l] != 'chr_exact' )
          lst_args[[ names(lst_fastLink_args)[l] ]] <-
            lst_fastLink_args[[l]]

        # Close 'Loop over elements'
      }

      # Close 'If additional arguments provided'
    }

    lst_fastLink <- suppressMessages( do.call(
      fastLink::fastLink,
      lst_args
    ) )

    int_rows <- c( NA, NA )

    # Loop over matches
    for ( m in 1:nrow(lst_fastLink$matches) ) {

      int_rows[1] <-
        dtf_long$IDN.INT.Row[lgc_base][
          lst_fastLink$matches[m, 1]
        ]
      int_rows[2] <-
        dtf_long$IDN.INT.Row[lgc_add][
          lst_fastLink$matches[m, 2]
        ]

      dtf_long$LNK.CHR.Rows[int_rows] <- paste0(
        dtf_long$LNK.CHR.Rows[int_rows],
        int_rows[1], ',', int_rows[2], ';'
      )
      dtf_long[[ chr_prob ]][int_rows] <-
        lst_fastLink$posterior[m]

      # Close 'Loop over matches'
    }

    # Update progress bar
    if (lgc_progress_bar) setTxtProgressBar(obj_pb, s)

    # Close 'Loop over sets'
  }
  if (lgc_progress_bar) close(obj_pb)

  return( dtf_long )
}

#### 2) swaap_link ####
#' Link Records Across Time Points
#'
#' Function to link records (e.g., across different time points)
#' using a set of linking items.
#'
#' @param dtf_long A data frame, assumed to be standard processed
#'   school-wide assessment data with the columns
#'   \code{'SSS.INT.TimePoint'} and \code{'SSS.INT.LongitudinalWave'}
#'   as well as the linking code items.
#' @param chr_method A character string, either
#'   \code{'dissimilarity'} (to link using dissimilarity scores)
#'   or \code{'group_by'} (to link using simpler method based
#'   on [dplyr::group_by]).
#' @param lgc_remove_duplicates A logical value; if \code{TRUE}
#'   function attempts to identify and trim duplicate records
#'   within a time point.
#' @param lst_sets A list of lists, with each sublist specifying
#'   \code{'Base'} and \code{'Add'} logical vectors for the pair of data
#'   subsets in \code{dtf_long} to link over (e.g., \code{'Base'} would
#'   subset the first time point and \code{'Add'} would subset the second
#'   time point).
#' @param obj_items Either a character vector with the column
#'   names for the linking items, or a list of character vectors,
#'   one vector for each set defined in \code{lst_sets}.
#'   Passing a list with separate vectors allows using different
#'   linking items for different sets when necessary. If
#'   \code{NULL} attempts to identify linking items based on
#'   standardized names.
#' @param lst_combos A list of lists. For \code{chr_method = 'dissimilarity'}
#'   each sublist consists of an integer vector indexing the combination
#'   of linking items to consider in order of priority. One sublist of
#'   integer vectors must be defined for each set defined by
#'   \code{lst_sets}. For a given sublist, indices apply to the
#'   character vector defined for the relevant set in \code{obj_items},
#'   meaning that if character vectors differ across sets, indices
#'   should be defined accordingly.
#'   For \code{chr_method = 'fastLink'} each sublist consists of
#'   integer indices for the items to pass to the \code{stringdist},
#'   \code{numeric}, and \code{partial} arguments for [fastLink::fastLink].
#' @param lst_missing A list of lists, where each sublist consists of
#'   an integer vector indicating which items should be checked for
#'   missingness when linking using a given combo (therefore
#'   \code{lst_missing} should match in structure to \code{lst_combos}).
#'   When specifying additional items beyond those listing in the given
#'   combo, ensures that if these extra items are non-missing for a
#'   record, the record will not be linked. This behavior can be
#'   suppressed by supplying \code{c()} instead of an integer vector,
#'   which in turn allows for matches with a dissimilarity score
#'   greater than 0. Only applicable for \code{chr_method = 'dissimilarity'}.
#' @param fun_trim_duplicates An optional function that returns
#'   a logical vector for the subset of duplicate records
#'   indicating which records should be kept (return \code{TRUE}).
#'   Default is to select record with the highest completion rate.
#' @param lgc_duplicates A logical value; if \code{TRUE} when
#'   generating default values for \code{lst_sets} does so to
#'   check for duplicate records within a time point.
#' @param lgc_district A logical value; if \code{TRUE} when
#'   generating default values for \code{obj_items} uses
#'   district codes rather than school codes.
#' @param chr_progress A character string, either \code{'bar'}
#'   (progress bar to track function completion), \code{'section'}
#'   (lists completed sections to track function completion), or
#'   \code{''} to not track progress.
#'
#' @author Michael Pascale; Kevin Potter
#'
#' @returns A data frame.
#'
#' @examples
#' # Generate demonstration data
#' dtf_long <- swaap_simulate( 'link', 'demo' )
#' # Record linkage
#' dtf_linked <- swaap_link( dtf_long )
#'
#' @export

swaap_link <- function(
    dtf_long,
    chr_method = 'dissimilarity',
    lgc_remove_duplicates = FALSE,
    lst_sets = NULL,
    obj_items = NULL,
    lst_combos = NULL,
    lst_missing = NULL,
    fun_trim_duplicates = NULL,
    lst_fastLink_args = list( threshold.match = .9, chr_exact = '' ),
    lgc_duplicates = FALSE,
    lgc_district = FALSE,
    chr_progress = 'bar' ) {

  if ( chr_progress != '' ) message( 'Start: swaap_link' )

  lgc_progress <- FALSE
  lgc_progress_bar <- chr_progress != ''

  chr_progress_labels <- c(
    'label', 'labels',
    'section', 'sections'
  )

  # Track progress by section labels
  if ( chr_progress %in% chr_progress_labels ) {

    lgc_progress <- TRUE
    lgc_progress_bar <- FALSE

    # Close 'Track progress by section labels'
  }

  dtt_start <- Sys.time()

  #### 2.1) Setup ####
  if (lgc_progress) message( '  Setup' )

  lgc_fastLink <- 'fastLink' %in% chr_method

  # Check list of sets
  lst_sets <- swaap::swaap_link.input.sets(
    dtf_long = dtf_long,
    lst_sets = lst_sets,
    lgc_duplicates = lgc_duplicates
  )
  # Confirm if checking for duplicates
  lgc_duplicates <- all( lst_sets[[1]]$Base == lst_sets[[1]]$Add )

  # Check linking items
  lst_items <- swaap::swaap_link.input.items(
    dtf_long = dtf_long,
    lst_sets = lst_sets,
    obj_items = obj_items,
    lgc_fastLink = lgc_fastLink,
    lgc_district = lgc_district
  )

  # Additional inputs for dissimilarity method
  if ( chr_method %in% 'dissimilarity' ) {

    # Check combos
    lst_combos <- swaap::swaap_link.input.combos(
      dtf_long = dtf_long,
      lst_items = lst_items,
      lst_combos = lst_combos
    )

    # Check missing
    lst_missing <- swaap::swaap_link.input.missing(
      dtf_long = dtf_long,
      lst_items = lst_items,
      lst_combos = lst_combos,
      lst_missing = lst_missing
    )

    # Close 'Additional inputs for dissimilarity method'
  }

  # Additional inputs for fastLink method
  if ( chr_method %in% 'fastLink' ) {

    # Check combos
    lst_combos <- swaap::swaap_link.input.combos(
      dtf_long = dtf_long,
      lst_items = lst_items,
      lst_combos = lst_combos,
      lgc_fastLink = TRUE
    )

    # Ensure numeric inputs
    chr_items <- lst_items |> unlist() |> unique()
    chr_numeric <- chr_items[
      substr(chr_items, 5, 7 ) %in% c( 'INT', 'DBL' )
    ]

    # Any numeric items
    if ( length(chr_numeric) > 0 ) {

      # Loop over numeric items
      for ( v in seq_along(chr_numeric) ) {

        dtf_long[[ chr_numeric[v] ]] <- as.numeric(
          dtf_long[[ chr_numeric[v] ]]
        )

        # Close 'Loop over numeric items'
      }

      # Close 'Any numeric items'
    }

    # Close 'Additional inputs for fastLink method'
  }

  #### 2.1.1) fun_copy_prior ####
  fun_copy_prior <- function(
    dtf_long,
    chr_new ) {

    chr_col <- colnames(dtf_long)

    # New column already exists
    if ( chr_new %in% chr_col ) {

      # Determine prior versions using
      # column 'IDN.INT.Row' as reference
      lgc_prior <- grepl(
        'IDN.INT.Row.V',
        chr_col,
        fixed = TRUE
      )

      int_add <- 0
      if ( sum(lgc_prior) == 0 ) int_add <- 1

      int_ver <- sum(lgc_prior) + int_add

      # Copy column
      chr_old <- paste0( chr_new, '.V', int_ver )

      dtf_long[[ chr_old ]] <- dtf_long[[ chr_new ]]

      # Close 'New column already exists'
    }

    return( dtf_long )
  }

  # Copy columns from any prior linking

  # Copy old row index
  dtf_long <- dtf_long |> fun_copy_prior(
    'IDN.INT.Row'
  )
  # Copy old ID column
  dtf_long <- dtf_long |> fun_copy_prior(
    'IDN.CHR.Linked.ID'
  )
  # Copy old attempt indicator
  dtf_long <- dtf_long |> fun_copy_prior(
    'LNK.LGC.Attempted'
  )
  # Copy old link indicators
  dtf_long <- dtf_long |> fun_copy_prior(
    'LNK.LGC.NoIssues'
  )
  # Copy old duplicate indicator
  dtf_long <- dtf_long |> fun_copy_prior(
    'LNK.LGC.Duplicates'
  )
  # Copy old method
  dtf_long <- dtf_long |> fun_copy_prior(
    'LNK.CHR.Method'
  )
  # Copy old linked rows
  dtf_long <- dtf_long |> fun_copy_prior(
    'LNK.CHR.Rows'
  )
  # Copy old time point patterns
  dtf_long <- dtf_long |> fun_copy_prior(
    'LNK.CHR.TimePoints'
  )
  # Copy old attributes
  dtf_long <- dtf_long |> fun_copy_prior(
    'LNK.CHR.AttributeWithParameters'
  )

  #### 2.2) Link records ####
  if (lgc_progress) message( '  Linking' )

  # Initialize row index
  dtf_long$IDN.INT.Row <- 1:nrow(dtf_long)
  # Initialize ID column for linking
  dtf_long <- swaap::swaap_add.ID(
    dtf_long,
    'Linked'
  )
  # Track original IDs
  chr_old_ID <- dtf_long$IDN.CHR.Linked.ID
  # Indicator for whether linking attempted
  dtf_long$LNK.LGC.Attempted <- FALSE
  # Type of link method
  dtf_long$LNK.CHR.Method <- ''
  dtf_long$LNK.CHR.Rows <- ''
  dtf_long$LNK.LGC.Preliminary <- FALSE
  dtf_long$LNK.LGC.Duplicates <- FALSE
  dtf_long$LNK.LGC.NoIssues <- FALSE
  dtf_long$LNK.CHR.AttributeWithParameters <-
    'attributes( <data>$LNK.CHR.AttributeWithParameters )'

  # Link using dissimilarity scores
  if ( chr_method == 'dissimilarity' ) {

    dtf_long <- dtf_long |>
      swaap:::swaap_link.internal.via_dissimilarity(
        lst_sets = lst_sets,
        lst_items = lst_items,
        lst_combos = lst_combos,
        lst_missing = lst_missing,
        lgc_progress = lgc_progress,
        lgc_progress_bar = lgc_progress_bar
      )

    # Close 'Link using dissimilarity scores'
  }

  # Link using group_by method
  if ( chr_method == 'group_by' ) {

    dtf_long <- dtf_long |>
      swaap:::swaap_link.internal.via_group_by(
        lst_sets = lst_sets,
        lst_items = lst_items,
        lgc_progress = lgc_progress,
        lgc_progress_bar = lgc_progress_bar
      )

    # Close 'Link using group_by method'
  }

  # Link using fastLink
  if ( chr_method == 'fastLink' ) {

    # Initialize exact match argument
    chr_exact <- NULL

    # Check if any fastLink arguments
    if ( !is.null(lst_fastLink_args) ) {

      # Check for exact match arguments
      if ( 'chr_exact' %in% names(lst_fastLink_args) ) {

        chr_exact <- lst_fastLink_args$chr_exact

        # Update argument list
        if ( length( lst_fastLink_args ) == 1 ) {

          lst_fastLink_args <- NULL

          # Close 'Update argument list'
        } else {

          lst_fastLink_args$chr_exact <- NULL

          # Close else for 'Update argument list'
        }

        # Close 'Check for exact match arguments'
      }

      # Close 'Check if any fastLink arguments'
    }

    dtf_long <- dtf_long |>
      swaap:::swaap_link.internal.via_fastLink(
        lst_sets = lst_sets,
        lst_items = lst_items,
        lst_combos = lst_combos,
        lst_fastLink_args = lst_fastLink_args,
        lgc_progress = lgc_progress,
        lgc_progress_bar = lgc_progress_bar
      )

    # Close 'Link using fastLink'
  }

  #### 2.3) Assign IDs ####
  if (lgc_progress) message( '  Assigning IDs' )

  dtf_long <- dtf_long |>
    swaap:::swaap_link.internal.assign_IDs(
      lgc_progress = lgc_progress,
      lgc_progress_bar = lgc_progress_bar
    )

  # Update data frame to have link patterns
  dtf_long <- dtf_long |>
    swaap::swaap_link.report.by_ID(
      lgc_update = TRUE
    )
  # Remove intermediary variable
  dtf_long$LNK.LGC.Preliminary <- NULL

  # Additional checking of duplicates
  chr_check <- dtf_long$LNK.CHR.TimePoints[
    dtf_long$LNK.LGC.NoIssues
  ] |> unique()
  int_dup <- sapply( chr_check, function(s) {
    chr_unq <- strsplit( s, '-', fixed = TRUE )[[1]]
    return(
      length(chr_unq) - dplyr::n_distinct(chr_unq)
    )
  } )

  # Fix duplicates
  if ( any(int_dup > 0) ) {

    dtf_long$LNK.LGC.NoIssues[
      dtf_long$IDN.CHR.Linked.ID %in% dtf_long$IDN.CHR.Linked.ID[
        dtf_long$LNK.CHR.TimePoints %in% chr_check[int_dup > 0]
      ]
    ] <- FALSE
    dtf_long$LNK.LGC.Duplicates[
      dtf_long$IDN.CHR.Linked.ID %in% dtf_long$IDN.CHR.Linked.ID[
        dtf_long$LNK.CHR.TimePoints %in% chr_check[int_dup > 0]
      ]
    ] <- TRUE

    # Close 'Fix duplicates'
  }

  # Fix broken matches
  if ( any( nchar( dtf_long$LNK.CHR.TimePoints ) == 1 &
            dtf_long$LNK.LGC.NoIssues ) ) {

    dtf_check <- dtf_long |>
      dplyr::filter(
        LNK.CHR.TimePoints %in% as.character(
          unique( dtf_long$SSS.INT.TimePoint )
        ) &
        dtf_long$LNK.LGC.NoIssues
      )

    # Loop over rows
    for ( r in 1:nrow(dtf_check) ) {

      lgc_matched <- grepl(
        dtf_check$LNK.CHR.Rows[r],
        dtf_long$LNK.CHR.Rows,
        fixed = TRUE
      )

      dtf_long$IDN.CHR.Linked.ID[lgc_matched] <-
        dtf_long$IDN.CHR.Linked.ID[
          lgc_matched &
          !dtf_long$IDN.CHR.Linked.ID %in% dtf_check$IDN.CHR.Linked.ID[r]
        ][1]

      dtf_long$LNK.LGC.NoIssues[lgc_matched] <- FALSE
      dtf_long$LNK.LGC.Duplicates[lgc_matched] <- TRUE

      # Close 'Loop over rows'
    }

    # Close 'Fix broken matches'
  }

  # Additional processing for fastLink method
  if ( chr_method %in% 'fastLink' ) {

    fun_equal <- function(
     vec_x ) {

      lgc_out <- FALSE

      # Any non-missing
      if ( any( !is.na(vec_x) ) ) {

        vec_x <- vec_x[!is.na(vec_x)]
        lgc_out <- all( vec_x %in% vec_x[1] )

        # Close 'Any non-missing'
      }

      return( lgc_out )
    }

    dtf_long$LNK.LGC.fastLinkFalsePositive <-
      rep( FALSE, nrow(dtf_long) )

    # Items that must have exact matches
    if ( !is.null(chr_exact) ) {

      # Default
      if ( chr_exact == '' ) {

        chr_exact <- c(
          'SBJ.INT.Link.DistrictCode',
          'SSS.INT.DistrictCode',
          'SBJ.INT.Link.SchoolCode',
          'SSS.INT.SchoolCode',
          'SBJ.CHR.Link.Sex',
          'SBJ.INT.Link.FL.Sex',
          'SBJ.CHR.Link.BirthYearMonth',
          'SBJ.CHR.Link.FL.BirthYearMonth'
        )

        # Close 'Default'
      }

      chr_items <- lst_items |> unlist() |> unique()

      chr_items <- chr_items[
        chr_items %in% chr_exact
      ]

      # If relevant items found
      if ( length(chr_items) > 0 ) {

        dtf_IDs <- dtf_long |>
          dplyr::filter(
            LNK.LGC.NoIssues
          ) |>
          dplyr::group_by(
            ID = IDN.CHR.Linked.ID
          ) |>
          dplyr::summarise_at(
            chr_items, fun_equal
          ) |>
          data.frame()

        # More than one variable
        if ( ncol(dtf_IDs) > 2 ) {

          lgc_FP <- rowSums( dtf_IDs[, -1] ) < ( ncol(dtf_IDs) - 1 )

          # Close 'More than one variable'
        } else {

          lgc_FP <- !dtf_IDs[[2]]

          # Close else for 'More than one variable'
        }

        dtf_long$LNK.LGC.fastLinkFalsePositive <-
          dtf_long$IDN.CHR.Linked.ID %in% dtf_IDs$ID[
            lgc_FP
          ]

        dtf_long$LNK.CHR.TimePoints[
          dtf_long$LNK.LGC.fastLinkFalsePositive
        ] <- as.character(
          dtf_long$SSS.INT.TimePoint[
            dtf_long$LNK.LGC.fastLinkFalsePositive
          ]
        )

        dtf_long$LNK.LGC.NoIssues[
          dtf_long$LNK.LGC.fastLinkFalsePositive
        ] <- FALSE

        dtf_long$IDN.CHR.Linked.ID[
          dtf_long$LNK.LGC.fastLinkFalsePositive
        ] <- chr_old_ID[
          dtf_long$LNK.LGC.fastLinkFalsePositive
        ]

        # Close 'If relevant items found'
      }

      # Close 'Items that must have exact matches'
    }

    # Close 'Additional processing for fastLink method'
  }

  #### 2.4) Final trimming of duplicates ####

  # If specified
  if ( lgc_remove_duplicates ) {

    if (lgc_progress) message( '  Trim duplicates' )

    if ( is.null(fun_trim_duplicates) )
      fun_trim_duplicates <- swaap::swaap_link.trim_rule(
        'duplicate time points'
      )

    lgc_update <- FALSE

    # Check if column already exists
    if ( 'QLT.LGC.RemoveDuplicate' %in% colnames(dtf_long) ) {

      # Copy column
      lgc_remove <- dtf_long$QLT.LGC.RemoveDuplicate
      lgc_update <- TRUE

      # Close 'Check if column already exists'
    }

    dtf_long <- dtf_long |>
      swaap::swaap_link.trim(
        fun_rule = fun_trim_duplicates
      )

    # Incorporate previous duplicate info
    if ( lgc_update ) {

      # Any duplicates
      if ( any(dtf_long$QLT.LGC.RemoveDuplicate) ) {

        dtf_long$QLT.LGC.RemoveDuplicate[
          lgc_remove %in% TRUE
        ] <- TRUE
        dtf_remove <- lgc_remove |> swaap::swaap_data.attr()
        dtf_remove_new <-
          dtf_long$QLT.LGC.RemoveDuplicate |>
          swaap::swaap_data.attr()

        # If possible combine as is
        if ( !any( dtf_remove_new$Pattern %in% dtf_remove$Pattern ) ) {

          dtf_remove <- rbind(
            dtf_remove,
            dtf_remove_new
          )

          # Close 'If possible combine as is'
        } else {

          # Loop over rows
          for ( r in 1:nrow(dtf_remove_new) ) {

            # Sum
            if ( dtf_remove_new$Pattern[r] %in% dtf_remove$Pattern ) {

              dtf_remove[
                dtf_remove$Pattern %in% dtf_remove_new$Pattern[r],
                -1
              ] <- dtf_remove[
                dtf_remove$Pattern %in% dtf_remove_new$Pattern[r],
                -1
              ] + dtf_remove_new[r, -1]

              # Close 'Sum'
            } else {

              dtf_remove <- rbind(
                dtf_remove,
                dtf_remove_new[r, ]
              )

              # Close else for 'Sum'
            }

            # Close 'Loop over rows'
          }

          # Close else for 'If possible combine as is'
        }

        # Close 'Any duplicates'
      } else {

        # Add prior data frame
        attributes(
          dtf_long$QLT.LGC.RemoveDuplicate
        ) <- attributes(
          lgc_remove
        )

        # Close else for 'Any duplicates'
      }

      # Close 'Incorporate previous duplicate info'
    }

    dtf_long <- dtf_long |>
      dplyr::filter(
        !QLT.LGC.RemoveDuplicate
      )

    # Close 'If specified'
  }

  # Track run time
  dtt_end <- Sys.time()

  attributes(dtf_long$LNK.CHR.AttributeWithParameters) <- list(
    swaap.inputs_for_linking = list(
      chr_method = chr_method,
      lst_sets = lst_sets,
      lst_items = lst_items,
      lst_combos = switch(
        chr_method,
        dissimilarity = lst_combos,
        group_by = NULL,
        fastLink = lst_combos
      ),
      lst_missing = switch(
        chr_method,
        dissimilarity = lst_combos,
        group_by = NULL,
        fastLink = lst_combos
      ),
      lst_fastLink_args = lst_fastLink_args,
      lst_time = list(
        start = dtt_start,
        end = dtt_end,
        duration = dtt_end - dtt_start
      )
    )
  )

  if ( chr_progress != '' ) message( '\n--End: swaap_link' )

  return( dtf_long )
}

#### 3) Input functions ####

#### 3.1) swaap_link.input.sets ####
#' Function to Define Sets of Records to Link
#'
#' Function to generate input to pass to the [swaap::swaap_link]
#' function. Creates a list of lists with logical vectors
#' indicating the rows to consider when linking two time points.
#' Can also be used to check that input is correctly specified.
#'
#' @param dtf_long A data frame, assumed to be standard processed
#'   school-wide assessment data. Default methods need the columns
#'   \code{'SSS.INT.LongitudinalWave'}, \code{'SSS.INT.TimePoint'} and
#'   \code{'SSS.INT.Grade'}.
#' @param lst_sets An optional argument, the list of sets. If provided,
#'   function checks if input is valid.
#' @param lgc_duplicates A logical value; if \code{TRUE}, produces
#'   the list of lists with logical vectors for checking for duplicate
#'   records within a given time point.
#' @param chr_groups A character vector of three column names for the
#'   longitudinal wave, time point, and grade level, respectively.
#'
#' @author Kevin Potter
#'
#' @returns A list of lists, each sublist consisting of the elements
#' \code{Base} (a logical vector for rows from the first time point)
#' and \code{Add} (a logical vector for rows from the second time point),
#' If \code{lgc_duplicates = TRUE}, both logical vectors are for the
#' same time point.
#'
#' @examples
#' dtf_long <- swaap_simulate( 'link', 'demo' )
#'
#' lst_sets <- dtf_long |>
#'   swaap_link.input.sets()
#'
#' @export


swaap_link.input.sets <- function(
    dtf_long,
    lst_sets = NULL,
    lgc_duplicates = FALSE,
    chr_groups = NULL ) {

  # Create input
  if ( is.null(lst_sets) ) {

    # Check if only SBIRT sample
    lgc_SBIRT <- FALSE

    # SBIRT variables found
    if ( 'SSS.LGC.SBIRT' %in% colnames(dtf_long) ) {

      if ( all( dtf_long$SSS.LGC.SBIRT ) ) lgc_SBIRT <- TRUE

      # Close 'SBIRT variables found'
    }

    # Default grouping variables
    if ( is.null(chr_groups) ) {

      chr_groups <- c( 'SSS.INT.LongitudinalWave',
                       'SSS.INT.TimePoint',
                       'SSS.INT.Grade' )

      # Grouping variables for SBIRT
      if ( lgc_SBIRT ) {

        chr_groups <- c( 'SSS.INT.RecruitmentWave',
                         'SSS.INT.SBIRTTimePoint',
                         'SSS.INT.Grade' )

        # Try standard variable names
        if ( !all( chr_groups %in% colnames(dtf_long) ) ) {

          chr_groups <- c( 'SSS.INT.LongitudinalWave',
                           'SSS.INT.TimePoint',
                           'SSS.INT.Grade' )

          # Close 'Try standard variable names'
        }

        # Close 'Grouping variables for SBIRT'
      }

      # Close 'Default grouping variables'
    }

    chr_error <- paste0(
      "Argument 'chr_groups' must be column names for ",
      "longitudinal wave, time point, and grade level respectively"
    )

    # Check input
    if ( length(chr_groups) != 3 )
      stop( chr_error )
    if ( !all( chr_groups %in% colnames(dtf_long) ) )
      stop( chr_error )

    # Group by longitudinal wave, time point, and grade level
    chr_g <- chr_groups

    # Pairs of time points
    if ( !lgc_duplicates ) {

      #### 3.1.1) Default - SBIRT ####

      # If only SBIRT sample
      if ( lgc_SBIRT ) {

        int_WV <-
          dtf_long[[ chr_g[1] ]] |>
          unique() |>
          sort()

        mat_TM <- rbind(
          c( 0, 1 ),
          c( 0, 2 ),
          c( 0, 3 ),
          c( 0, 4 ),
          c( 1, 2 ),
          c( 1, 3 ),
          c( 1, 4 ),
          c( 2, 3 ),
          c( 2, 4 ),
          c( 3, 4 )
        )
        mat_GR <- rbind(
          c( 9, 9 ),
          c( 9, 10 ),
          c( 9, 10 ),
          c( 9, 11 ),
          c( 9, 10 ),
          c( 9, 10 ),
          c( 9, 11 ),
          c( 10, 10 ),
          c( 10, 11 ),
          c( 10, 11 )
        )

        lst_sets <- list()
        chr_names <- c()
        int_inc <- 1

        # Loop over waves
        for ( w in seq_along(int_WV) ) {

          # Loop over time points
          for ( p in 1:nrow(mat_TM) ) {

            # Loop over starting grades
            for ( g in 0:1 ) {

              # Create subsets
              lgc_base <-
                dtf_long[[ chr_g[1] ]] %in% int_WV[w] &
                dtf_long[[ chr_g[2] ]] %in% mat_TM[p, 1] &
                dtf_long[[ chr_g[3] ]] %in% ( mat_GR[p, 1] + g )
              lgc_add <-
                dtf_long[[ chr_g[1] ]] %in% int_WV[w] &
                dtf_long[[ chr_g[2] ]] %in% mat_TM[p, 2] &
                dtf_long[[ chr_g[3] ]] %in% ( mat_GR[p, 2] + g )

              # If subsets exist
              if ( any(lgc_base) & any(lgc_add) ) {

                lst_sets[[int_inc]] <- list(
                  Base = lgc_base,
                  Add = lgc_add
                )
                chr_names[int_inc] <- paste0(
                  'W', int_WV[w],
                  'T', mat_TM[p, 1],
                  'G', mat_GR[p, 1] + g,
                  't',
                  'W', int_WV[w],
                  'T', mat_TM[p, 2],
                  'G', mat_GR[p, 2] + g
                )

                int_inc <- int_inc + 1

                # Close 'If subsets exist'
              }

              # Close 'Loop over starting grades'
            }

            # Close 'Loop over time points'
          }

          # Close 'Loop over waves'
        }

        names( lst_sets ) <- chr_names

        return( lst_sets )

        # Close 'If only SBIRT sample'
      }

      #### 3.1.2) Default - survey ####

      int_waves <-
        dtf_long[[ chr_g[1] ]] |>
        unique() |>
        sort()
      int_times <-
        dtf_long[[ chr_g[2] ]] |>
        unique() |>
        sort()

      mat_pairs <- cbind(
        lapply(
          seq_along(int_times)[-1], function(j) {

            rep( int_times[j-1], length(int_times[-(1:(j-1))]) )

          }
        ) |> unlist(),
        lapply(
          seq_along(int_times)[-1], function(j) {

            int_times[-(1:(j-1))]

          }
        ) |> unlist()
      )

      lst_sets <- list()
      chr_names <- c()
      int_inc <- 1

      # Loop over waves
      for ( w in seq_along(int_waves) ) {

        # Loop over possible pairs
        for ( p in 1:nrow(mat_pairs) ) {

          # Create subsets
          lgc_base <-
            dtf_long[[ chr_g[2] ]] %in% mat_pairs[p, 1] &
            dtf_long[[ chr_g[1] ]] %in% int_waves[w]
          lgc_add <-
            dtf_long[[ chr_g[2] ]] %in% mat_pairs[p, 2] &
            dtf_long[[ chr_g[1] ]] %in% int_waves[w]

          # Add set
          if ( any(lgc_base) & any(lgc_add) ) {

            lst_sets[[int_inc]] <- list(
              Base = lgc_base,
              Add = lgc_add
            )
            chr_names[int_inc] <- paste0(
              'W', int_waves[w],
              'T', mat_pairs[p, 1],
              't',
              'W', int_waves[w],
              'T', mat_pairs[p, 2]
            )

            int_inc <- int_inc + 1

            # Close 'Add set'
          }

          # Close 'Loop over possible pairs'
        }

        # Close 'Loop over waves'
      }

      names( lst_sets ) <- chr_names

      return( lst_sets )

      # Close 'Pairs of time points'
    } else {

      #### 3.1.3) Duplicates - SBIRT ####

      # Check if only SBIRT sample
      lgc_SBIRT <- FALSE

      # SBIRT variables found
      if ( 'SSS.LGC.SBIRT' %in% colnames(dtf_long) ) {

        if ( all( dtf_long$SSS.LGC.SBIRT ) ) lgc_SBIRT <- TRUE

        # Close 'SBIRT variables found'
      }

      # If only SBIRT sample
      if ( lgc_SBIRT ) {

        # Group by recruitment wave, time point, and grade level

        int_WV <-
          dtf_long[[ chr_g[1] ]] |>
          unique() |>
          sort()

        mat_sets <- rbind(
          c( 0, 9 ),
          c( 1, 9 ),
          c( 2, 10 ),
          c( 3, 10 ),
          c( 4, 11 )
        )

        lst_sets <- list()
        chr_names <- c()
        int_inc <- 1

        # Loop over waves
        for ( w in seq_along(int_WV) ) {

          # Loop over time points
          for ( p in 1:nrow(mat_sets) ) {

            # Loop over starting grades
            for ( g in 0:1 ) {

              # Create subsets
              lgc_base <-
                dtf_long[[ chr_g[1] ]] %in% int_WV[w] &
                dtf_long[[ chr_g[2] ]] %in% mat_sets[p, 1] &
                dtf_long[[ chr_g[3] ]] %in% ( mat_sets[p, 2] + g )
              lgc_add <-
                dtf_long[[ chr_g[1] ]] %in% int_WV[w] &
                dtf_long[[ chr_g[2] ]] %in% mat_sets[p, 1] &
                dtf_long[[ chr_g[3] ]] %in% ( mat_sets[p, 2] + g )

              # If subsets exist
              if ( any(lgc_base) & any(lgc_add) ) {

                lst_sets[[int_inc]] <- list(
                  Base = lgc_base,
                  Add = lgc_add
                )
                chr_names[int_inc] <- paste0(
                  'W', int_WV[w],
                  'T', mat_sets[p, 1],
                  'G', mat_sets[p, 2] + g
                )

                int_inc <- int_inc + 1

                # Close 'If subsets exist'
              }

              # Close 'Loop over starting grades'
            }

            # Close 'Loop over time points'
          }

          # Close 'Loop over waves'
        }

        names( lst_sets ) <- chr_names

        return( lst_sets )

        # Close 'If only SBIRT sample'
      }

      #### 3.1.4) Duplicates - survey ####

      int_waves <-
        dtf_long[[ chr_g[1] ]] |>
        unique() |>
        sort()
      int_times <-
        dtf_long[[ chr_g[2] ]] |>
        unique() |>
        sort()

      lst_sets <- list()
      chr_names <- c()
      int_inc <- 1

      # Loop over waves
      for ( w in seq_along(int_waves) ) {

        # Loop over time points
        for ( p in seq_along(int_times) ) {

          # Create subsets
          lgc_base <-
            dtf_long[[ chr_g[2] ]] %in% int_times[p] &
            dtf_long[[ chr_g[1] ]] %in% int_waves[w]
          lgc_add <-
            dtf_long[[ chr_g[2] ]] %in% int_times[p] &
            dtf_long[[ chr_g[1] ]] %in% int_waves[w]

          # Add set
          if ( any(lgc_base) & any(lgc_add) ) {

            lst_sets[[int_inc]] <- list(
              Base = lgc_base,
              Add = lgc_add
            )
            chr_names[int_inc] <- paste0(
              'W', int_waves[w],
              'T', int_times[p]
            )

            int_inc <- int_inc + 1

            # Close 'Add set'
          }

          # Close 'Loop over time points'
        }

        # Close 'Loop over waves'
      }

      names( lst_sets ) <- chr_names

      # Close else for 'Pairs of time points'
    }

    # Close 'Create input'
  }

  #### 3.1.5) Check input ####

  chr_error <-
    paste0(
      "\nArgument 'lst_sets' must be in format:\n",
      "list(\n",
      "  <Set> = list(\n",
      "    Base = <logical vector>,\n",
      "    Add = <logical vector>\n",
      "  ),\n",
      "  ...\n",
      ")\n",
      "\n",
      "Logical vectors for 'Base' and 'Add' specify ",
      "subset of rows to consider for the pair of ",
      "time points to link"
    )

  # Make sure is list of lists
  if ( !is.list(lst_sets) ) stop(chr_error)

  # Check input validity
  lgc_checks_rows <- rep( FALSE, length(lst_sets) )

  # Loop over sets
  for ( s in seq_along(lst_sets) ) {

    # Sublist not a list
    if ( !is.list(lst_sets[[s]] ) ) stop(chr_error)

    # Sublist has wrong names
    if ( !all( c( 'Base', 'Add' ) %in% names( lst_sets[[s]] ) ) )
      stop(chr_error)

    lgc_checks_rows[s] <-
      is.logical( lst_sets[[s]]$Base ) &
      is.logical( lst_sets[[s]]$Add )

    lgc_checks_rows[s] <-
      length( lst_sets[[s]]$Base ) == nrow(dtf_long) &
      length( lst_sets[[s]]$Add ) == nrow(dtf_long)

    # Close 'Loop over sets'
  }

  if ( !all(lgc_checks_rows) )
    stop( 'Check that logical vectors for sets are for current data' )

  return( lst_sets )
}

#### 3.2) swaap_link.input.items ####
#' Function to Define Items to Link Over
#'
#' Function to generate input to pass to the [swaap::swaap_link]
#' function. Creates a list of the column names for the
#' items to link over per each pair of time points.
#'
#' @param dtf_long A data frame, assumed to be standard processed
#'   school-wide assessment data. If column \code{'SSS.INT.Grade'}
#'   is present function will try to check for instances in which
#'   school code does not apply (i.e., transition from middle to
#'   high school).
#' @param lst_sets A list of lists, each sublist consisting of the
#'   elements \code{Base} (a logical vector for rows from the first
#'   time point) and \code{Add} (a logical vector for rows from the
#'   second time point). See output from [swaap::swaap_link.input.sets].
#' @param obj_items An optional argument, either a character vector
#'   of column names or a list of character vectors. If a character
#'   vector, function converts into a list using \code{lst_sets}.
#'   If a list, function checks if input is valid.
#' @param lgc_fastLink A logical value; if \code{TRUE}, will
#'   return linking items intended for use with the
#'   [fastLink::fastLink] function when generating input.
#' @param lgc_district A logical value; if \code{TRUE} uses
#'   district codes instead of school codes when generating input.
#'
#' @returns A list of character vectors, the column names for the
#' items to link over per each pair of time points.
#'
#' @examples
#' dtf_long <- swaap_simulate( 'link', 'demo' )
#'
#' lst_sets <- dtf_long |>
#'   swaap_link.input.sets()
#' lst_items <- dtf_long |>
#'   swaap_link.input.items( lst_sets )
#'
#' @export

swaap_link.input.items <- function(
    dtf_long,
    lst_sets,
    obj_items = NULL,
    lgc_fastLink = FALSE,
    lgc_district = FALSE ) {

  # No items provided
  if ( is.null(obj_items) ) {

    # Exact matching
    if ( !lgc_fastLink ) {

      # Default items for linking
      lst_defaults <- list(
        standard = swaap::swaap_select.linking(
          lgc_district = lgc_district
        ),
        original = swaap::swaap_select.linking(
          lgc_district = lgc_district,
          lgc_original = TRUE
        )
      )

      # Close 'Exact matching'
    } else {

      # Default items for linking
      lst_defaults <- list(
        standard = swaap::swaap_select.linking(
          lgc_district = lgc_district,
          lgc_fastLink = TRUE
        ),
        original = swaap::swaap_select.linking(
          lgc_district = lgc_district,
          lgc_original = TRUE, lgc_fastLink = TRUE
        )
      )

      # Close else for 'Exact matching'
    }

    # Standard items found
    if ( all( lst_defaults$standard %in% colnames(dtf_long) ) ) {

      obj_items <- lst_defaults$standard

      # Close 'Standard items found'
    } else {

      if ( all( lst_defaults$original %in% colnames(dtf_long) ) )
        obj_items <- lst_defaults$original

      # Close else for 'Standard items found'
    }

    if ( is.null( obj_items ) )
      stop( "No standard linking items found in data set" )

    # Close 'No items provided'
  }

  # Convert to list
  if ( is.character(obj_items) ) {

    obj_items <- lapply(
      seq_along(lst_sets), function(s) {

        return( obj_items )

      }
    )
    names( obj_items ) <- names( lst_sets )

    # Close 'Convert to list'
  }

  chr_error <- paste0(
    "Argument 'obj_items' must be a list of character vectors ",
    "indicating the column names for variables to link over per ",
    "each pair of time points; Element names must match element ",
    "names for the list 'lst_sets' as well"
  )

  # Check input
  if ( !is.list(obj_items) )
    stop( chr_error )

  if ( length(obj_items) != length(lst_sets) )
    stop( chr_error )

  if ( !all( names(obj_items) == names(lst_sets) ) )
    stop( chr_error )

  lgc_check_grade <- 'SSS.INT.Grade' %in% colnames(dtf_long)

  # Loop through sets
  for ( s in seq_along(lst_sets) ) {

    lgc_base <-
      lst_sets[[s]]$Base
    lgc_add <-
      lst_sets[[s]]$Add

    lgc_base_NA <- apply(
      dtf_long[lgc_base, obj_items[[s]]],
      2, function(x) all( is.na(x) )
    )
    lgc_add_NA <- apply(
      dtf_long[lgc_add, obj_items[[s]]],
      2, function(x) all( is.na(x) )
    )

    # If any variables are NA for all cases
    if ( any(lgc_base_NA) | any(lgc_add_NA) ) {

      chr_remove <- obj_items[[s]][
        lgc_base_NA | lgc_add_NA
      ]

      # Keep items with non-NA cases
      obj_items[[s]] <- obj_items[[s]][
        !lgc_base_NA & !lgc_add_NA
      ]

      chr_warning <- paste0(
        "For set ", names(lst_sets)[s], " removed ",
        "following item(s) due to NA values: ",
        paste( chr_remove, collapse = ", " )
      )

      warning( chr_warning )

      # Close 'If any variables are NA for all cases'
    }

    # If variable is constant for fastLink
    if ( lgc_fastLink ) {

      lgc_base_constant <- apply(
        dtf_long[lgc_base, obj_items[[s]]],
        2, function(x) dplyr::n_distinct(x) == 1
      )
      lgc_add_constant <- apply(
        dtf_long[lgc_add, obj_items[[s]]],
        2, function(x) dplyr::n_distinct(x) == 1
      )

      # If any variables are constant
      if ( any(lgc_base_constant) | any(lgc_add_constant) ) {

        chr_remove <- obj_items[[s]][
          lgc_base_constant | lgc_base_constant
        ]

        # Keep items with non-NA cases
        obj_items[[s]] <- obj_items[[s]][
          !lgc_base_constant & !lgc_base_constant
        ]

        chr_warning <- paste0(
          "For set ", names(lst_sets)[s], " removed ",
          "following item(s) due to being constant: ",
          paste( chr_remove, collapse = ", " )
        )

        warning( chr_warning )

        # Close 'If any variables are constant'
      }

      # Close 'If variable is constant for fastLink'
    }

    # Close 'Loop through sets'
  }

  return( obj_items )
}

#### 3.3) swaap_link.input.combos ####
#' Function to Define Combinations of Items to Link Over
#'
#' Function to generate input to pass to the [swaap::swaap_link]
#' function. Creates a list of lists, either the integer indices
#' for the combinations of linking items to consider for
#' exact matching approaches, or the integer indices for the
#' items to pass to pass to the \code{stringdist}, \code{numeric},
#' and \code{partial} arguments of the function [fastLink::fastLink].
#'
#' @param dtf_long A data frame, assumed to be standard processed
#'   school-wide assessment data.
#' @param lst_items A list of of character vectors, the columns to
#'   use as linking items for each set (the pair of time points to link).
#'   See output from [swaap::swaap_link.input.items].
#' @param lst_combos An optional argument. For exact matching methods,
#'   should be a list of lists, each sublist consisting of integer
#'   indices indicating the combination of linking items to use.
#'   For the [fastLink::fastLink] function, a list of lists, each
#'   sublist specifying integer indices indicating which items to
#'   pass to the \code{stringdist}, \code{numeric},
#'   and \code{partial} arguments of the function [fastLink::fastLink].
#'   If provided, function checks if input is valid.
#' @param lgc_fastLink A logical value; if \code{TRUE} specifies
#'   which items to pass to the \code{stringdist}, \code{numeric},
#'   and \code{partial} arguments of the function [fastLink::fastLink]
#'   when generating input.
#'
#' @returns A list of lists. For exact matching methods, each sublist
#' consists of integer indices indicating the combination of linking
#' items to use. For the [fastLink::fastLink] function, each sublist
#' specifies the integer indices to pass to the \code{stringdist},
#' \code{numeric}, and \code{partial} arguments.
#'
#' @examples
#' dtf_long <- swaap_simulate( 'link', 'demo' )
#'
#' lst_sets <- dtf_long |>
#'   swaap_link.input.sets()
#' lst_items <- dtf_long |>
#'   swaap_link.input.items( lst_sets )
#' lst_combos <- dtf_long |>
#'   swaap_link.input.combos( lst_items )
#'
#' @export

swaap_link.input.combos <- function(
    dtf_long,
    lst_items,
    lst_combos = NULL,
    lgc_fastLink = FALSE ) {

  # Exact matching
  if ( !lgc_fastLink ) {

    #### 3.3.1) Exact matching ####

    # No input
    if ( is.null(lst_combos) ) {

      # Initialize list
      lst_combos <- lapply(
        seq_along(lst_items), function(s) {
          list()
        }
      )
      names(lst_combos) <- names(lst_items)

      # Known defaults
      lst_defaults <- list(
        school = list(
          standard = swaap::swaap_select.linking(),
          original = swaap::swaap_select.linking( lgc_original = TRUE )
        ),
        district = list(
          standard = swaap::swaap_select.linking( lgc_district = TRUE ),
          original = swaap::swaap_select.linking(
            lgc_original = TRUE, lgc_district = TRUE
          )
        )
      )

      # Loop over sets
      for ( s in seq_along(lst_items) ) {

        chr_items <- lst_items[[s]]

        lgc_CD <-
          lst_defaults$school$standard[1] %in% chr_items |
          lst_defaults$school$original[1] %in% chr_items |
          lst_defaults$district$standard[1] %in% chr_items |
          lst_defaults$district$original[1] %in% chr_items
        lgc_SI <-
          lst_defaults$school$standard[2] %in% chr_items |
          lst_defaults$school$original[2] %in% chr_items
        lgc_LQ <-
          any( lst_defaults$school$standard[-(1:2)] %in% chr_items ) |
          any( lst_defaults$school$original[-(1:2)] %in% chr_items )

        # School ID provided
        if ( lgc_SI ) {

          int_slot <- length( lst_combos[[s]] ) + 1

          lst_combos[[s]][[ int_slot ]] <- which(
            chr_items %in% lst_defaults$school$standard[2] |
            chr_items %in% lst_defaults$school$original[2]
          )

          # Add in school code
          if ( lgc_CD ) {

            lst_combos[[s]][[ int_slot ]] <- c(
              which(
                chr_items %in% lst_defaults$school$standard[1] |
                chr_items %in% lst_defaults$school$original[1] |
                chr_items %in% lst_defaults$district$standard[1] |
                chr_items %in% lst_defaults$district$original[1]
              ),
              lst_combos[[s]][[ int_slot ]]
            )

            # Close 'Add in school code'
          }

          # Close 'School ID provided'
        }

        # Linking questions
        if ( lgc_LQ ) {

          mat_LQ <- cbind(
            lst_defaults$school$standard[-(1:2)],
            lst_defaults$school$original[-(1:2)]
          )

          int_items <- sapply(
            1:nrow(mat_LQ), function(r) {

              int_out <- NA

              # Item found
              if ( any( mat_LQ[r, ] %in% chr_items ) ) {

                int_out <- which( chr_items %in% mat_LQ[r, ] )

                # Close 'Item found'
              }

              return( int_out )
            }
          )
          names(int_items) <- NULL

          # Increment slot
          int_slot <- length( lst_combos[[s]] ) + 1

          lst_combos[[s]][[ int_slot ]] <- int_items

          # Add in school code
          if ( lgc_CD ) {

            lst_combos[[s]][[ int_slot ]] <- c(
              which(
                chr_items %in% lst_defaults$school$standard[1] |
                chr_items %in% lst_defaults$school$original[1] |
                chr_items %in% lst_defaults$district$standard[1] |
                chr_items %in% lst_defaults$district$original[1]
              ),
              lst_combos[[s]][[ int_slot ]]
            )

            # Close 'Add in school code'
          }

          lgc_I5 <-
            sum( chr_items %in% mat_LQ[, 1] ) > 5 |
            sum( chr_items %in% mat_LQ[, 2] ) > 5

          # If number of items is 5 or more
          if ( lgc_I5 ) {

            # Loop over items
            for ( i in seq_along(int_items) ) {

              # Increment slot
              int_slot <- length( lst_combos[[s]] ) + 1

              lst_combos[[s]][[ int_slot ]] <- int_items[-i]

              # Add in school code
              if ( lgc_CD ) {

                lst_combos[[s]][[ int_slot ]] <- c(
                  which(
                    chr_items %in% lst_defaults$school$standard[1] |
                    chr_items %in% lst_defaults$school$original[1] |
                    chr_items %in% lst_defaults$district$standard[1] |
                    chr_items %in% lst_defaults$district$original[1]
                  ),
                  lst_combos[[s]][[ int_slot ]]
                )

                # Close 'Add in school code'
              }

              # Close 'Loop over items'
            }

            # Close 'If number of items is 5 or more'
          }

          # Close 'Linking questions'
        } else {

          # If no combos added
          if ( length(lst_combos[[s]]) == 0 )
            lst_combos[[s]][[1]] <- seq_along(chr_items)

          int_items <- unique( unlist( lst_combos[[s]] ) )
          # If more items then current combos
          if ( length(chr_items[int_items]) < length(chr_items) ) {

            int_slot <- length( lst_combos[[s]] ) + 1

            lst_combos[[s]][[ int_slot ]] <- which(
              !chr_items %in% chr_items[int_items]
            )

            # Close 'If more items then current combos'
          }

          # Close else for 'Linking questions'
        }

        names( lst_combos[[s]] ) <-
          paste0( 'C', seq_along(lst_combos[[s]] ) )

        # Close 'Loop over sets'
      }

      # Close 'No input'
    }

    # Check values

    chr_error <-
      paste0(
        "\nArgument 'lst_combos' must be a list of ",
        "lists (one sublist for each set) consisting of ",
        "integer vectors indicating combinations of ",
        "linking items to match over, in the format:\n",
        "list(\n",
        "  <Set> = list(\n",
        "    <Combo> = <item indices>,\n",
        "    ...\n",
        "  ),\n",
        "  ...\n",
        ")\n",
        "\n",
        "The order of combinations can be used to indicate ",
        "which combos of linking items to prioritize when ",
        "matching\n",
        "\n",
        "Number of elements in 'lst_combos' must match number ",
        "of elements in 'lst_items' and have the same name"
      )

    if ( is.null(lst_combos) )
      stop( chr_error )

    if ( !is.list(lst_combos) )
      stop( chr_error )

    if ( length(lst_combos) != length(lst_items) )
      stop( chr_error )

    if ( !all( names(lst_combos) == names(lst_items) ) )
      stop( chr_error )

    # Loop over sets
    for ( s in seq_along(lst_combos) ) {

      lgc_indices_match <-
        all( unique( unlist( lst_combos[[s]] ) ) %in% seq_along(lst_items[[s]] ) )

      # Confirm that indices correspond to items
      if ( !lgc_indices_match )
        stop( chr_error )

      # Close 'Loop over sets'
    }

    return( lst_combos )

    # Close 'Exact matching'
  } else {

    #### 3.3.2) fastLink ####

    # No input
    if ( is.null(lst_combos) ) {

      # Initialize slots
      lst_combos <- lapply(
        seq_along(lst_items), function(s) {
          list()
        }
      )
      names(lst_combos) <- names( lst_items )

      # Standard linking items
      chr_linking <- c(
        swaap::swaap_select.linking( lgc_district = TRUE )[1],
        swaap::swaap_select.linking( lgc_fastLink = TRUE )
      )
      # Standard contact info items
      chr_contact <- c(
        swaap::swaap_select.linking( lgc_district = TRUE )[1],
        swaap::swaap_select.linking()[1],
        swaap::swaap_select.contact()[1:4]
      )

      # Loop over slots
      for ( s in seq_along(lst_items) ) {

        chr_items <- lst_items[[s]]

        lgc_linking <- all( chr_items %in% chr_linking )

        # Standard linking items
        if ( lgc_linking ) {

          chr_data_type <- sapply(
            chr_items, function(i) {
              substr( i, start = 5, stop = 7 )
            }
          )
          lgc_not_char <- !chr_data_type %in% 'CHR'

          # Any numeric items
          if ( any(lgc_not_char) ) {

            lst_combos[[s]] <- list(
              stringdist = which(!lgc_not_char),
              numeric = which(lgc_not_char)
            )

            # Close 'Any numeric items'
          } else {

            lst_combos[[s]] <- list(
              stringdist = seq_along(chr_items)
            )

            # Close else for 'Any numeric items'
          }

          # Close 'Standard linking items'
        }

        lgc_contact <- all( chr_items %in% chr_contact )

        # Standard contact info items
        if ( lgc_contact ) {

          lst_combos[[s]] <- list()

          chr_stringdist <- c(
            'SBJ.CHR.Contact.Name',
            'SBJ.CHR.Contact.Email',
            'SBJ.CHR.Contact.DateOfBirth'
          )

          # Items matched on string distance
          if ( any( chr_stringdist %in% chr_items ) ) {

            lst_combos[[s]]$stringdist <- which(
              chr_items %in% chr_stringdist
            )

            # Close 'Items matched on string distance'
          }

          chr_numeric <- c(
            'SBJ.INT.Link.SchoolCode',
            'SBJ.CHR.Contact.Cellphone'
          )

          # Items matched on numeric distance
          if ( any( chr_numeric %in% chr_items ) ) {

            lst_combos[[s]]$numeric <- which(
              chr_items %in% chr_numeric
            )

            # Close 'Items matched on numeric distance'
          }

          chr_partial <- c(
            'SBJ.CHR.Contact.Name'
          )

          # Items acceptable for partial matching
          if ( any( chr_partial %in% chr_items ) ) {

            lst_combos[[s]]$partial <- which(
              chr_items %in% chr_partial
            )

            # Close 'Items acceptable for partial matching'
          }

          # Close 'Standard linking items'
        }

        # No matches
        if ( length(lst_combos[[s]]) == 0 ) {

          lst_combos[[s]] <- list(
            stringdist = seq_along(chr_items)
          )

          # Close 'No matches'
        }

        # Close 'Loop over slots'
      }

      # Close 'No input'
    }

    # Check values

    chr_error <-
      paste0(
        "\nArgument 'lst_combos' must be a list of ",
        "lists (one sublist for each set) consisting of ",
        "the elements 'stringdist', 'numeric', and 'partial' ",
        "in the format:\n",
        "list(\n",
        "  <Set> = list(\n",
        "    stringdist = <item indices>,\n",
        "    numeric = <item indices>,\n",
        "    partial = <item indices>\n",
        "  ),\n",
        "  ...\n",
        ")\n",
        "\n",
        "Items indexed by 'stringdist' will be matched ",
        "by string distance and items index by 'partial' ",
        "can have partial matches only\n",
        "\n",
        "Number of elements in 'lst_combos' must match number ",
        "of elements in 'lst_items' and have the same name"
      )

    if ( is.null(lst_combos) )
      stop( chr_error )

    if ( !is.list(lst_combos) )
      stop( chr_error )

    if ( length(lst_combos) != length(lst_items) )
      stop( chr_error )

    if ( !all( names(lst_combos) == names(lst_items) ) )
      stop( chr_error )

    # Loop over sets
    for ( s in seq_along(lst_combos) ) {

      lgc_names_correct <- all(
        names( lst_combos[[s]] ) %in% c( 'stringdist', 'numeric', 'partial' )
      )

      if ( !lgc_names_correct )
        stop( chr_error )

      lgc_indices_match <-
        all(
          unique( unlist( lst_combos[[s]] ) ) %in% seq_along(lst_items[[s]] )
        )

      # Confirm that indices correspond to items
      if ( !lgc_indices_match )
        stop( chr_error )

      # Close 'Loop over sets'
    }

    return( lst_combos )

    # Close else for 'Exact matching'
  }

}

#### 3.4) swaap_link.input.missing ####
#' Function to Define Missing Items to Ignore
#'
#' Function to generate input to pass to the [swaap::swaap_link]
#' function. Creates a list of lists, the integer indices
#' for the linking items that must be non-missing for linking
#' to be attempted with exact matching methods.
#'
#' @param dtf_long A data frame, assumed to be standard processed
#'   school-wide assessment data.
#' @param lst_items A list of of character vectors, the columns to
#'   use as linking items for each set (the pair of time points to link).
#'   See output from [swaap::swaap_link.input.items].
#' @param lst_combos A list of lists, each sublist consisting of integer
#'   indices indicating the combination of linking items to use.
#'   See output from [swaap::swaap_link.input.combos].
#' @param lst_missing An optional argument, a list of lists, each
#'   sublist consisting of integer indices for items that must be
#'   non-missing for linking to be attempted. If provided,
#'   function checks if input is valid.
#'
#' @returns A list of lists, each sublist consisting of integer indices
#' for items that must be non-missing for linking to be attempted.
#'
#' @examples
#' dtf_long <- swaap_simulate( 'link', 'demo' )
#'
#' lst_sets <- dtf_long |>
#'   swaap_link.input.sets()
#' lst_items <- dtf_long |>
#'   swaap_link.input.items( lst_sets )
#' lst_combos <- dtf_long |>
#'   swaap_link.input.combos( lst_items )
#' lst_missing <- dtf_long |>
#'   swaap_link.input.missing( lst_items, lst_comobs )
#'
#' @export

swaap_link.input.missing <- function(
    dtf_long,
    lst_items,
    lst_combos,
    lst_missing = NULL ) {

  # No input
  if ( is.null(lst_missing) ) {

    # Initialize list
    lst_missing <- lst_combos

    # Known defaults
    lst_defaults <- list(
      school = list(
        standard = swaap::swaap_select.linking(),
        original = swaap::swaap_select.linking( lgc_original = TRUE )
      ),
      district = list(
        standard = swaap::swaap_select.linking( lgc_district = TRUE ),
        original = swaap::swaap_select.linking(
          lgc_original = TRUE, lgc_district = TRUE
        )
      )
    )

    # Loop over sets
    for ( s in seq_along(lst_items) ) {

      chr_items <- lst_items[[s]]

      # Loop over combos
      for ( l in seq_along(lst_combos[[s]]) ) {

        int_missing <- seq_along(chr_items)

        chr_current <- chr_items[ lst_combos[[s]][[l]] ]

        # Check to see if there are at least 5 linking questions
        int_LQ <- sum(
          chr_current %in% c(
            lst_defaults$school$standard[-(1:2)],
            lst_defaults$school$original[-(1:2)]
          )
        )

        # Check to see if there is a school ID
        int_SI <- which(
          chr_items %in% c(
            lst_defaults$school$standard[2],
            lst_defaults$school$original[2]
          )
        )

        # School ID can be missing when using linking questions
        if ( int_LQ >= 5 & length(int_SI) > 0 ) {

          int_missing <- int_missing[
            -int_SI
          ]

          # Close 'School ID can be missing when using linking questions'
        }

        # If not using linking questions
        if ( int_LQ == 0 ) {

          int_missing <- seq_along(chr_current)

          # Close 'If not using linking questions'
        }

        lst_missing[[s]][[l]] <- int_missing

        # Close 'Loop over combos'
      }

      # Close 'Loop over sets'
    }

    # Close 'No input'
  }

  # Check values

  chr_error <-
    paste0(
      "\nArgument 'lst_missing' must be a list of ",
      "lists (one sublist for each set) consisting of ",
      "integer vectors indicating which linking items ",
      "to check for missingness per combo, in the format:\n",
      "list(\n",
      "  <Set> = list(\n",
      "    <Combo> = <item indices>,\n",
      "    ...\n",
      "  ),\n",
      "  ...\n",
      ")\n",
      "\n",
      "If any items for a given combo are found missing no ",
      "linking will be done for that record - this can be ",
      "suppressed by using c() instead"
    )

  # Make sure is list of lists
  if ( !is.list(lst_missing) ) stop(chr_error)

  # Make sure has same number as sets
  if ( length(lst_missing) != length(lst_items) )
    stop( "Argument 'lst_missing' must be same length as 'lst_items'" )

  lgc_in_data <- rep( TRUE, length(lst_missing) )

  # Loop over sets
  for ( s in seq_along(lst_missing) ) {

    # Check is list of lists
    if ( !is.list( lst_missing[[s]] ) ) stop(chr_error)

    # Make sure has same number as combos
    if ( length(lst_missing[[s]]) != length(lst_combos[[s]]) )
      stop(
        "Argument 'lst_missing' must have same structure as 'lst_combos"
      )

    # Loop over sublists
    for (l in seq_along( lst_missing[[s]] ) ) {

      lgc_in_data[s] <- all(
        lst_missing[[s]][[l]] %in% seq_along( lst_items[[s]] )
      )

      # Close 'Loop over sublists'
    }

    # Close 'Loop over sets'
  }

  if ( !all(lgc_in_data) )
    stop( 'Item indices for missing must match items provided' )

  return( lst_missing )
}

#### 4) Helper functions ####

#### 4.1) swaap_link.parameters ####
#' Extract Linkage Parameters
#'
#' Function to extract the parameters used when
#' running [swaap::swaap_link] (e.g., \code{'lst_sets'},
#' \code{'lst_items'}, etc.).
#'
#' @param dtf_linked A data frame, output from the
#'   [swaap::swaap_link] function. Must have the column
#'   \code{LNK.CHR.AttributeWithParameters}. Alternatively,
#'   set to \code{NULL} to see a reminder of the possible
#'   options for \code{'chr_input'}.
#' @param chr_input A character string, either \code{'chr_method'},
#'   \code{'lst_sets'}, \code{'lst_items'}, \code{'lst_combos'},
#'   or \code{'lst_missing'}.
#' @param lgc_unlist A logical value; if \code{TRUE} unlists
#'   an object and returns the unique elements (useful for
#'   the \code{'lst_items'} option).
#'
#' @author Kevin Potter
#'
#' @returns The specified parameter input used when linking records.
#'
#' @export

swaap_link.parameters <- function(
    dtf_linked,
    chr_input = 'lst_items',
    lgc_unlist = FALSE ) {

  # Template for chr_input
  if ( is.null(dtf_linked) ) {

    chr_message <- paste0(
      "Argument 'chr_input' can be:\n",
      "  'chr_method'\n",
      "  'lst_sets'\n",
      "  'lst_items'\n",
      "  'lst_combos'\n",
      "  'lst_missing'\n",
      "  'lst_time'\n"
    )
    message( chr_message )

    # Close 'Template for chr_input'
  }

  lst_parameters <- attributes(
    dtf_linked$LNK.CHR.AttributeWithParameters
  )[[ 'swaap.inputs_for_linking' ]]

  obj_return <- lst_parameters[[ chr_input ]]

  if ( lgc_unlist )
    obj_return <- obj_return |> unlist() |> unique()

  return( obj_return )
}

#### 4.2) swaap_link.rows ####
#' Extract Rows Flagged for Linking
#'
#' Function to extract the rows flagged for linking after
#' running [swaap::swaap_link].
#'
#' @param dtf_linked A data frame, output from the
#'   [swaap::swaap_link] function. Must have the column
#'   \code{LNK.CHR.Rows}.
#' @param int_row An integer, the row of \code{dtf_linked}
#'   to consider.
#'
#' @author Kevin Potter
#'
#' @returns Either the subset of rows in \code{dtf_linked}
#' that were flagged for linking, or (if \code{dtf_linked}
#' consists of only a single row) an integer vector.
#'
#' @export

swaap_link.rows <- function(
    dtf_linked,
    int_row = 1 ) {

  # Version of gsub that takes string as first argument
  fun_gsub <- function(
    chr_string,
    chr_pattern,
    chr_with ) {

    return(
      gsub( chr_pattern, chr_with, chr_string, fixed = TRUE )
    )

  }

  # Copy data frame
  dtf_current <- dtf_linked

  # Subset to specified row
  if ( nrow(dtf_linked) > 1 ) {

    dtf_current <- dtf_linked[int_row, ]

    # Close 'Subset to specified row'
  }

  int_rows <- as.numeric(
    strsplit(
      dtf_current$LNK.CHR.Rows |>
        fun_gsub( ',', ' ' ) |>
        fun_gsub( ';', ' ' ),
      split = ' ', fixed = TRUE
    )[[1]]
  ) |> unique() |> sort()

  # Return data frame
  if ( nrow(dtf_linked) > 1 ) {

    lgc_rows <-
      dtf_linked$IDN.INT.Row %in% c(
        int_rows,
        dtf_current$IDN.INT.Row
      )

    dtf_output <- dtf_linked[lgc_rows, ]

    return(
      dtf_output
    )

    # Close 'Return data frame'
  }

  return( int_rows )
}

#### 4.3) swaap_link.trim ####
#' Flag Duplicate Records to Trim Based on Rules
#'
#' Function to flag duplicate records based
#' on automated rules (default is to select
#' first record with highest rate of completion)
#' for subsequent trimming via the column
#' \code{'QLT.LGC.RemoveDuplicate'}.
#'
#' @param dtf_long A data frame, assumed to be standard processed
#'   school-wide assessment data with the columns
#'   \code{'LNK.LGC.Duplicates'} and \code{'IDN.CHR.Linked.ID'}.
#' @param fun_rule A function that takes a data frame
#'   and returns a logical vector equal to the number
#'   of rows set to \code{TRUE} for rows to keep and
#'   \code{FALSE} otherwise.
#' @param lst_arg An optional list of additional arguments to
#'   pass to the function for trimming.
#'
#' @author Kevin Potter
#'
#' @returns A data frame with a column \code{'QLT.LGC.RemoveDuplicate'}
#' indicating rows to remove that fail to meet the rules.
#'
#' @export

swaap_link.trim <- function(
    dtf_long,
    fun_rule = NULL,
    lst_arg = NULL ) {

  # Initialize column
  dtf_long$QLT.LGC.RemoveDuplicate <- FALSE

  # Default rule for resolving duplicates
  if ( is.null(fun_rule) ) {

    fun_rule <- swaap_link.trim_rule(
      chr_rule = ''
    )

    # Close 'Default rule for resolving duplicates'
  }

  # Isolate duplicates
  lgc_dup <- dtf_long$LNK.LGC.Duplicates

  # If any duplicates
  if ( any( lgc_dup) ) {

    chr_IDs <- unique(
      dtf_long$IDN.CHR.Linked.ID[lgc_dup]
    )

    # Loop over IDs
    for ( i in seq_along(chr_IDs) ) {

      lgc_rows <-
        dtf_long$IDN.CHR.Linked.ID %in% chr_IDs[i]

      dtf_long$QLT.LGC.RemoveDuplicate[lgc_rows] <- fun_rule(
        dtf_long[lgc_rows, ],
        lst_arg = lst_arg
      )

      # Close 'Loop over IDs'
    }

    # Add attribute with summary of what was removed/kept

    dtf_summary <- data.frame(
      Pattern = sort(
        unique( dtf_long$LNK.CHR.TimePoints[lgc_dup] )
      ),
      Records = NA,
      Records.Removed = NA,
      IDs = NA,
      IDs.Removed = NA
    )

    # Loop over rows
    for ( r in 1:nrow(dtf_summary) ) {

      lgc_pattern <-
        dtf_long$LNK.CHR.TimePoints %in% dtf_summary$Pattern[r] &
        lgc_dup
      dtf_summary$Records[r] <- sum(lgc_pattern)
      dtf_summary$Records.Removed[r] <- sum(
        lgc_pattern &
        dtf_long$QLT.LGC.RemoveDuplicate
      )
      dtf_summary$IDs[r] <- dplyr::n_distinct(
        dtf_long$IDN.CHR.Linked.ID[lgc_pattern]
      )
      dtf_summary$IDs.Removed[r] <-
        dtf_summary$IDs[r] -
        dplyr::n_distinct(
          dtf_long$IDN.CHR.Linked.ID[
            lgc_pattern &
            !dtf_long$QLT.LGC.RemoveDuplicate
          ]
        )

      # Close 'Loop over rows'
    }

    attributes(dtf_long$QLT.LGC.RemoveDuplicate) <- list(
      swaap.summary_removed = dtf_summary
    )

    # Close 'If any duplicates'
  }

  return(dtf_long)
}


#### 4.4) swaap_link.trim_rule ####
#' Return Function for Rule to Trim Duplicates
#'
#' Function to return another function that implements
#' a desired rule to trim flagged duplicates from a linked
#' data set.
#'
#' @param chr_rule A character string, the type of rule
#'   to use. Options include \code{'completed'} (choose
#'   the record with the highest completion rate), or
#'   \code{'outcome and completed'} (choose the record
#'   that has a non-missing value for an outcome then
#'   choose the record with the highest completion rate).
#'
#' @author Kevin Potter
#'
#' @returns A data frame with a column \code{'QLT.LGC.RemoveDuplicate'}
#' indicating rows to remove that fail to meet the rules.
#'
#' @export

swaap_link.trim_rule <- function(
    chr_rule ) {

  #### 3.5.1) List of defined rules ####

  lst_rules <- list(
    completed = c(
      'completed',
      'completion',
      'complete',
      'proportion completed',
      'proportion complete'
    ),
    outcome_and_completed = c(
      'outcome and completed',
      'outcome + completed',
      'outcome and completion',
      'outcome + completion'
    ),
    duplicate_times = c(
      'duplicate times',
      'exclude duplicate times',
      'duplicate time points',
      'exclude duplicate time points'
    )
  )

  # Default option
  if ( chr_rule == '' ) {

    chr_rule <- 'completed'

    # Close 'Default option'
  }

  #### 4.4.2) Rules for trimming duplicates ####

  #### 4.4.2.1) fun_rule.completed ####
  fun_rule.completed <- function(
    dtf_long,
    lst_arg = NULL ) {

    lgc_out <- rep( FALSE, nrow(dtf_long) )

    num_cmp <-
      dtf_long$QLT.DBL.ProportionCompleted.Total

    # If non-missing completion
    if ( any( !is.na(num_cmp) ) ) {

      int_keep <- which(
        !is.na( num_cmp ) &
          num_cmp %in% max( num_cmp, na.rm = T )
      )[1]

      lgc_out[int_keep] <- TRUE

      # Close 'If non-missing completion'
    }

    return( !lgc_out )
  }

  #### 4.4.2.2) fun_rule.outcome_and_completed ####
  fun_rule.outcome_and_completed <- function(
    dtf_long,
    lst_arg = NULL ) {

    lgc_out <- rep( FALSE, nrow(dtf_long) )

    num_cmp <-
      dtf_long$QLT.DBL.ProportionCompleted.Total

    # Extract outcome
    if ( !is.null(lst_arg) ) {

      chr_outcome <- lst_arg[[1]]

      # Close 'Extract outcome'
    } else {

      chr_outcome <- 'SBS.INT.ALC.Past31.UseRating'

      # Close else for 'Extract outcome'
    }

    num_out <- dtf_long[[ chr_outcome ]]

    # Outcome not found
    if ( is.null(num_out) )
      num_out <- rep( NA, length(lgc_out) )

    # Non-missing outcome
    if ( any( !is.na(num_out) ) )
      num_cmp[ is.na(num_out) ] <- NA

    # If non-missing completion
    if ( any( !is.na(num_cmp) ) ) {

      int_keep <- which(
        !is.na( num_cmp ) &
          num_cmp %in% max( num_cmp, na.rm = T )
      )[1]

      lgc_out[int_keep] <- TRUE

      # Close 'If non-missing completion'
    }

    return( !lgc_out )
  }

  #### 4.4.2.3) fun_rule.duplicate_times ####
  fun_rule.duplicate_times <- function(
    dtf_long,
    lst_arg = NULL ) {

    lgc_out <- rep( FALSE, nrow(dtf_long) )

    chr_patterns <- sort( unique( dtf_long$LNK.CHR.TimePoints ) )
    chr_possible <- paste0(
      sort( unique( dtf_long$SSS.INT.TimePoint ) ),
      '-',
      sort( unique( dtf_long$SSS.INT.TimePoint ) )
    )

    lgc_duplicates <- sapply(
      chr_patterns, function(s) sapply(
        chr_possible, function(p) grepl( p, s, fixed = TRUE )
      )
    )

    # Multiple time points
    if ( !is.null( dim(lgc_duplicates ) ) ) {

      lgc_duplicates <- apply( lgc_duplicates, 2, any )

      # Close 'Multiple time points'
    }

    # Any duplicates
    if ( any(lgc_duplicates) ) {

      lgc_out <- dtf_long$LNK.CHR.TimePoints %in%
        chr_patterns[!lgc_duplicates]

      # Close 'Any duplicates'
    }

    return( !lgc_out )
  }

  #### 4.4.3) Return specified rule ####

  if ( chr_rule %in% lst_rules$completed )
    return( fun_rule.completed )

  if ( chr_rule %in% lst_rules$outcome_and_completed )
    return( fun_rule.outcome_and_completed )

  if ( chr_rule %in% lst_rules$duplicate_times )
    return( fun_rule.duplicate_times )

  chr_err <- paste0(
    'Rule not found, possible options are:\n',
    paste( paste0( "  '", names(lst_rules), "'" ), collapse = "\n" ),
    "\n"
  )

  stop( chr_err )
}

#### 4.5) swaap_link.similarity ####
#' Compute Post-hoc Similarity Score for Linked Records
#'
#' Function to compute a post-hoc similarity score based
#' on exact matches for records across a pair of time
#' points.
#'
#' @param dtf_linked A data frame, output from the
#'   [swaap::swaap_link] function. Must have the columns
#'   \code{IDN.CHR.Linked.ID}, \code{LNK.LGC.NoIssues},
#'   and \code{IDN.CHR.Linked.ID}.
#' @param chr_items A character vector, the columns to
#'   compare when computing the similarity score.
#' @param int_times An integer vector, the pair of
#'   of time points to compute the score over.
#'
#' @author Kevin Potter
#'
#' @returns A list with the matrix of matches by linked ID,
#' a data frame summarizing similarity by item, and a
#' data frame with similarity scores by ID.
#'
#' @export

swaap_link.similarity <- function(
    dtf_linked,
    chr_items,
    int_times = NULL ) {

  if ( is.null(int_times) )
    int_times <- sort( unique( dtf_linked$SSS.INT.TimePoint ) )

  dtf_IDs <- dtf_linked |>
    dplyr::filter(
      SSS.INT.TimePoint %in% int_times
    ) |>
    dplyr::group_by(
      IDN.CHR.Linked.ID
    ) |>
    dplyr::summarise(
      Records = length( IDN.CHR.Linked.ID ),
      .groups = 'drop'
    ) |>
    data.frame()

  dtf_IDs <- dtf_IDs |>
    dplyr::filter(
      Records >= 2
    )

  mat_similarity <- sapply(
    1:nrow(dtf_IDs), function(r) {

      int_R <- dtf_IDs$R[r]
      int_rows <- which(
        dtf_linked$IDN.CHR.Linked.ID %in% dtf_IDs$IDN.CHR.Linked.ID[r]
      )

      int_score <- sapply(
        chr_items,
        function(i) sum(
          dtf_linked[[i]][ int_rows[1] ] == dtf_linked[[i]][ int_rows[-1] ]
        ) / ( length(int_rows) - 1 )
      )

      return(
        int_score
      )

    }
  ) |> t()

  rownames( mat_similarity ) <- dtf_IDs[[1]]
  colnames( mat_similarity ) <- chr_items

  dtf_IDs$Similarity <- apply(
    mat_similarity, 1, function(x) {
      sum( x == 1, na.rm = T )
    }
  )
  dtf_IDs$Similarity.Partial <- apply(
    mat_similarity, 1, function(x) {
      sum( x > 0 & x < 1, na.rm = T )
    }
  )
  dtf_IDs$Items <- apply(
    mat_similarity, 1, function(x) {
      sum( !is.na(x) )
    }
  )
  dtf_IDs$Score <-
    dtf_IDs$Similarity / dtf_IDs$Items

  int_items <- apply(
    mat_similarity, 2, function(x) sum( !is.na(x) )
  )
  int_similar <- colSums( mat_similarity == 1, na.rm = T )

  lst_output <- list(
    similarity = mat_similarity,
    summary = data.frame(
      Item = chr_items,
      N = int_items,
      Match.Count = int_similar,
      Match.Proportion = int_similar / int_items
    ),
    scores = dtf_IDs
  )
  rownames( lst_output$summary ) <- 1:nrow( lst_output$summary )

  return( lst_output )
}

#### 4.6) swaap_link.sets ####
#' Summary of Sets for Linking
#'
#' Returns a table summarizing the set of pairs to link
#' over in terms of year, semester, and grade level.
#'
#' @param dtf_data A data frame, assumed to be standard processed
#'   school-wide assessment data with the columns
#'   \code{'SSS.INT.SurveyYear'}, \code{'SSS.CHR.Semester'},
#'   and \code{'SSS.INT.Grade'}.
#' @param lst_sets A list of lists, with each sublist specifying
#'   \code{'Base'} and \code{'Add'} logical vectors for the pair of data
#'   subsets in \code{dtf_long} to link over (e.g., \code{'Base'} would
#'   subset the first time point and \code{'Add'} would subset the second
#'   time point).
#' @param lgc_character A logical value; if \code{TRUE} outputs a
#'   character vector instead of a data frame.
#'
#' @author Kevin Potter
#'
#' @returns Either a data frame with a row for each set of pairs, or
#' a character vector with concatenated abbreviations describing the
#' sets.
#'
#' @export

swaap_link.sets <- function(
    dtf_data,
    lst_sets,
    lgc_character = FALSE ) {

  dtf_sets <- data.frame(
    Set = seq_along(lst_sets),
    Year.Base = NA,
    Semester.Base = NA,
    Grade.Base = NA,
    Records.Base = NA,
    Year.Add = NA,
    Semester.Add = NA,
    Grade.Add = NA,
    Records.Add = NA
  )

  # Loop over sets
  for ( s in 1:nrow(dtf_sets) ) {

    dtf_sets$Year.Base[s] <-
      dtf_data$SSS.INT.SurveyYear[
        lst_sets[[s]][[1]]
      ] |> unique() |> substr( 3, 4 )

    dtf_sets$Year.Add[s] <-
      dtf_data$SSS.INT.SurveyYear[
        lst_sets[[s]][[2]]
      ] |> unique() |> substr( 3, 4 )

    dtf_sets$Semester.Base[s] <-
      dtf_data$SSS.CHR.Semester[
        lst_sets[[s]][[1]]
      ] |> unique() |> substr( 1, 1 )

    dtf_sets$Semester.Add[s] <-
      dtf_data$SSS.CHR.Semester[
        lst_sets[[s]][[2]]
      ] |> unique() |> substr( 1, 1 )

    dtf_sets$Grade.Base[s] <-
      dtf_data$SSS.INT.Grade[
        lst_sets[[s]][[1]]
      ] |> unique()

    dtf_sets$Grade.Add[s] <-
      dtf_data$SSS.INT.Grade[
        lst_sets[[s]][[2]]
      ] |> unique()

    dtf_sets$Records.Base[s] <- sum(
      lst_sets[[s]][[1]]
    )
    dtf_sets$Records.Add[s] <- sum(
      lst_sets[[s]][[2]]
    )

    # Close 'Loop over sets'
  }

  lgc_grade_base <- dtf_sets$Grade.Base < 10
  lgc_grade_add <- dtf_sets$Grade.Add < 10

  dtf_sets$Grade.Base[ lgc_grade_base ] <-
    paste0( '0', dtf_sets$Grade.Base[ lgc_grade_base ] )
  dtf_sets$Grade.Add[ lgc_grade_add ] <-
    paste0( '0', dtf_sets$Grade.Add[ lgc_grade_add ] )

  # Return character vector
  if ( lgc_character ) {

    chr_sets <- paste0(
      'Y',
      dtf_sets$Year.Base,
      dtf_sets$Semester.Base,
      'G',
      dtf_sets$Grade.Base,
      't',
      'Y',
      dtf_sets$Year.Add,
      dtf_sets$Semester.Add,
      'G',
      dtf_sets$Grade.Add
    )

    return( chr_sets )

    # Close 'Return character vector'
  }

  return( dtf_sets )
}

#### 3.8) swaap_link.timepoints ####
#' Extract Possible Linked Time Point Patterns
#'
#' Function to extract possible linked time point patterns
#' based on available time points, or isolate records that
#' meet linked time point patterns.
#'
#' @param dtf_data A data frame, assumed to be standard processed
#'   school-wide assessment data with the columns
#'   \code{'SSS.INT.TimePoint'} and \code{'LNK.CHR.TimePoints'}.
#'
#' @author Kevin Potter
#'
#' @returns Either a character vector with all possible linked
#' time point patterns (ignoring duplicates), or a logical
#' vector for all records that meet this linked time point
#' patterns.
#'
#' @export

swaap_link.timepoints <- function(
    dtf_data,
    lgc_character = TRUE ) {

  int_times <- dtf_data$SSS.INT.TimePoint |> unique() |> sort()

  lst_links <- lapply(
    2:length(int_times),
    function(n) {
      mat_links <- combn( int_times, n )
      chr_links <- sapply(
        1:ncol(mat_links), function(k) {
          paste( mat_links[, k], collapse = '-' )
        }
      )
    }
  )
  chr_timepoints <- unlist( lst_links )

  if (!lgc_character)
    return( dtf_data$LNK.CHR.TimePoints %in% chr_timepoints )

  return( chr_timepoints )
}

#### 3.9) swaap_link.linked_over ####

swaap_link.linked_over <- function(
    dtf_linked,
    chr_measures ) {

  chr_new <- sapply(
    chr_measures, function(m) {

      chr_parts <- strsplit(
        m, split = '.', fixed = TRUE
      )[[1]]

      chr_out <- paste0(
        'LNK.CHR.', chr_parts[3], 's'
      )

      return( chr_out )
    }
  )

  # Loop over measures
  for ( m in seq_along(chr_new) )
    dtf_linked[[ chr_new[m] ]] <- NA

  chr_IDs <- unique( dtf_SRV.Merged$IDN.CHR.Linked.ID )

  # Loop over IDs
  for ( i in chr_IDs ) {

    lgc_cases <- dtf_linked$IDN.CHR.Linked.ID %in% i

    # Loop over measures
    for ( m in seq_along(chr_new) )
      dtf_linked[[ chr_new[m] ]][lgc_cases] <- paste(
        sort( dtf_linked[[ chr_measures[m] ]][lgc_cases] ),
        collapse = '-'
      )

    # Close 'Loop over IDs'
  }

  return( dtf_linked )
}


#### 5) Report functions ####

#### 5.1) swaap_link.report.by_ID ####
#' Linkage Patterns by Linked Identifier
#'
#' Function to summarize measures by linked IDs.
#' As default summarizes the pattern of linked
#' time points.
#'
#' @param dtf_linked A data frame, output from the
#'   [swaap::swaap_link] function. Must have the columns
#'   \code{IDN.CHR.Linked.ID}.
#' @param chr_measures A character vector, the columns in
#'   \code{dtf_linked} to summarize per unique value of
#'   \code{IDN.CHR.Linked.ID}.
#' @param fun_summary An optional function to summarize
#'   a vector of values. Default is to sort values
#'   and concatenate with a hyphen.
#' @param chr_start A character vector (length cannot
#'   exceed \code{chr_measures}), the start of each
#'   name for the new summary variables.
#' @param chr_end A character vector (length cannot
#'   exceed \code{chr_measures}), the end of each
#'   name for the new summary variables.
#' @param lst_args An optional list of additional inputs
#'   for the \code{fun_summary} function. If less than
#'   5 elements are provided, expanded to 5 elements
#'   via [base::rep_len], each element is passed as
#'   an additional argument to \code{fun_summary}
#'   per each measure.
#' @param lgc_update A logical value; if \code{TRUE}
#'   merges the wide-form summary variables by ID
#'   with the original long-form data frame
#'   \code{dtf_linked}.
#'
#' @author Kevin Potter
#'
#' @returns A wide-form data frame with one row
#' per linked ID along with the pattern of linked
#' time points (e.g., \code{'0-1'} means a link
#' from baseline to the first time point). If
#' \code{lgc_update} is \code{TRUE}, instead
#' returns an updated version of \code{dtf_linked}.
#'
#' @export

swaap_link.report.by_ID <- function(
    dtf_linked,
    chr_measures = 'SSS.INT.TimePoint',
    fun_summary = NULL,
    chr_start = 'LNK.CHR.',
    chr_end = 's',
    lst_args = NULL,
    lgc_update = FALSE ) {

  if ( length(chr_measures) > 5 )
    stop( 'Only up to 5 measures allowed at a time' )

  # Expand size
  if ( length(lst_args) <= 1 ) {

    lst_args <- list(
      lst_args,
      lst_args,
      lst_args,
      lst_args,
      lst_args
    )

    # Close 'Expand size'
  }

  # Expand size
  if ( length(lst_args) < 5 )
    lst_args <- rep_len( lst_args, 5 )

  # Default summary function
  if ( is.null( fun_summary ) ) {

    fun_summary <- function(
      obj_vec, lst_args ) {

      return( paste( sort( obj_vec ), collapse = '-' ) )

    }

    # Close 'Default summary function'
  }

  # Loop over up to 5 measures
  for ( m in 1:5 ) {

    dtf_linked[[ paste0( 'M', m ) ]] <- NA

    # If measure exists
    if ( m <= length(chr_measures) )
      dtf_linked[[ paste0( 'M', m ) ]] <-
        dtf_linked[[ chr_measures[m] ]]

    # Close 'Loop over up to 5 measures'
  }

  dtf_IDs <- dtf_linked |>
    dplyr::group_by(
      IDN.CHR.Linked.ID
    ) |>
    dplyr::summarise(
      SM1 = fun_summary( M1, lst_args[[1]] ),
      SM2 = fun_summary( M2, lst_args[[2]] ),
      SM3 = fun_summary( M3, lst_args[[3]] ),
      SM4 = fun_summary( M4, lst_args[[4]] ),
      SM5 = fun_summary( M5, lst_args[[5]] ),
      .groups = 'drop'
    ) |>
    data.frame()

  chr_columns <- colnames(dtf_IDs)
  chr_start <- rep_len( chr_start, length(chr_measures) )
  chr_end <- rep_len( chr_end, length(chr_measures) )
  chr_new <- sapply(
    seq_along( chr_measures ), function(m) {

      return(
        paste0(
          chr_start[m],
          strsplit( chr_measures[m], split = '.', fixed = TRUE )[[1]][3],
          chr_end[m]
        )
      )

    }
  )
  chr_columns[seq_along(chr_measures) + 1] <- chr_new
  colnames(dtf_IDs) <- chr_columns
  dtf_IDs <- dtf_IDs |>
    dplyr::select(
      IDN.CHR.Linked.ID,
      all_of( chr_new )
    )

  # Update original data set with linkage patterns
  if ( lgc_update ) {

    dtf_linked <- suppressMessages(
      dtf_linked |>
        dplyr::left_join(
          dtf_IDs
        ) |>
        dplyr::select(
          -all_of( paste0( 'M', 1:5 ) )
        )
    )

    return( dtf_linked )

    # Close 'Update original data set with linkage patterns'
  }

  return( dtf_IDs )
}

#### 5.2) swaap_link.report ####
#' Summary of Linking of Records
#'
#' Function to summarize the performance of
#' the linking code for the school-wide
#' assessment. Provides summary statistics
#' for what records were linked, and if
#' columns with the 'true' IDs are detected,
#' provides details on hits and correct
#' rejections.
#'
#' @param dtf_linked A data frame, the output
#'   from [swaap::swaap_link].
#' @param lst_groups A named list of column names,
#'   the grouping factors to consider when summarizing
#'   the number of records linked.
#'
#' @author Kevin Potter
#'
#' @returns A list of data frames.
#'
#' @examples
#' # Linking across time points
#' dtf_long <- swaap_simulate( 'link', 'demo' )
#' dtf_linked <- swaap_link(dtf_long)
#' lst_summary <- swaap_link.report(dtf_linked)
#'
#' @export

swaap_link.report <- function(
    dtf_linked,
    lst_groups = NULL ) {

  lst_output <- list()

  #### 5.2.1) Setup ####

  fun_count_percent <- function(
    lgc_x,
    int_num = NULL,
    int_denom = NULL ) {

    # Numerator/Denominator
    if ( !is.null(lgc_x) ) {

      int_num <- sum(lgc_x)
      int_denom <- length(x)

      # Close 'Numerator/Denominator'
    }

    chr_out <- paste0(
      int_num, '/', int_denom,
      ' (',
      format(
        round( 100*int_num/int_denom, 1 ),
        nsmall = 1
      ), '%)'
    )

    return(chr_out)
  }

  dtf_summary.overall <- dtf_linked |>
    dplyr::group_by(
      Any = grepl( '-', LNK.CHR.TimePoints ),
      Linkage = LNK.CHR.TimePoints
    ) |>
    dplyr::summarise(
      Records = length(IDN.CHR.Linked.ID),
      IDs = dplyr::n_distinct( IDN.CHR.Linked.ID ),
      .groups = 'drop'
    ) |>
    data.frame() |>
    dplyr::mutate(
      Percent = round(
        100*IDs / sum( IDs ), 1
      ),
      Collapsed = ''
    )
  dtf_summary.overall$Collapsed[
    which( dtf_summary.overall$Any )[1]
  ] <- round( 100*sum(
    dtf_summary.overall$IDs[ dtf_summary.overall$Any ]
  ) / sum(
    dtf_summary.overall$IDs
  ), 1 )
  dtf_summary.overall$Collapsed[
    which( !dtf_summary.overall$Any )[1]
  ] <- round( 100*sum(
    dtf_summary.overall$IDs[ !dtf_summary.overall$Any ]
  ) / sum(
    dtf_summary.overall$IDs
  ), 1 )

  lst_output$linkage <- list(
    overall = dtf_summary.overall
  )

  # Wide-form data with linkage patterns
  dtf_IDs <- swaap::swaap_link.report.by_ID(
    dtf_linked
  )



  # # Initialize output
  # lst_output <- list()
  #
  # #### 5.2) Linkage patterns [Overall] ####
  #
  # dtf_summary.linkage_patterns <- aggregate(
  #   rep( TRUE, nrow(dtf_IDs) ),
  #   list( Patterns = dtf_IDs$SSS.CHR.Linked.Linkage_patterns,
  #         Duplicates = dtf_IDs$QCC.LGC.Duplicates ),
  #   function(x) sum(x)
  # )
  # colnames(dtf_summary.linkage_patterns)[3] <- 'N'
  # dtf_summary.linkage_patterns$CP <- sapply(
  #   1:nrow(dtf_summary.linkage_patterns), function(r) {
  #
  #     fun_count_percent(
  #       NULL,
  #       dtf_summary.linkage_patterns$N[r],
  #       dtf_summary.linkage_patterns$N |> sum()
  #     )
  #
  #   }
  # )
  #
  # lst_output$linkage_patterns <- list(
  #   overall = dtf_summary.linkage_patterns
  # )
  #
  # #### 5.3) Linkage patterns [Groups] ####
  #
  # #### 5.4) Any linked [Overall] ####
  #
  # dtf_IDs$Current <- dtf_IDs$SSS.INT.Linked.Records.TP.0
  # dtf_IDs$Current[
  #   dtf_IDs$Current > 1
  # ] <- '2+'
  #
  # dtf_summary.linked_with <- aggregate(
  #   rep( TRUE, nrow(dtf_IDs) ),
  #   list( Linked_with = dtf_IDs$Current ),
  #   function(x) sum(x)
  # )
  #
  # colnames(dtf_summary.linked_with)[2] <- 'N'
  # dtf_summary.linked_with <-
  #   dtf_summary.linked_with[
  #     dtf_summary.linked_with[[1]] %in% c( '1', '2+' ),
  #   ]
  # dtf_summary.linked_with$CP <- sapply(
  #   1:nrow(dtf_summary.linked_with), function(r) {
  #
  #     fun_count_percent(
  #       NULL,
  #       dtf_summary.linked_with$N[r],
  #       dtf_summary.linked_with$N |> sum()
  #     )
  #
  #   }
  # )
  #
  # lst_output$linked_with <- list(
  #   overall = dtf_summary.linked_with
  # )

  #### 5.2.?) ... ####

  # If column with true IDs detected
  if ( 'LNK.INT.True.ID' %in% colnames(dtf_linked) ) {

    dtf_summary.true_ID <- data.frame(
      Type = unique(
        dtf_linked$LNK.CHR.True.TestType
      ),
      Records = NA,
      Linked = NA,
      Hits_true = NA,
      Hits = NA,
      Rejects_true = NA,
      Rejects = NA,
      Duplicates = NA
    )

    # Loop over types
    for ( r in 1:nrow(dtf_summary.true_ID) ) {

      lgc_rows <-
        dtf_linked$LNK.CHR.True.TestType %in%
        dtf_summary.true_ID$Type[r]

      int_ID <- dtf_linked$LNK.INT.True.ID[lgc_rows]
      lgc_zero <- int_ID == 0
      chr_ID <- dtf_linked$IDN.CHR.Linked.ID[lgc_rows]

      dtf_summary.true_ID$Linked[r] <-
        length( unique( chr_ID ) )

      dtf_summary.true_ID$Records[r] <- sum(lgc_rows)
      dtf_summary.true_ID$Hits_true[r] <-
        length( unique( int_ID[!lgc_zero] ) )
      dtf_summary.true_ID$Rejects_true[r] <-
        sum( lgc_zero )

      n_hits <- 0

      # Hits for records that should be linked
      if ( any(!lgc_zero) ) {

        dtf_H <- aggregate(
          chr_ID[!lgc_zero],
          list( int_ID[!lgc_zero] ),
          function(x) {
            length( unique(x) )
          }
        )
        n_hits <- sum( dtf_H[[2]] == 1 )

        # Close 'Hits for records that should be linked'
      }

      n_rejects <- 0

      # False alarms for records that should not be linked
      if ( any(lgc_zero) ) {

        dtf_FA <- aggregate(
          int_ID,
          list( chr_ID ),
          function(x) {
            sum( x == 0 )
          }
        )
        n_rejects <- sum( dtf_FA[[2]] == 1 )

        # Close 'False alarms for records that should not be linked'
      }

      dtf_summary.true_ID$Duplicates[r] <- 0

      lgc_dup <-
        dtf_linked$LNK.LGC.Duplicates[lgc_rows]

      # Check if duplicate records
      if ( any( lgc_dup ) ) {

        dtf_summary.true_ID$Duplicates[r] <-
          length( unique( chr_ID[lgc_dup] ) )

        # Close 'Check if duplicate records'
      }

      dtf_summary.true_ID$Hits[r] <- n_hits
      dtf_summary.true_ID$Rejects[r] <- n_rejects

      # Close 'Loop over types'
    }

    lst_output$true <- dtf_summary.true_ID

    # Close 'If column with true IDs detected'
  }

  return( lst_output )
}

#### 5.3) swaap_link.report.discrepant ####
#' Determine Discrepancies Between Records
#'
#' Given a data frame with linked records across
#' a pair of time points, computes discrepancies
#' between these records over a specified set of
#' items. Will generate a plot summarizing
#' findings if specified. Users should carefully
#' consider treatment of missing values when
#'
#'
#' @param dtf_linked A data frame, must have the
#'   columns \code{'IDN.CHR.Linked.ID'} &
#'   \code{'LNK.LGC.NoIssues'}.
#' @param chr_items A character vector, the items
#'  to check for discrepancies across pairs of
#'  records.
#' @param chr_missingness A character string,
#'   used to specify the treatment of missing
#'   values when comparing across records.
#'   If \code{'match'} marks an item with
#'   both record values as missing as stable,
#'   and an item with only one value as
#'   missing as discrepant. If \code{'ignore'}
#'   marks an item as stable irrespective if
#'   one or both values are missing. If
#'   \code{'exclude'} removes the pair of
#'   records from consideration if any
#'   missing values are found.
#' @param lgc_plot A logical value; if \code{TRUE}
#'   generates a plot summarizing the results.
#' @param num_adj ...
#' @param mat_layout ...
#' @param chr_colors ...
#'
#' @author Kevin Potter
#'
#' @returns A list with a) a data frame tracking
#'   stable and discrepant items per each pair of
#'   linked records, b) a data frame tracking the
#'   number of pairs that follow a specific pattern
#'   of stable/discrepant values, and c) a data frame
#'   tracking the marginal stability rates per each
#'   item.
#'
#' @export

swaap_link.report.discrepant <- function(
    dtf_linked,
    chr_items,
    chr_missingness = 'exclude',
    lgc_plot = TRUE,
    num_adj = c( .05, 1.05 ),
    mat_layout = NULL,
    chr_colors = c( 'lightblue', 'pink' ) ) {

  #### 5.3.1) Setup ####

  if ( !chr_missingness %in% c( 'match', 'ignore', 'exclude') )
    stop(
      "Argument 'chr_missingness' must be 'match', 'ignore', or 'exclude'"
    )

  dtf_IDs <- dtf_linked |>
    dplyr::filter(
      LNK.LGC.NoIssues
    ) |>
    dplyr::group_by(
      IDN.CHR.Linked.ID
    ) |>
    dplyr::summarise_at(
      chr_items, function(x) sum( is.na(x) )/length(x)
    ) |>
    data.frame()
  dtf_IDs$R <- sapply(
    dtf_IDs[[1]], function(i) {
      sum( dtf_linked$IDN.CHR.Linked.ID %in% i )
    }
  )
  dtf_IDs <- dtf_IDs |>
    dplyr::filter(
      R == 2
    )

  if ( nrow(dtf_IDs) == 0 )
    stop( "No pairs of records found" )

  # Compute stability
  mat_stable <- matrix(
    NA, nrow(dtf_IDs), length(chr_items)
  )

  # Loop over linked IDs
  for ( i in 1:nrow(dtf_IDs) ) {

    lgc_rows <-
      dtf_linked$IDN.CHR.Linked.ID == dtf_IDs[[1]][i]

    mat_stable[i, ] <-
      unlist( dtf_linked[lgc_rows, chr_items][1, ] ) ==
      unlist( dtf_linked[lgc_rows, chr_items][2, ] )

    lgc_r1 <- is.na( dtf_linked[lgc_rows, chr_items][1, ] )
    lgc_r2 <- is.na( dtf_linked[lgc_rows, chr_items][2, ] )

    if ( chr_missingness %in% c( 'match', 'ignore' ) )
      mat_stable[i, lgc_r1 & lgc_r2] <- TRUE
    if ( chr_missingness %in% 'ignore' )
      mat_stable[i, lgc_r1 | lgc_r2] <- TRUE

    # Close 'Loop over linked IDs'
  }

  # Set missing comparisons to discrepant
  if ( chr_missingness %in% 'match' ) {

    mat_stable[ is.na(mat_stable) ] <- FALSE

    # Close 'Set missing comparisons to discrepant'
  }

  # Exclude any missing comparisons
  if ( chr_missingness %in% 'exclude' ) {

    lgc_missing <- apply(
      mat_stable, 1, function(x) any( is.na(x) )
    )

    if ( all(lgc_missing) )
      stop( 'No records found with non-missing comparisons' )

    dtf_IDs <- dtf_IDs[ !lgc_missing, ]
    mat_stable <- mat_stable[ !lgc_missing, ]

    # Close 'Exclude any missing comparisons'
  }

  colnames( mat_stable ) <- chr_items

  chr_patterns <- apply(
    mat_stable, 1, function(x)
      paste( as.numeric( x ), collapse = '' )
  )

  dtf_stable <- cbind(
    data.frame(
      IDN.CHR.Linked.ID = dtf_IDs[[1]],
      Stable.Count = rowSums( mat_stable ),
      Stable.Pattern = chr_patterns
    ),
    mat_stable
  )
  rownames(dtf_stable) <- 1:nrow(dtf_stable)

  dtf_pattern <- dtf_stable |>
    dplyr::group_by( Stable.Count, Stable.Pattern ) |>
    dplyr::summarise(
      Pattern.Count = length(Stable.Pattern),
      Pattern.Proportion = NA,
      .groups = 'drop'
    ) |>
    data.frame()
  dtf_pattern <- dtf_pattern[
    rev( 1:nrow(dtf_pattern) ),
  ]
  dtf_pattern$Pattern.Proportion <-
    dtf_pattern$Pattern.Count /
    sum( dtf_pattern$Pattern.Count )
  rownames(dtf_pattern) <- 1:nrow(dtf_pattern)

  dtf_item <- data.frame(
    Item = colnames(mat_stable),
    N = nrow(mat_stable ),
    Stable.Count = colSums(mat_stable),
    Stable.Proportion = NA
  )
  dtf_item$Stable.Proportion <-
    dtf_item$Stable.Count / dtf_item$N
  rownames(dtf_item) <- 1:nrow(dtf_item)
  dtf_item <- dtf_item |>
    dplyr::arrange(
      dplyr::desc(Stable.Proportion)
    )
  dtf_item$Discrepant.Proportion <-
    1 - dtf_item$Stable.Proportion

  mat_pattern <- sapply(
    dtf_pattern$Stable.Pattern, function(x) {
      as.numeric( mat_stable[ which(chr_patterns == x )[1], ] )
    }
  ) |> t()
  colnames(mat_pattern) <- chr_items
  mat_pattern <- mat_pattern[, dtf_item$Item]
  dtf_pattern <- cbind(
    dtf_pattern,
    mat_pattern
  )

  lst_summary <- list(
    stable = dtf_stable,
    pattern = dtf_pattern,
    item = dtf_item
  )

  #### 5.3.2) Plotting ####

  # Generate plot
  if (lgc_plot) {

    chr_terms = c(
      "Stable",
      "Discrepant",
      "discrepant"
    )

    chr_labels <- sapply(
      dtf_item$Item, function(s) {
        tail( strsplit( s, split = '.', fixed = TRUE )[[1]], n = 1 )
      }
    )
    int_col <- length(chr_labels)

    if ( is.null( mat_layout ) )
      mat_layout <- rbind(
        c( 2, 2, 2, 2, 4 ),
        c( 1, 1, 1, 1, 3 ),
        c( 1, 1, 1, 1, 3 ),
        c( 1, 1, 1, 1, 3 )
      )

    layout(
      mat_layout
    )

    #### 5.3.2.1) Panel 1 ####

    num_xl <- c( 0, nrow(dtf_pattern) )
    num_yl <- c( 0, int_col )

    par( mai = c( .07, .8, .001, .001 ) )

    plot(
      num_xl, num_yl,
      type = 'n', xaxt = 'n', yaxt = 'n',
      xlab = '', ylab = '', bty = 'n'
    )

    axis(
      2, rev( 1:int_col ) - .5, chr_labels,
      las = 1, cex = 1, line = -2, tick = FALSE
    )

    # Loop over columns
    for ( j in 1:int_col ) {

      # Loop over rows
      for ( k in 1:nrow(mat_pattern) ) {

        polygon(
          c( 0, 1, 1, 0 ) + (k - 1),
          c( 0, 0, 1, 1 ) + (int_col - j),
          col = c(
            chr_colors[2],
            chr_colors[1]
          )[ mat_pattern[k, j] + 1 ]
        )

        # Close 'Loop over rows'
      }

      # Close 'Loop over columns'
    }

    #### 5.3.2.2) Panel 2 ####

    int_counts <- dtf_pattern$Pattern.Count

    num_yl <- c( 0, 1 )

    par( mai = c( .001, .8, .07, .001 ) )

    plot(
      num_xl, num_yl,
      type = 'n', xaxt = 'n', yaxt = 'n',
      xlab = '', ylab = '', bty = 'n'
    )

    # draw_hv(
    #   h = seq( 0, 1, .2 ), l = num_xl,
    #   col = 'grey80', lwd = 1
    # )
    #
    # add_axes(
    #   seq( 0, 1, .2 ), ( 100*seq( 0, 1, .2 ) ) %p% '%',
    #   side = 2, line = -1.25, cex = 1, las = 1
    # )

    mtext(
      "Frequency of \neach pattern",
      side = 2, line = 2.5, cex = .65
    )

    # Loop over rows
    for ( k in 1:nrow(dtf_pattern) ) {

      polygon(
        c( 0, 1, 1, 0 ) + (k - 1),
        c( 0, 0, 1, 1 ) * int_counts[k] / sum(int_counts),
        col = 'grey'
      )

      text(
        .6 + (k - 1),
        num_adj[1] + int_counts[k] / sum(int_counts),
        paste0( round( 100*int_counts[k] / sum(int_counts), 1 )
                |> format( nsmall = 1 ), '%' ),
        cex = .7, pos = 3, srt = 90
      )

      # Close 'Loop over rows'
    }

    #### 5.3.2.3) Panel 3 ####

    int_counts <- dtf_item$Stable.Count

    num_xl <- c( 0, 1 )
    num_yl <- c( 0, int_col )

    par( mai = c( .07, .001, .001, .07 ) )

    plot(
      num_xl, num_yl,
      type = 'n', xaxt = 'n', yaxt = 'n',
      xlab = '', ylab = '', bty = 'n'
    )

    # draw_hv(
    #   v = seq( 0, 1, .2 ), l = num_yl,
    #   col = 'grey80', lwd = 1
    # )
    #
    # add_axes(
    #   seq( 0, 1, .4 ), ( 100 * seq( 0, 1, .4 ) ) %p% '%',
    #   side = 3, line = -1.25, cex = 1
    # )
    #
    # add_axes(
    #   seq( .2, 1, .4 ), ( 100 * seq( 0.2, 1, .4 ) ) %p% '%',
    #   side = 3, line = -1.25, cex = 1
    # )

    mtext(
      paste0( "Total ", chr_terms[3], " by variable" ),
      side = 3, line = 1, cex = .65
    )

    # Loop over columns
    for ( j in 1:int_col ) {

      num_prop <- 1 - int_counts[j]/nrow(dtf_stable)

      polygon(
        c( 0, 1, 1, 0 )*num_prop,
        c( 0, 0, 1, 1 ) + (int_col - j),
        col = 'grey'
      )

      text(
        num_adj[2]*num_prop,
        .5 + (int_col - j),
        paste0( round( 100*num_prop, 1 )
                |> format( nsmall = 1 ), '%' ),
        cex = .9, pos = 4
      )

      # Close 'Loop over rows'
    }

    #### 5.3.2.4) Panel 4 ####

    par( mar = rep( 0, 4 ) )

    plot(
      0:1, 0:1,
      type = 'n', xaxt = 'n', yaxt = 'n',
      xlab = '', ylab = '', bty = 'n'
    )

    legend(
      .25, .9,
      chr_terms[1:2],
      fill = c( 'lightblue', 'pink' ),
      bty = 'n',
      cex = 1.5,
      xpd = NA
    )

    # Close 'Generate plot'
  }

  return( lst_summary )
}

#### 5.4) swaap_link.report.comparison ####

swaap_link.report.comparison <- function(
    dtf_linked,
    chr_ID,
    chr_TP ) {

  dtf_linked$ID_1 <- dtf_linked[[ chr_ID[1] ]]
  dtf_linked$ID_2 <- dtf_linked[[ chr_ID[2] ]]

  dtf_linked$TP_1 <-dtf_linked[[ chr_TP[1] ]]
  dtf_linked$TP_2 <- dtf_linked[[ chr_TP[2] ]]

  dtf_time_by_ID <- dtf_linked |>
    dplyr::group_by(
      ID = ID_1
    ) |> dplyr::summarise(
      Records = length( ID_1 ),
      Distinct = dplyr::n_distinct( ID_2 ),
      TimePoints = unique( TP_1 ),
      Match = all( TP_1 == TP_2 ),
      TimePoint.Mismatch1 = unique(TP_2)[1],
      TimePoint.Mismatch2 = unique(TP_2)[2],
      TimePoint.Mismatch3 = unique(TP_2)[3],
      .groups = 'drop'
    ) |> data.frame()

  return( dtf_time_by_ID )
}
