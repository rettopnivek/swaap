# School-wide assessment linking code
# Written by...
#   Michael Pascale
#   Kevin Potter
# Maintained by...
#   Kevin Potter
# Email:
#   kpotter5@mgh.harvard.edu
# Please email us directly if you
# have any questions or comments
# Last updated: 2025-05-12

# Table of contents
# 1) Internal functions
#   1.1) swaap_link.internal.inputs
#     1.1.1) lst_sets
#     1.1.2) lst_items
#     1.1.3) lst_combos
#     1.1.4) lst_missing
#   1.2) swaap_link.internal.via_dissimilarity
#     1.2.1) Setup
#     1.2.2) Identify rows for linking
#     1.2.3) Match over all records and items
#     1.2.4) Compute dissimilarity scores
#   1.3) swaap_link.internal.via_group_by
#     1.3.1) fun_collapse_rows
#     1.3.2) Setup
#     1.3.3) Match records via items
#     1.3.4) Identify linked rows
#   1.4) swaap_link.internal.assign_IDs
#     1.4.1) Setup
#     1.4.2) Identify links
# 2) swaap_link
#   2.1) Setup
#     2.1.1) fun_copy_prior
#   2.2) Link records
#   2.3) Assign IDs
#   2.4) Final trimming of duplicates
# 3) Helper functions
#   3.1) swaap_link.helper.input
#     3.1.1) Setup
#     3.1.2) Sets
#       3.1.2.1) Default - SBIRT
#       3.1.2.2) Default - survey
#       3.1.2.3) Duplicates - SBIRT
#       3.1.2.4) Duplicates - survey
#     3.1.3) Items
#     3.1.4) Combos
#       3.1.4.1) fun_combos
#     3.1.5) Missing
#       3.1.5.1) fun_missing
#   3.2) swaap_link.helper.parameters
#   3.3) swaap_link.helper.rows
#   3.4) swaap_link.helper.trim
#   3.5) swaap_link.helper.trim_rule
#     3.5.1) List of defined rules
#     3.5.2) Rules for trimming duplicates
#       3.5.2.1) fun_rule.completed
#       3.5.2.2) fun_rule.outcome_and_completed
#     3.5.3) Return specified rule
#   3.6) swaap_link.helper.similarity
# 4) Report functions
#   4.1) swaap_link.report.by_ID
#   4.2) swaap_link.report
#   4.1) Setup
#   #5.2) Linkage patterns [Overall]
#   #5.3) Linkage patterns [Groups]
#   #5.4) Any linked [Overall]
#   4.?) ...
#   4.3) swaap_link.report.discrepant
#     4.3.1) Setup
#     4.3.2) Plotting
#       4.3.2.1) Panel 1
#       4.3.2.2) Panel 2
#       4.3.2.3) Panel 3
#       4.3.2.4) Panel 4

#### 1) Internal functions ####

#### 1.1) swaap_link.internal.inputs ####
# Check Inputs and Define Defaults
#
# Internal function to check inputs for the swaap_link
# function and generate default inputs if necessary.
#
# @param 'dtf_long' A data frame, assumed to be standard processed
#   school-wide assessment data with the columns
#   \code{'SSS.INT.TimePoint'} and \code{'SSS.INT.LongitudinalWave'}
#   as well as the linking code items.
# @param 'obj_input' An R object, the given input to check.
# @param 'chr_type' A character string, either 'lst_sets',
#   'lst_items', 'lst_combos', or 'lst_missing', the
#   type of input to check.
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
# @param 'lgc_progress' A logical value; if TRUE displays the
#   progress of the function.
#
# @author Kevin Potter
#
# @returns A list, structured based on the given desired input.

swaap_link.internal.inputs <- function(
    dtf_long,
    obj_input,
    chr_type,
    lst_sets = NULL,
    lst_items = NULL,
    lst_combos = NULL,
    lgc_progress = FALSE ) {

  #### 1.1.1) lst_sets ####

  # Check input for sets
  if ( chr_type == 'lst_sets' ) {

    # Default input
    if ( is.null(obj_input) ) {

      obj_input <- swaap::swaap_link.helper.input(
        dtf_long,
        'sets',
        lgc_progress = lgc_progress
      )

      # Close 'Default input'
    }

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
    if ( !is.list(obj_input) ) stop(chr_error)

    # Check input validity
    lgc_checks_rows <- rep( FALSE, length(obj_input) )

    # Loop over sets
    for ( s in seq_along(obj_input) ) {

      # Sublist not a list
      if ( !is.list(obj_input[[s]] ) ) stop(chr_error)

      # Sublist has wrong names
      if ( !all( c( 'Base', 'Add' ) %in% names( obj_input[[s]] ) ) )
        stop(chr_error)

      lgc_checks_rows[s] <-
        is.logical( obj_input[[s]]$Base ) &
        is.logical( obj_input[[s]]$Add )

      lgc_checks_rows[s] <-
        length( obj_input[[s]]$Base ) == nrow(dtf_long) &
        length( obj_input[[s]]$Add ) == nrow(dtf_long)

      # Close 'Loop over sets'
    }

    if ( !all(lgc_checks_rows) )
      stop( 'Check that logical vectors for sets are for current data' )

    return( obj_input )

    # Close 'Check input for sets'
  }

  #### 1.1.2) lst_items ####

  # Check input for lst_items
  if ( chr_type == 'lst_items' ) {

    # Default input
    if ( is.null(obj_input) ) {

      obj_input <- dtf_long |>
        swaap::swaap_link.helper.input(
          chr_input = 'items',
          lst_sets = lst_sets,
          lgc_progress = lgc_progress
        )

      # Close 'Default input'
    }

    # Convert character to list
    if ( is.character(obj_input) ) {

      obj_input <- lapply(
        seq_along(lst_sets), function(s) {
          return(obj_input)
        }
      )
      names(obj_input) <- names(lst_sets)

      # Close 'Convert character to list'
    }

    chr_error <-
      paste0(
        "\nArgument 'obj_items' must be a vector of ",
        "column names to use as linking items or be ",
        "in format:\n",
        "list(\n",
        "  <Set> = <Vector of column names>,\n",
        "  ...\n",
        ")\n",
        "\n",
        "Providing a list allows different items to ",
        "be used across different pairs of time points"
      )

    # Make sure is list of lists
    if ( !is.list(obj_input) ) stop(chr_error)

    # Make sure has same number as sets
    if ( length(obj_input) != length(lst_sets) )
      stop( "Argument 'obj_items' must be same length as 'lst_sets'" )

    lgc_in_data <- rep( FALSE, length(obj_input) )

    # Loop over sets
    for ( s in seq_along(obj_input) ) {

      lgc_in_data[s] <- all( obj_input[[s]] %in% colnames(dtf_long) )

      # Close 'Loop over sets'
    }

    if ( !all(lgc_in_data) )
      stop( 'Must provide linking items found in data set' )

    return( obj_input )

    # Close 'Check input for lst_items'
  }

  #### 1.1.3) lst_combos ####

  # Check input for lst_combos
  if ( chr_type == 'lst_combos' ) {

    # Default input
    if ( is.null(obj_input) ) {

      obj_input <- dtf_long |>
        swaap::swaap_link.helper.input(
          chr_input = 'combos',
          obj_extra = lst_items,
          lgc_progress = lgc_progress
        )

      # Close 'Default input'
    }

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
        "matching"
      )

    # Make sure is list of lists
    if ( !is.list(obj_input) ) stop(chr_error)

    # Make sure has same number as sets
    if ( length(obj_input) != length(lst_items) )
      stop( "Argument 'lst_combos' must be same length as 'lst_items'" )

    lgc_in_data <- rep( TRUE, length(obj_input) )

    # Loop over sets
    for ( s in seq_along(obj_input) ) {

      # Check is list of lists
      if ( !is.list( obj_input[[s]] ) ) stop(chr_error)

      # Loop over sublists
      for (l in seq_along( obj_input[[s]] ) ) {

        lgc_in_data[s] <- all(
          obj_input[[s]][[l]] %in% seq_along( lst_items[[s]] )
        )

        # Close 'Loop over sublists'
      }

      # Close 'Loop over sets'
    }

    if ( !all(lgc_in_data) )
      stop( 'Item indices for combos must match items provided' )

    return( obj_input )

    # Close 'Check input for lst_combos'
  }

  #### 1.1.4) lst_missing ####

  # Check input for lst_missing
  if ( chr_type == 'lst_missing' ) {

    # Default input
    if ( is.null(obj_input) ) {

      obj_input <- swaap::swaap_link.helper.input(
        dtf_long,
        'missing',
        obj_extra = list(
          lst_items,
          lst_combos
        ),
        lgc_progress = lgc_progress
      )

      # Close 'Default input'
    }

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

    return( obj_input )

    # Make sure is list of lists
    if ( !is.list(obj_input) ) stop(chr_error)

    # Make sure has same number as sets
    if ( length(obj_input) != length(lst_items) )
      stop( "Argument 'lst_missing' must be same length as 'lst_items'" )

    lgc_in_data <- rep( TRUE, length(obj_input) )

    # Loop over sets
    for ( s in seq_along(obj_input) ) {

      # Check is list of lists
      if ( !is.list( obj_input[[s]] ) ) stop(chr_error)

      # Make sure has same number as combos
      if ( length(obj_input[[s]]) != length(lst_combos[[s]]) )
        stop(
          "Argument 'lst_missing' must have same structure as 'lst_combos"
        )

      # Loop over sublists
      for (l in seq_along( obj_input[[s]] ) ) {

        lgc_in_data[s] <- all(
          obj_input[[s]][[l]] %in% seq_along( lst_items[[s]] )
        )

        # Close 'Loop over sublists'
      }

      # Close 'Loop over sets'
    }

    if ( !all(lgc_in_data) )
      stop( 'Item indices for missing must match items provided' )


    # Close 'Check input for lst_missing'
  }

  stop( '' )
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

  #### 1.3.1) fun_collapse_rows ####
  fun_collapse_rows <- function(
    int_rows ) {

    obj_out <- NA

    # Collapse for 1 to 10 rows
    if ( length( int_rows ) %in% 1:20 ) {

      obj_out <- paste( int_rows, collapse = ',' )

      # Close 'Collapse for 1 to 20 rows'
    }

    return( obj_out )
  }

  #### 1.3.2) Setup ####

  if (lgc_progress) message( '    Identify rows' )

  lgc_all <- rep( TRUE, nrow(dtf_long) )
  chr_items <- unique( unlist( lst_items ) )

  # Loop over sets
  for ( s in seq_along(lst_sets) ) {

    lgc_base <-
      lst_sets[[s]]$Base
    lgc_add <-
      lst_sets[[s]]$Add
    lgc_all[lgc_base | lgc_add] <- TRUE

    # Close 'Loop over sets'
  }

  # Update indicator for attempting linkage
  dtf_long$LNK.LGC.Attempted[lgc_all] <- TRUE
  dtf_long$LNK.CHR.Method[lgc_all] <- 'group_by'

  #### 1.3.3) Match records via items ####

  if (lgc_progress) message( '    Group by items' )

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
        IDN.INT.Row[ SSS.INT.TimePoint %in% 0 ][1],
      RW_1 =
        IDN.INT.Row[ SSS.INT.TimePoint %in% 1 ][1],
      RW_2 =
        IDN.INT.Row[ SSS.INT.TimePoint %in% 2 ][1],
      RW_3 =
        IDN.INT.Row[ SSS.INT.TimePoint %in% 3 ][1],
      RW_4 =
        IDN.INT.Row[ SSS.INT.TimePoint %in% 4 ][1],
      RW_5 =
        IDN.INT.Row[ SSS.INT.TimePoint %in% 5 ][1],
      RW_6 =
        IDN.INT.Row[ SSS.INT.TimePoint %in% 6 ][1],
      RW_7 =
        IDN.INT.Row[ SSS.INT.TimePoint %in% 7 ][1],
      RW_8 =
        IDN.INT.Row[ SSS.INT.TimePoint %in% 8 ][1],
      RW_9 =
        IDN.INT.Row[ SSS.INT.TimePoint %in% 9 ][1],
      ALL =
        fun_collapse_rows(
          IDN.INT.Row
        ),
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

  #### 1.3.4) Identify linked rows ####

  if (lgc_progress) message( '    Identify links' )

  dtf_linked <- dtf_patterns |>
    dplyr::filter(
      Distinct > 1 &
        Missing %in% 0 &
        Distinct == Records
    )

  # Any successful links
  if ( nrow(dtf_linked) > 0 ) {

    # Progress bar
    if ( lgc_progress_bar ) {

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

        dtf_long$LNK.CHR.Rows[int_rows] <-
          paste(
            paste0( int_rows[1], ',', int_rows[-1] ),
            collapse = ';'
          )

        # Close 'If at least two rows'
      }

      # Update the progress bar
      if (lgc_progress_bar)
        setTxtProgressBar(obj_pb, r)

      # Close 'Loop over linked cases'
    }
    if (lgc_progress_bar) close(obj_pb)

    # Close 'Any successful links'
  }

  dtf_long$LNK.CHR.Rows[
    dtf_long$LNK.CHR.Rows != ""
  ] <- paste0(
    dtf_long$LNK.CHR.Rows[
      dtf_long$LNK.CHR.Rows != ""
    ], ";"
  )
  dtf_long$LNK.LGC.Preliminary <-
    dtf_long$LNK.CHR.Rows != ""

  return( dtf_long )
}

#### 1.4) swaap_link.internal.assign_IDs ####
# Assign Identifiers Based on Linkage
#
# @param 'dtf_long' A data frame, assumed to be standard processed
#   school-wide assessment data with the columns
#   \code{'SSS.INT.TimePoint'} and \code{'SSS.INT.LongitudinalWave'}
#   as well as the linking code items. The data frame is
#   assumed to have been run through either the function
#   'swaap_link.internal.via_dissimilarity' or
#   'swaap_link.internal.via_group_by' beforehand.
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

  #### 1.4.1) Setup ####

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

  #### 1.4.2) Identify links ####

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

  # Set up progress bar
  if ( lgc_progress_bar ) {

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
    if (lgc_progress_bar)
      setTxtProgressBar(obj_pb, l)

    # Close 'Loop over possible links'
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
#' @param lst_combos A list of lists, where each sublist consists of
#'   an integer vector indexing the combination of linking items to
#'   consider in order of priority. One sublist of integer vectors must
#'   be defined for each set defined by \code{lst_sets}. For a
#'   given sublist, indices apply to the character vector defined
#'   for the relevant set in \code{obj_items}, meaning that if
#'   character vectors differ across sets, indices should be defined
#'   accordingly.
#' @param lst_missing A list of lists, where each sublist consists of
#'   an integer vector indicating which items should be checked for
#'   missingness when linking using a given combo (therefore
#'   \code{lst_missing} should match in structure to \code{lst_combos}).
#'   When specifying additional items beyond those listing in the given
#'   combo, ensures that if these extra items are non-missing for a
#'   record, the record will not be linked. This behavior can be
#'   suppressed by supplying \code{c()} instead of an integer vector,
#'   which in turn allows for matches with a dissimilarity score
#'   greater than 0.
#' @param fun_trim_duplicates An optional function that returns
#'   a logical vector for the subset of duplicate records
#'   indicating which records should be kept (return \code{TRUE}).
#'   Default is to select record with the highest completion rate.
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
    chr_progress = 'bar' ) {

  if ( chr_progress != '' ) message( 'Start: swaap_link' )

  lgc_progress <- FALSE
  lgc_progress_bar <- TRUE

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

  # Check list of sets
  lst_sets <- swaap:::swaap_link.internal.inputs(
    dtf_long = dtf_long,
    obj_input = lst_sets,
    chr_type = 'lst_sets',
    lgc_progress = lgc_progress
  )

  # Check linking items
  lst_items <- swaap:::swaap_link.internal.inputs(
    dtf_long = dtf_long,
    obj_input = obj_items,
    chr_type = 'lst_items',
    lst_sets = lst_sets,
    lgc_progress = lgc_progress
  )

  # Additional inputs for dissimilarity method
  if ( chr_method != 'group_by' ) {

    # Check combos
    lst_combos <- swaap:::swaap_link.internal.inputs(
      dtf_long = dtf_long,
      obj_input = lst_combos,
      chr_type = 'lst_combos',
      lst_sets = lst_sets,
      lst_items = lst_items,
      lgc_progress = lgc_progress
    )

    # Check missing
    lst_missing <- swaap:::swaap_link.internal.inputs(
      dtf_long = dtf_long,
      obj_input = lst_missing,
      chr_type = 'lst_missing',
      lst_sets = lst_sets,
      lst_items = lst_items,
      lst_combos = lst_combos,
      lgc_progress = lgc_progress
    )

    # Close 'Additional inputs for dissimilarity method'
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
  # Copy old linked rows
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
  # Indicator for whether linking attempted
  dtf_long$LNK.LGC.Attempted <- FALSE
  # Type of link method
  dtf_long$LNK.CHR.Method <- ''
  dtf_long$LNK.CHR.Rows <- ''
  dtf_long$LNK.LGC.Preliminary <- FALSE
  dtf_long$LNK.LGC.Duplicates <- FALSE
  dtf_long$LNK.LGC.NoIssues <- FALSE
  dtf_long$LNK.CHR.AttributeWithParameters <-
    'attributes(...)$swaap.inputs_for_linking'

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
        lst_items = lst_items
      )
    dtf_linked <<- dtf_long

    # Close 'Link using group_by method'
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

  #### 2.4) Final trimming of duplicates ####

  # If specified
  if ( lgc_remove_duplicates ) {

    if (lgc_progress) message( '  Trim duplicates' )

    if ( is.null(fun_trim_duplicates) )
      fun_trim_duplicates <- swaap::swaap_link.helper.trim_rule(
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
      swaap::swaap_link.helper.trim(
        fun_rule = fun_trim_duplicates
      )

    # Incorporate previous duplicate info
    if ( lgc_update ) {

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
      lst_combos = ifelse(
        chr_method == 'dissimilarity',
        lst_combos,
        NULL
      ),
      lst_missing = ifelse(
        chr_method == 'dissimilarity',
        lst_missing,
        NULL
      ),
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


#### 3) Helper functions ####

#### 3.1) swaap_link.helper.input ####
#' Function to Help Create Input for Linking Function
#'
#' Helper function to generate input to pass to the [swaap::swaap_link]
#' function.
#'
#' @param dtf_long A data frame, assumed to be standard processed
#'   school-wide assessment data with the columns
#'   \code{'SSS.INT.TimePoint'} and \code{'SSS.INT.LongitudinalWave'}
#'   as well as the linking code items.
#' @param chr_input A character string indicating the type of input to
#'   create, either \code{'sets'}, \code{'items'}, \code{'combos'},
#'   or \code{'missing'}.
#' @param lst_sets An optional argument, include to aid in defining
#'   items. A list of lists, with each sublist specifying
#'   \code{'Base'} and \code{'Add'} logical vectors for the pair of data
#'   subsets in \code{dtf_long} to link over (e.g., \code{'Base'} would
#'   subset the first time point and \code{'Add'} would subset the second
#'   time point). Include to aid in defining items, combos, and
#'   missingness treatment.
#' @param obj_extra Additional input needed to define combos and
#'   treatment of missingness. For combos, either a character vector
#'   or list of the items; for missingness, a list with both the
#'   items and combos input.
#' @param lgc_progress A logical value; if\code{TRUE} tracks function
#'   progress.
#'
#' @author Kevin Potter
#'
#' @returns A list structured in the relevant way for the given output.
#'
#' @examples
#' dtf_long <- swaap_simulate( 'link', 'demo' )
#'
#' lst_sets <- dtf_long |>
#'   swaap_link.helper.input( 'sets' )
#' lst_items <- dtf_long |>
#'   swaap_link.helper.input( 'items', lst_sets = lst_sets )
#' lst_combos <- dtf_long |>
#'   swaap_link.helper.input( 'combos', obj_extra = lst_items )
#' lst_missing <- dtf_long |>
#'   swaap_link.helper.input(
#'     'missing', obj_extra = list( lst_items, lst_combos )
#'   )
#'
#' @export

swaap_link.helper.input <- function(
    dtf_long,
    chr_input,
    lst_sets = NULL,
    obj_extra = NULL,
    lgc_progress = FALSE ) {

  if (lgc_progress) 'Start: swaap_link.helper.input'

  #### 3.1.1) Setup ####

  lst_inputs <- list(
    sets = c(
      'sets',
      'lst_sets',
      'link_over',
      'link over',
      'pairs'
    ),
    items = c(
      'items',
      'lst_items',
      'linking items',
      'link items',
      'linking questions',
      'link questions',
      'link_using',
      'link using'
    ),
    combos = c(
      'combos',
      'lst_combos',
      'combo',
      'item combos',
      'item combo'
    ),
    missing = c(
      'missing',
      'lst_missing',
      'missing items'
    )
  )

  #### 3.1.2) Sets ####

  # Create input
  if ( chr_input %in% lst_inputs$sets ) {

    if (lgc_progress) '  Input: Sets'

    # No extra details
    if ( is.null(obj_extra) ) {

      #### 3.1.2.1) Default - SBIRT ####

      # Check if only SBIRT sample
      lgc_SBIRT <- FALSE

      # SBIRT variables found
      if ( 'SSS.LGC.SBIRT' %in% colnames(dtf_long) ) {

        if ( all( dtf_long$SSS.LGC.SBIRT ) ) lgc_SBIRT <- TRUE

        # Close 'SBIRT variables found'
      }

      # If only SBIRT sample
      if ( lgc_SBIRT ) {

        if (lgc_progress) '    SBIRT'

        # Group by recruitment wave, time point, and grade level

        int_WV <-
          dtf_long$SSS.INT.RecruitmentWave |>
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
                dtf_long$SSS.INT.RecruitmentWave %in% int_WV[w] &
                dtf_long$SSS.INT.SBIRTTimePoint %in% mat_TM[p, 1] &
                dtf_long$SSS.INT.Grade %in% ( mat_GR[p, 1] + g )
              lgc_add <-
                dtf_long$SSS.INT.RecruitmentWave %in% int_WV[w] &
                dtf_long$SSS.INT.SBIRTTimePoint %in% mat_TM[p, 2] &
                dtf_long$SSS.INT.Grade %in% ( mat_GR[p, 2] + g )

              # If subsets exist
              if ( any(lgc_base) & any(lgc_add) ) {

                lst_sets[[int_inc]] <- list(
                  Base = lgc_base,
                  Add = lgc_add
                )
                chr_names[int_inc] <- paste0(
                  'W', int_WV[w],
                  'T', mat_TM[p, 1],
                  'G', mat_GR[p, 1],
                  't',
                  'W', int_WV[w],
                  'T', mat_TM[p, 2],
                  'G', mat_GR[p, 2]
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

      #### 3.1.2.2) Default - survey ####

      if (lgc_progress) '    Survey'

      int_times <-
        dtf_long$SSS.INT.TimePoint |>
        unique() |>
        sort()
      int_waves <-
        dtf_long$SSS.INT.LongitudinalWave |>
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
            dtf_long$SSS.INT.TimePoint %in% mat_pairs[p, 1] &
            dtf_long$SSS.INT.LongitudinalWave %in% int_waves[w]
          lgc_add <-
            dtf_long$SSS.INT.TimePoint %in% mat_pairs[p, 2] &
            dtf_long$SSS.INT.LongitudinalWave %in% int_waves[w]

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

      # Close 'No extra details'
    } else {

      # Create sets for checking for duplicates
      if ( 'duplicates' %in% obj_extra ) {

        #### 3.1.2.3) Duplicates - SBIRT ####

        # Check if only SBIRT sample
        lgc_SBIRT <- FALSE

        # SBIRT variables found
        if ( 'SSS.LGC.SBIRT' %in% colnames(dtf_long) ) {

          if ( all( dtf_long$SSS.LGC.SBIRT ) ) lgc_SBIRT <- TRUE

          # Close 'SBIRT variables found'
        }

        # If only SBIRT sample
        if ( lgc_SBIRT ) {

          if (lgc_progress) '    SBIRT'

          # Group by recruitment wave, time point, and grade level

          int_WV <-
            dtf_long$SSS.INT.RecruitmentWave |>
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
                  dtf_long$SSS.INT.RecruitmentWave %in% int_WV[w] &
                  dtf_long$SSS.INT.SBIRTTimePoint %in% mat_sets[p, 1] &
                  dtf_long$SSS.INT.Grade %in% ( mat_sets[p, 2] + g )
                lgc_add <-
                  dtf_long$SSS.INT.RecruitmentWave %in% int_WV[w] &
                  dtf_long$SSS.INT.SBIRTTimePoint %in% mat_sets[p, 1] &
                  dtf_long$SSS.INT.Grade %in% ( mat_sets[p, 2] + g )

                # If subsets exist
                if ( any(lgc_base) & any(lgc_add) ) {

                  lst_sets[[int_inc]] <- list(
                    Base = lgc_base,
                    Add = lgc_add
                  )
                  chr_names[int_inc] <- paste0(
                    'W', int_WV[w],
                    'T', mat_sets[p, 1],
                    'G', mat_sets[p, 2]
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

        #### 3.1.2.4) Duplicates - survey ####

        int_times <-
          dtf_long$SSS.INT.TimePoint |>
          unique() |>
          sort()
        int_waves <-
          dtf_long$SSS.INT.LongitudinalWave |>
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
              dtf_long$SSS.INT.TimePoint %in% int_times[p] &
              dtf_long$SSS.INT.LongitudinalWave %in% int_waves[w]
            lgc_add <-
              dtf_long$SSS.INT.TimePoint %in% int_times[p] &
              dtf_long$SSS.INT.LongitudinalWave %in% int_waves[w]

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

        return( lst_sets )

        # Close 'Create sets for checking for duplicates'
      }

      if ( 'code' %in% obj_extra ) {

      }

      # Close else for 'No extra details'
    }

    # Close 'Create input'
  }

  #### 3.1.3) Items ####

  # Create input
  if ( chr_input %in% lst_inputs$items ) {

    if (lgc_progress) '  Input: Items'

    chr_columns <- colnames(dtf_long)

    chr_linking <- NULL

    chr_linking_clean <-
      swaap::swaap_select.linking()
    chr_linking_orig <-
      swaap::swaap_select.linking( lgc_original = TRUE )

    lgc_clean <- any(
      chr_linking_clean[
        !chr_linking_clean %in% chr_linking_orig
      ] %in% chr_columns
    )

    # If any linking items exist [Clean]
    if ( lgc_clean ) {

      chr_linking <- chr_linking_clean[
        chr_linking_clean %in% chr_columns
      ]

      # Close 'If any linking items exist [Clean]'
    }

    lgc_orig <- any(
      chr_linking_orig %in% chr_columns
    )

    # If any linking items exist [Original]
    if ( !lgc_clean & lgc_orig ) {

      chr_linking <- chr_linking_orig[
        chr_linking_orig %in% chr_columns
      ]

      # Close 'If any linking items exist [Original]'
    }

    # No items found
    if ( is.null(chr_linking) ) {

      stop( 'No standard linking items found in data' )

      # Close 'No items found'
    }

    # If sets provided
    if ( !is.null(lst_sets) ) {

      lst_items <- lapply(
        seq_along(lst_sets), function(s) {
          return(
            chr_linking
          )
        }
      )
      names( lst_items ) <- names( lst_sets )

      # Loop through sets
      for ( s in seq_along(lst_sets) ) {

        lgc_base <-
          lst_sets[[s]]$Base
        lgc_add <-
          lst_sets[[s]]$Add

        lgc_base_NA <- apply(
          dtf_long[lgc_base, lst_items[[s]]],
          2, function(x) all( is.na(x) )
        )
        lgc_add_NA <- apply(
          dtf_long[lgc_add, lst_items[[s]]],
          2, function(x) all( is.na(x) )
        )

        # If any variables are NA for all cases
        if ( any(lgc_base_NA) | any(lgc_add_NA) ) {

          # Keep items with non-NA cases
          lst_items[[s]] <- lst_items[[s]][
            !lgc_base_NA & !lgc_add_NA
          ]

          # Close 'If any variables are NA for all cases'
        }

        # Close 'Loop through sets'
      }

      # Close 'If sets provided'
    } else {

      return( chr_linking )

      # Close else for 'If sets provided'
    }

    return( lst_items )

    # Close 'Create input'
  }


  #### 3.1.4) Combos ####

  # Create input
  if ( chr_input %in% lst_inputs$combos ) {

    if (lgc_progress) '  Input: Combos'

    #### 3.1.4.1) fun_combos ####
    fun_combos <- function(
      chr_items ) {

      lst_combos <- list()

      # Standard linking items
      chr_linking_items <-
        swaap::swaap_select.linking()
      # Standard linking items [Original]
      chr_linking_items_orig <-
        swaap::swaap_select.linking( lgc_original = TRUE )

      chr_diff <-
        chr_linking_items[
          !chr_linking_items %in% chr_linking_items_orig
        ]

      if ( !any(chr_diff %in% chr_items) )
        chr_linking_items <- chr_linking_items_orig

      chr_SI <- chr_linking_items[1:2]
      chr_S <- chr_linking_items[1]
      chr_I <- chr_linking_items[2]
      chr_LQ <- chr_linking_items[-(1:2)]

      # If school code provided
      if ( chr_S %in% chr_items ) {

        # Add school code + ID
        if ( all( chr_SI %in% chr_items ) ) {

          lst_combos$SI <- which(
            chr_items %in% chr_SI
          )

          # Close 'Add school code + ID'
        }

        # Add school code + LQ
        if ( all( chr_LQ %in% chr_items ) ) {

          int_patterns <- 1:7

          lst_combos[[
            paste0( 'SQ', paste( int_patterns, collapse = '' ) )
          ]] <- which(
            chr_items %in% c( chr_S, chr_LQ )
          )

          # Loop over individual items
          for ( i in seq_along(chr_LQ) ) {

            chr_cur <- chr_LQ[-i]
            chr_slot <- paste0(
              'SQ',
              paste(
                which(
                  chr_items[
                    chr_items %in% chr_LQ
                  ] %in% chr_cur
                ),
                collapse = ''
              )
            )

            lst_combos[[ chr_slot ]] <- which(
              chr_items %in% c( chr_S, chr_cur )
            )

            # Close 'Loop over individual items'
          }

          # Close 'Add school code + LQ'
        }

        # If fewer than 7 linking questions
        if ( sum(chr_LQ %in% chr_items) %in% 1:6 ) {

          chr_slot <- paste0(
            'SQ',
            paste(
              which(
                chr_items[
                  chr_items %in% chr_LQ
                ] %in% chr_LQ
              ),
              collapse = ''
            )
          )

          lst_combos[[ chr_slot ]] <- which(
            chr_items %in% c( chr_S, chr_LQ )
          )

          # Close 'If fewer than 7 linking questions'
        }

        # Close 'If school code provided'
      } else {

        # Add school ID
        if ( chr_I %in% chr_items ) {

          lst_combos$I <- which(
            chr_items %in% chr_I
          )

          # Close 'Add school ID'
        }

        # Add LQ
        if ( all( chr_LQ %in% chr_items ) ) {

          int_patterns <- 1:7

          lst_combos[[
            paste0( 'Q', paste( int_patterns, collapse = '' ) )
          ]] <- which(
            chr_items %in% chr_LQ
          )

          # Loop over individual items
          for ( i in seq_along(chr_LQ) ) {

            chr_cur <- chr_LQ[-i]
            chr_slot <- paste0(
              'Q',
              paste(
                which(
                  chr_items[
                    chr_items %in% chr_LQ
                  ] %in% chr_cur
                ),
                collapse = ''
              )
            )

            lst_combos[[ chr_slot ]] <- which(
              chr_items %in% chr_cur
            )

            # Close 'Loop over individual items'
          }

          # Close 'Add school code + LQ'
        }

        # If fewer than 7 linking questions
        if ( sum(chr_LQ %in% chr_items) %in% 1:6 ) {

          chr_slot <- paste0(
            'Q',
            paste(
              which(
                chr_items[
                  chr_items %in% chr_LQ
                ] %in% chr_LQ
              ),
              collapse = ''
            )
          )

          lst_combos[[ chr_slot ]] <- which(
            chr_items %in% chr_LQ
          )

          # Close 'If fewer than 7 linking questions'
        }

        # Close else for 'If school code provided'
      }

      # If no known matches
      if ( length(lst_combos) == 0 )
        lst_combos$All <- seq_along(chr_items)

      return( lst_combos )
    }

    # Must provided items
    if ( is.null(obj_extra) )
      stop( "Provide column names for linking items via 'obj_extra'" )

    # If character vector
    if ( is.character(obj_extra) ) {

      lst_combos <- fun_combos(
        obj_extra
      )

      # Expand by sets
      if ( !is.null(lst_sets) ) {

        lst_combos <- lapply(
          seq_along(lst_sets), function(s) {
            return( lst_combos )
          }
        )
        names(lst_combos) <- names(lst_sets)

        # Close 'Expand by sets'
      }

      return( lst_combos )

      # Close 'If character vector'
    }

    # If list
    if ( is.list(obj_extra) ) {

      lst_combos <- lapply(
        seq_along(obj_extra), function(s) {
          fun_combos(obj_extra[[s]])
        }
      )
      names(lst_combos) <- names(obj_extra)

      return( lst_combos )

      # Close 'If list'
    }

    stop(
      "Argument 'obj_extra' must be a character vector or list"
    )

    # Close 'Create input'
  }

  #### 3.1.5) Missing ####

  # Create input
  if ( chr_input %in% lst_inputs$missing ) {

    if (lgc_progress) '  Input: Missing'

    #### 3.1.5.1) fun_missing ####
    fun_missing <- function(
      chr_items,
      lst_combos ) {

      # Initialize output
      lst_missing <- lst_combos

      # Standard linking items
      chr_linking_items <-
        swaap::swaap_select.linking()
      # Standard linking items [Original]
      chr_linking_items_orig <-
        swaap::swaap_select.linking( lgc_original = TRUE )

      chr_diff <-
        chr_linking_items[
          !chr_linking_items %in% chr_linking_items_orig
        ]

      if ( !any(chr_diff %in% chr_items) )
        chr_linking_items <- chr_linking_items_orig

      chr_SI <- chr_linking_items[1:2]
      chr_S <- chr_linking_items[1]
      chr_I <- chr_linking_items[2]
      chr_LQ <- chr_linking_items[-(1:2)]

      # Loop over combos
      for ( l in seq_along(lst_combos) ) {

        # Current items
        chr_cur <- chr_items[ lst_combos[[l]] ]

        # Default is to check if all items missing
        lst_missing[[l]] <- seq_along(chr_items)

        # Check against known combos

        # If school code provided
        if ( chr_S %in% chr_items ) {

          # School code + ID
          if ( all( chr_SI %in% chr_items ) ) {

            # Match to combo
            if ( all( chr_cur %in% chr_SI ) ) {

              lst_missing[[l]] <- which(
                chr_items %in% chr_SI
              )

              # Close 'Match to combo'
            }

            # Close 'School code + ID'
          }

          # If linking questions provided
          if ( all(chr_LQ %in% chr_items) ) {

            # If at least 6 linking questions
            if ( sum( chr_cur %in% chr_LQ ) > 5 ) {

              # Use school code, ID, and all linking questions
              lst_missing[[l]] <- which(
                chr_items %in% c( chr_SI, chr_LQ )
              )

              # Close 'If at least 6 linking questions'
            }

            # Close 'If linking questions provided'
          }

          # Close 'If school code provided'
        } else {

          # School ID
          if ( chr_I %in% chr_items ) {

            # Match to combo
            if ( all( chr_cur %in% chr_I ) ) {

              lst_missing[[l]] <- which(
                chr_items %in% chr_I
              )

              # Close 'Match to combo'
            }

            # Close 'School ID'
          }

          # If linking questions provided
          if ( all(chr_LQ %in% chr_items) ) {

            # If at least 6 linking questions
            if ( sum( chr_cur %in% chr_LQ ) > 5 ) {

              # Use school ID and all linking questions
              lst_missing[[l]] <- which(
                chr_items %in% c( chr_I, chr_LQ )
              )

              # Close 'If at least 6 linking questions'
            }

            # Close 'If linking questions provided'
          }

          # Close 'If school code provided'
        }

        # Close 'Loop over combos'
      }

      return( lst_missing )
    }

    chr_error <- paste0(
      "Provide list with [[1]] linking items and [[2]] ",
      "list of combinations"
    )

    # Must provided extra details
    if ( is.null(obj_extra) )
      stop( chr_error )

    if ( !is.list(obj_extra) )
      stop( chr_error )

    if ( length(obj_extra) != 2 )
      stop( chr_error )

    # If character vector
    if ( is.character(obj_extra[[1]]) ) {

      lgc_indices <- all(
        unique( obj_extra[[2]] |> unlist() ) %in% seq_along( obj_extra[[1]] )
      )
      if ( !lgc_indices )
        stop( "Second element of 'obj_extra' should be list of combinations" )

      lst_missing <- fun_missing(
        obj_extra[[1]],
        obj_extra[[2]]
      )

      # Close 'If character vector'
    } else {

      lst_missing <- obj_extra[[1]]

      # Loop over sets
      for ( s in seq_along(obj_extra[[1]]) ) {

        lst_1 <- obj_extra[[1]][[s]]
        lst_2 <- obj_extra[[2]][[s]]

        lgc_indices <- all(
          unique( lst_2 |> unlist() ) %in% seq_along( lst_1 )
        )
        if ( !lgc_indices )
          stop( "Second element of 'obj_extra' should be list of combinations" )

        lst_missing[[s]] <- fun_missing(
          lst_1,
          lst_2
        )

        # Close 'Loop over sets'
      }

      # Close else for 'If character vector'
    }

    return( lst_missing )

    # Close 'Create input'
  }

  if (lgc_progress) '--End: swaap_link.helper.input'

  chr_error <- paste0(
    "Check inputs as no processing was done. Argument ",
    "'chr_input' should be either 'sets', 'items', ",
    "'combos', or 'missing'"
  )

  stop(chr_error)
}


#### 3.2) swaap_link.helper.parameters ####
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

swaap_link.helper.parameters <- function(
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

#### 3.3) swaap_link.helper.rows ####
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

swaap_link.helper.rows <- function(
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

#### 3.4) swaap_link.helper.trim ####
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

swaap_link.helper.trim <- function(
    dtf_long,
    fun_rule = NULL,
    lst_arg = NULL ) {

  # Initialize column
  dtf_long$QLT.LGC.RemoveDuplicate <- FALSE

  # Default rule for resolving duplicates
  if ( is.null(fun_rule) ) {

    fun_rule <- swaap_link.helper.trim_rule(
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


#### 3.5) swaap_link.helper.trim_rule ####
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

swaap_link.helper.trim_rule <- function(
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

  #### 3.5.2) Rules for trimming duplicates ####

  #### 3.5.2.1) fun_rule.completed ####
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

  #### 3.5.2.2) fun_rule.outcome_and_completed ####
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

  #### 3.5.2.3) fun_rule.duplicate_times ####
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

  #### 3.5.3) Return specified rule ####

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

#### 3.6) swaap_link.helper.similarity ####
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

swaap_link.helper.similarity <- function(
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

#### 4) Report functions ####

#### 4.1) swaap_link.report.by_ID ####
#' Linkage Patterns by Linked Identifier
#'
#' Function to determine the pattern of linked time
#' points for each linked ID.
#'
#' @param dtf_linked A data frame, output from the
#'   [swaap::swaap_link] function. Must have the columns
#'   \code{IDN.CHR.Linked.ID} and \code{SSS.INT.TimePoint}.
#'
#' @author Kevin Potter
#'
#' @returns A wide-form data frame with one row
#' per linked ID along with the pattern of linked
#' time points (e.g., \code{'0-1'} means a link
#' from baseline to the first time point).
#'
#' @export

swaap_link.report.by_ID <- function(
    dtf_linked,
    lgc_update = FALSE ) {

  dtf_IDs <- dtf_linked |>
    dplyr::group_by(
      IDN.CHR.Linked.ID
    ) |>
    dplyr::summarise(
      LNK.CHR.TimePoints = paste(
        sort( SSS.INT.TimePoint ), collapse = '-'
      ),
      LNK.LGC.AnyDuplicates = any(
        LNK.LGC.Duplicates
      ),
      .groups = 'drop'
    ) |>
    data.frame()

  # Patterns per time point
  chr_TP <- sort( unique( dtf_linked$SSS.INT.TimePoint) )

  mat_TP <- matrix(
    0, nrow(dtf_IDs), length( chr_TP )
  )
  colnames( mat_TP ) <- paste0(
    'LNK.INT.Records.TP.', chr_TP
  )

  # Loop over each time point
  for ( j in 1:ncol(mat_TP) ) {

    lgc_any <- grepl(
      chr_TP[j], dtf_IDs$LNK.CHR.TimePoints,
      fixed = TRUE
    )

    mat_TP[lgc_any, j] <-
      dtf_IDs$LNK.CHR.TimePoints[
        lgc_any
      ] |> sapply(
        function(x) {
          grepl( '-', strsplit( x, '' )[[1]], fixed = TRUE ) |> sum()
        }
      ) + 1

    # Close 'Loop over each time point'
  }

  dtf_IDs <- cbind( dtf_IDs, mat_TP )

  # Update original data set with linkage patterns
  if ( lgc_update ) {

    dtf_linked$LNK.CHR.TimePoints <- sapply(
      1:nrow(dtf_linked), function(r) {

        chr_out <- ''

        lgc_rows <-
          dtf_IDs$IDN.CHR.Linked.ID %in%
          dtf_linked$IDN.CHR.Linked.ID[r]

        # Return pattern
        if ( any(lgc_rows) ) {

          chr_out <- dtf_IDs$LNK.CHR.TimePoints[
            lgc_rows
          ]

          # Close 'Return pattern'
        }

        return( chr_out )
      }
    )

    return( dtf_linked )

    # Close 'Update original data set with linkage patterns'
  }

  return( dtf_IDs )
}

#### 4.2) swaap_link.report ####
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

  #### 4.1) Setup ####

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

  #### 4.?) ... ####

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

#### 4.3) swaap_link.report.discrepant ####
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

  #### 4.3.1) Setup ####

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

  #### 4.3.2) Plotting ####

  # Generate plot
  if (lgc_plot) {

    chr_terms = c(
      "Stable",
      "Discrepant",
      "discrepant"
    )

    chr_labels <- sapply(
      chr_items, function(s) {
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

    #### 4.3.2.1) Panel 1 ####

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

    #### 4.3.2.2) Panel 2 ####

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

    #### 4.3.2.3) Panel 3 ####

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

    #### 4.3.2.4) Panel 4 ####

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

