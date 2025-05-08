# Misc. data processing functions
# Written by Kevin Potter
# email: kpotter5@mgh.harvard.edu
# Please email me directly if you
# have any questions or comments
# Last updated 2025-05-08

#### 1) swaap_data.merge ####
#' Merge Multiple Data Sets
#'
#' Functions to merge multiple school-wide
#' assessment data sets into single long-form
#' data set.
#'
#' @param lst_data A list of data frames, assumed
#'   to be standard school-wide assessment survey
#'   data. Can also be \code{NULL}, in which case
#'   templates for the \code{chr_add}, \code{chr_recode},
#'   and \code{chr_select} variables are provided.
#' @param chr_add A character vector, the \code{swaap_add}
#'   functions to run. Either takes the full function
#'   name, or as a shorthand the part of the function
#'   name following the period (e.g., instead of
#'   \code{'swaap_add.school_enrollment'} you can just
#'   use \code{'school_enrollment'}).
#' @param chr_recode A character vector, the \code{swaap_recode}
#'   functions to run. Either takes the full function
#'   name, or as a shorthand the part of the function
#'   name following the period (e.g., instead of
#'   \code{'swaap_recode.base'} you can just
#'   use \code{'base'}).
#' @param chr_select A character vector, the \code{swaap_select}
#'   functions to run. Either takes the full function
#'   name, or as a shorthand the part of the function
#'   name following the period (e.g., instead of
#'   \code{'swaap_select.base'} you can just
#'   use \code{'base'}).
#' @param lgc_SBIRT A logical value; if \code{TRUE}
#'   restricts the output only to the SBIRT sample.
#' @param lgc_remove_flagged A logical value; if \code{TRUE}
#'   remove records that fail initial quality checks
#'   (see [swaap::swaap_add.quality_checks]).
#' @param lgc_progress A logical value; if \code{TRUE}
#'   displays the function's progress.
#'
#' @returns A data frame.
#'
#' @export

swaap_data.merge <- function(
    lst_data,
    chr_add = '',
    chr_recode = '',
    chr_select = '',
    lgc_SBIRT = FALSE,
    lgc_remove_flagged = TRUE,
    lgc_progress = TRUE ) {

  # List available functions
  if ( is.null(lst_data) ) {

    message("Templates for arguments:" )
    message("")

    # Add functions
    message( "chr_add = c(" )
    message( "  'school_enrollment'," )
    message( "  'year_and_semester'," )
    message( "  'SBIRT_sample'," )
    message( "  'subtance_use'," )
    message( "  'quality_checks'" )
    message( ")" )
    message("")

    # Recode functions
    message( "chr_recode = c(" )
    message( "  'base'," )
    message( "  'contact'," )
    message( "  'demographics'," )
    message( "  'experience'," )
    message( "  'inventories'," )
    message( "  'linking'," )
    message( "  'quality'" )
    message( ")" )
    message("")

    # Select functions
    message( "chr_select = c(" )
    message( "  'base'," )
    message( "  'contact_info'," )
    message( "  'demographics'," )
    message( "  'experience'," )
    message( "  'inventories'" )
    message( "  'linking'," )
    message( "  'quality'," )
    message( "  'SBIRT'," )
    message( "  'substance_use'" )
    message( ")" )
    message("")


    return(NULL)

    # Close 'List available functions'
  }

  if ( lgc_progress ) message( 'Start: swaap_merge' )

  # Default add functions
  if ( all( chr_add == '' ) ) {

    chr_add <- c(
      'swaap_add.school_enrollment',
      'swaap_add.year_and_semester',
      'swaap_add.SBIRT_sample',
      'swaap_add.substance_use',
      'swaap_add.quality_checks'
    )

    # Close 'Default add functions'
  }

  # Check if function header is needed
  lgc_no_header <- grepl(
    'swaap_add', chr_add, fixed = TRUE
  )
  if ( any( !lgc_no_header) )
    chr_add[ !lgc_no_header ] <-
    paste0( 'swaap_add.', chr_add[!lgc_no_header] )

  # Default recode functions
  if ( all( chr_recode == '' ) ) {

    chr_recode <- c(
      'swaap_recode.base',
      'swaap_recode.contact',
      'swaap_recode.demographics',
      'swaap_recode.experience',
      'swaap_recode.inventories',
      'swaap_recode.linking',
      'swaap_recode.quality'
    )

    # Close 'Default add functions'
  }

  # Check if function header is needed
  lgc_no_header <- grepl(
    'swaap_recode', chr_recode, fixed = TRUE
  )
  if ( any( !lgc_no_header) )
    chr_recode[ !lgc_no_header ] <-
    paste0( 'swaap_recode.', chr_recode[!lgc_no_header] )

  # Default select functions
  if ( all( chr_select == '' ) ) {

    chr_select <- c(
      'swaap_select.base',
      'swaap_select.contact_info',
      'swaap_select.demographics',
      'swaap_select.linking',
      'swaap_select.SBIRT',
      'swaap_select.substance_use',
      'swaap_select.experience',
      'swaap_select.inventories',
      'swaap_select.quality'
    )

    # Close 'Default add functions'
  }

  # Check if function header is needed
  lgc_no_header <- grepl(
    'swaap_select', chr_select, fixed = TRUE
  )
  if ( any( !lgc_no_header) )
    chr_select[ !lgc_no_header ] <-
    paste0( 'swaap_select.', chr_select[!lgc_no_header] )

  # Loop over data sets
  for ( d in seq_along( lst_data ) ) {

    if ( lgc_progress ) message( paste0( '  Data set ', d ) )

    dtf_current <- lst_data[[ d ]]

    if ( lgc_progress ) message( '    Add functions' )

    # Loop over add functions
    for ( j in seq_along( chr_add ) ) {

      if ( lgc_progress ) message( paste0( '      ', chr_add[j] ) )

      # Adding substances
      if ( 'swaap_add.substance_use' %in% chr_add[j] ) {

        dtf_current <- do.call(
          chr_add[j],
          list( dtf_data = dtf_current,
                chr_substance = 'Alcohol' )
        )

        dtf_current <- do.call(
          chr_add[j],
          list( dtf_data = dtf_current,
                chr_substance = 'Cannabis' )
        )

        dtf_current <- do.call(
          chr_add[j],
          list( dtf_data = dtf_current,
                chr_substance = 'Vapes' )
        )

        # Close 'Adding substances'
      } else {

        dtf_current <- do.call(
          chr_add[j],
          list( dtf_data = dtf_current )
        )

        # Close else for 'Adding substances'
      }

      # Close 'Loop over add functions'
    }

    if ( lgc_progress ) message( '    Recode functions' )

    # Loop over recode functions
    for ( j in seq_along( chr_recode ) ) {

      if ( lgc_progress ) message( paste0( '      ', chr_recode[j] ) )

      dtf_current <- do.call(
        chr_recode[j],
        list( dtf_data = dtf_current )
      )

      # Close 'Loop over recode functions'
    }

    if ( lgc_progress ) message( '    Select functions' )

    chr_columns <- ''

    # Loop over select functions
    for ( j in seq_along( chr_select ) ) {

      if ( lgc_progress ) message( paste0( '      ', chr_select[j] ) )

      chr_columns <- do.call(
        chr_select[j],
        list( chr_input = chr_columns )
      )

      # Close 'Loop over recode functions'
    }

    dtf_current <- dtf_current |>
      swaap_select( chr_columns )

    # If not first data set
    if ( d > 1 ) {

      chr_long <- colnames(dtf_long)

      # Add missing columns if needed
      if ( any( !chr_long %in% colnames(dtf_current) ) ) {

        chr_missing <- chr_long[
          !chr_long %in% colnames(dtf_current)
        ]

        # Loop over missing columns
        for ( k in seq_along(chr_missing) ) {

          dtf_current[[ chr_missing[k] ]] <- NA

          # Close 'Loop over missing columns'
        }

        # Close 'Add missing columns if needed'
      }

      # Close 'Add missing columns if needed'
    }

    # Add row for original data set
    dtf_current$IDN.INT.OriginalRow <- 1:nrow(dtf_current)
    # Add index for data set
    dtf_current$IDN.INT.DataSet <- d

    # Initialize data
    if ( d == 1 ) {

      dtf_long <- dtf_current

      # Close 'Initialize data'
    } else {

      # Add current data
      dtf_long <- rbind(
        dtf_long,
        dtf_current
      )

      # Close else for 'Initialize data'
    }

    # Close 'Loop over data sets'
  }

  # Restrict to SBIRT sample
  if (lgc_SBIRT)
    dtf_long <- dtf_long |> dplyr::filter( SSS.LGC.SBIRT )

  # Remove flagged records
  if (lgc_remove_flagged) {

    # If variable found
    if ( 'QLT.LGC.Remove' %in% colnames(dtf_long) ) {

      dtf_long <- dtf_long |> dplyr::filter( !QLT.LGC.Remove )

      # Close 'If variable found'
    }

    # Close 'Remove flagged records'
  }

  # Add time points once data have been merged
  dtf_long <- dtf_long |>
    swaap_add.time_point()

  if ( lgc_progress ) message( '--End: swaap_merge' )

  return( dtf_long )
}

#### 2) swaap_data.missing ####
#' Tally NA Values Over Columns
#'
#' Function that tallies the number
#' of \code{NA} values per row over
#' columns in a data set.
#'
#' @param dtf_data A data frame.
#' @param chr_columns An optional character vector,
#'   the subset of columns in \code{dtf_data} to
#'   consider. If not provided, all columns are
#'   considered.
#' @param chr_new A character string,
#'   the column name to add to \code{dtf_data}
#'   with the desired output. If \code{''}
#'   (default) no column is added.
#' @param chr_output A character string, the
#'   type of output to return. If \code{'count'}
#'   returns the total number of NA values per
#'   row; if \code{'proportion'} returns the
#'   proportion of NA values per row; if
#'   \code{'pattern'} returns a character
#'   string of 0s and 1s per row where
#'   1s denote NA values.
#'
#' @returns Either a vector or a data frame.
#'
#' @export

swaap_data.missing <- function(
    dtf_data,
    chr_columns = NULL,
    chr_new = '',
    chr_output = 'count' ) {

  if ( is.null(chr_columns) )
    chr_columns <- colnames(dtf_data)

  # Count of total missing
  if ( chr_output %in% c( 'total', 'count', 'n' ) ) {

    int_missing <- apply(
      dtf_data[, chr_columns], 1, function(x) {
        sum( is.na(x) )
      }
    )

    # Update data frame
    if ( chr_new != '' ) {

      dtf_data[[ chr_new ]] <- int_missing

      return( dtf_data )

      # Close 'Update data frame'
    }

    return( int_missing )

    # Close 'Count of total missing'
  }

  # Proportion missing
  if ( chr_output %in% c( 'proportion', 'percent', '%' ) ) {

    int_missing <- apply(
      dtf_data[, chr_columns], 1, function(x) {
        sum( is.na(x) )
      }
    )

    # Update data frame
    if ( chr_new != '' ) {

      dtf_data[[ chr_new ]] <- int_missing / ncol(dtf_data)

      return( dtf_data )

      # Close 'Update data frame'
    }

    return( int_missing / ncol(dtf_data) )

    # Close 'Proportion missing'
  }

  # Pattern of missingness
  if ( chr_output %in% c( 'pattern' ) ) {

    chr_missing <- apply(
      dtf_data[, chr_columns], 1, function(x) {
        paste( as.numeric( is.na(x) ), collapse = '' )
      }
    )

    # Update data frame
    if ( chr_new != '' ) {

      dtf_data[[ chr_new ]] <- chr_missing

      return( dtf_data )

      # Close 'Update data frame'
    }

    return( chr_missing )

    # Close 'Pattern of missingness'
  }

  chr_error <- paste0(
    "Check argument 'chr_output' - should be either ",
    "'count', 'proportion', or 'pattern'"
  )

  stop( chr_error )
}

#### 3) swaap_data.files ####
#' Path to School-wide Assessment Data Files
#'
#' Given a standardized starting label, identifies
#' and returns the path to data files for the
#' school-wide assessment survey data. Files are
#' assumed to start with a leading tag, followed
#' by a label (typically the year and semester),
#' followed by a separator and additional information
#' (e.g., the date).
#'
#' @param chr_dir The path to the folder with the
#'   files to consider.
#' @param chr_tag A character string, the standardized
#'   tag used to indicate data files for the
#'   school-wide assessment.
#' @param chr_sep A character string, the separator
#'   between the label and subsequent file info.'
#' @param lgc_full A logical value; if \code{TRUE}
#'   returns the full conditional path of the files.
#'
#' @returns A labeled character vector of file paths.
#'
#' @export

swaap_data.files <- function(
    chr_dir,
    chr_tag = 'SWA-Surveys-',
    chr_sep = '-',
    lgc_full = TRUE ) {

  chr_files <- dir( path = chr_dir )

  chr_tags <- substr( chr_files, 1, nchar(chr_tag) )

  # If any file tags match
  if ( any( chr_tags %in% chr_tag) ) {

    int_index <- which(
      chr_tags %in% chr_tag
    )
    chr_labels <- sapply(
      seq_along(int_index), function(s) {
        chr_cur <- gsub( chr_tag, '', chr_files[s], fixed = TRUE )
        return( strsplit( chr_cur, split = chr_sep )[[1]][1] )
      }
    )

    chr_files <- dir( path = chr_dir, full.names = lgc_full )[
      int_index
    ]

    names(chr_files) <- chr_labels

    return(chr_files)

    # Close 'If any file tags match'
  }

  stop( 'No files with starting tag found' )
}

#### 4) swaap_data.attr ####
#' Extract Attributes Specific to swaap package
#'
#' Function to extract attribute of a column
#' created as part of a \code{swaap} function
#' call.
#'
#' @param vec_values A vector of values with
#'   attributes.
#'
#' @returns The \code{swaap}-created attribute.
#'
#' @export

swaap_data.attr <- function(
    vec_values ) {

  lst_attr <- attributes( vec_values )

  if ( is.null(lst_attr) )
    stop( 'Column has no attributes' )

  chr_elements <- substr( names(lst_attr), 1, 5 )

  # Attribute specific to swaap package
  if ( 'swaap' %in% chr_elements ) {

    return(
      lst_attr[[ which( chr_elements %in% 'swaap' ) ]]
    )

    # Close 'Attribute specific to swaap package'
  }

  stop( 'No attributes specific to swaap package' )

}

#### 5) swaap_data.subset ####
#' Subset Data Frame While Perserving Attributes
#'
#' Function to subset a data frame while preserving
#' any column attributes.
#'
#' @param dtf_data A data frame.
#' @param vec_subset A vector, either a logical vector
#'   or a vector of indices specifying a subset of
#'   \code{dtf_data} to take.
#'
#' @returns A data frame.
#'
#' @export

swaap_data.subset <- function(
    dtf_data,
    vec_subset ) {

  lst_attr_by_column <- dtf_data |>
    swaap:::swaap_data.internal.copy_attr()

  dtf_data <- dtf_data[vec_subset, ]

  dtf_data <- dtf_data |>
    swaap:::swaap_data.internal.copy_attr(
      lst_attr_by_column
    )

  return( dtf_data )
}

#### 5.1) swaap_data.internal.copy_attr ####
# Function to Copy Attributes in a Data Frame
#
# Function that copies column attributes for a
# data frame.
#
# @param 'dtf_data' A data frame.
# @param 'lst_attr_by_column' A named list of
#   lists, where names must match column names
#   in 'dtf_data' - if NULL, function will
#   instead create this list.
#
# @returns Either a list of copied attributes,
# or if 'lst_attr_by_column' is provided, an
# updated data frame with attributes copied
# over.

swaap_data.internal.copy_attr <- function(
    dtf_data,
    lst_attr_by_column = NULL ) {

  # Copy existing attributes
  if ( is.null( lst_attr_by_column ) ) {

    chr_columns <- colnames(dtf_data)

    lst_attr_by_column <- lapply(
      seq_along(chr_columns), function(j) {

        lst_attr <- attributes( dtf_data[[ chr_columns[j] ]] )

        return( lst_attr )
      }
    )
    names(lst_attr_by_column) <- chr_columns

    return( lst_attr_by_column )

    # Close 'Copy existing attributes'
  } else {

    # Loop over saved attributes
    for ( j in seq_along(lst_attr_by_column) ) {

      chr_column <- names( lst_attr_by_column )[j]

      attributes( dtf_data[[ chr_column ]] ) <-
        lst_attr_by_column[[j]]

      # Close 'Loop over saved attributes'
    }

    return( dtf_data )

    # Close else for 'Copy existing attributes'
  }

}
