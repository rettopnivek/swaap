# Misc. data processing functions
# Written by...
#   Kevin Potter
# Maintained by...
#   Kevin Potter
# Email:
#   kpotter5@mgh.harvard.edu
# Please email me directly if you
# have any questions or comments
# Last updated: 2026-04-27

# Table of contents
# 1) swaap_data.merge
# 2) swaap_data.missing
# 3) swaap_data.files
# 4) swaap_data.attr
# 5) swaap_data.subset
#   5.1) swaap_data.internal.copy_attr
# 6) swaap_data.download
# 7) swaap_data.replace
# 8) swaap_data.replace_defaults
# 9) swaap_data.static
# 10) swaap_data.at
# 11) swaap_data.deidentified
# 12) swaap_data.codebook_names
# 13) swaap_data.survey_summary

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
#' @param lgc_items A logical value; if \code{TRUE}
#'   includes individual items for inventory measures.
#' @param lgc_remove_flagged A logical value; if \code{TRUE}
#'   remove records that fail initial quality checks
#'   (see [swaap::swaap_add.quality_checks]).
#' @param int_grades An integer vector with values between
#'   6 and 12, the grade levels to include.
#' @param lgc_progress A logical value; if \code{TRUE}
#'   displays the function's progress.
#'
#' @author Kevin Potter
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
    lgc_items = TRUE,
    lgc_remove_flagged = TRUE,
    chr_source_files = '',
    int_grades = NULL,
    lgc_progress = TRUE ) {

  # List available functions
  if ( is.null(lst_data) ) {

    message("Templates for arguments:" )
    message("")

    # Add functions
    message( "chr_add = c(" )
    message( "  'source'," )
    message( "  'school_enrollment'," )
    message( "  'year_and_semester'," )
    message( "  'SBIRT'," )
    message( "  'subtances'," )
    message( "  'quality'" )
    message( ")" )
    message("")

    # Recode functions
    message( "chr_recode = c(" )
    message( "  'base'," )
    message( "  'contact'," )
    message( "  'demographics'," )
    message( "  'discrimination'," )
    message( "  'experience'," )
    message( "  'intermittent'," )
    message( "  'inventories'," )
    message( "  'linking'," )
    message( "  'misc'," )
    message( "  'quality'" )
    message( ")" )
    message("")

    # Select functions
    message( "chr_select = c(" )
    message( "  'base'," )
    message( "  'contact'," )
    message( "  'demographics'," )
    message( "  'discrimination'," )
    message( "  'experience'," )
    message( "  'intermittent'," )
    message( "  'inventories'" )
    message( "  'linking'," )
    message( "  'misc'," )
    message( "  'quality'," )
    message( "  'SBIRT'," )
    message( "  'substances'" )
    message( "  'suicidality'" )
    message( ")" )
    message("")


    return(NULL)

    # Close 'List available functions'
  }

  if ( lgc_progress ) message( 'Start: swaap_merge' )

  # Default add functions
  if ( all( chr_add == '' ) ) {

    chr_add <- c(
      'swaap_add.source',
      'swaap_add.school_enrollment',
      'swaap_add.year_and_semester',
      'swaap_add.SBIRT',
      'swaap_add.substances',
      'swaap_add.quality'
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
      'swaap_recode.discrimination',
      'swaap_recode.experience',
      'swaap_recode.intermittent',
      'swaap_recode.inventories',
      'swaap_recode.linking',
      'swaap_recode.misc',
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
      'swaap_select.contact',
      'swaap_select.demographics',
      'swaap_select.discrimination',
      'swaap_select.linking',
      'swaap_select.misc',
      'swaap_select.SBIRT',
      'swaap_select.substances',
      'swaap_select.suicidality',
      'swaap_select.experience',
      'swaap_select.intermittent',
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

  # Use all provided grades
  if ( is.null( int_grades ) ) {

    int_grades <- sapply(
      seq_along(lst_data), function(d) {
        unique( lst_data[[d]]$SSS.INT.Grade )
      }
    ) |> unlist() |> unique() |> sort()

    # Close 'Use all provided grades'
  }

  # Loop over data sets
  for ( d in seq_along( lst_data ) ) {

    if ( lgc_progress ) message( paste0( '  Data set ', d ) )

    dtf_current <- lst_data[[ d ]]

    # Add row for original data set
    dtf_current$IDN.INT.OriginalRow <- 1:nrow(dtf_current)
    # Add index for data set
    dtf_current$IDN.INT.DataSet <- d

    dtf_current <- dtf_current |>
      dplyr::filter(
        is.na( SSS.INT.Grade ) |
        SSS.INT.Grade %in% int_grades
      )

    if ( lgc_progress ) message( '    Add functions' )

    # Loop over add functions
    for ( j in seq_along( chr_add ) ) {

      if ( lgc_progress ) message( paste0( '      ', chr_add[j] ) )

      # Flag special cases
      chr_special <- ''
      if ( 'swaap_add.source' %in% chr_add[j] )
        chr_special <- 'source'
      if ( 'swaap_add.substances' %in% chr_add[j] )
        chr_special <- 'substances'

      # Adding source info
      if ( chr_special == 'source' ) {

        dtf_current <- do.call(
          chr_add[j],
          list( dtf_data = dtf_current,
                chr_source_files = chr_source_files )
        )

        # Close 'Adding source info'
      }

      # Adding substances
      if ( chr_special == 'substances' ) {

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

        dtf_current <- do.call(
          chr_add[j],
          list( dtf_data = dtf_current,
                chr_substance = 'Cigarettes' )
        )

        dtf_current <- do.call(
          chr_add[j],
          list( dtf_data = dtf_current,
                chr_substance = 'Cigars' )
        )

        dtf_current <- do.call(
          chr_add[j],
          list( dtf_data = dtf_current,
                chr_substance = 'Smokeless' )
        )

        dtf_current <- do.call(
          chr_add[j],
          list( dtf_data = dtf_current,
                chr_substance = 'Other' )
        )

        dtf_current <- do.call(
          chr_add[j],
          list( dtf_data = dtf_current,
                chr_substance = 'Quit' )
        )

        dtf_current <- do.call(
          chr_add[j],
          list( dtf_data = dtf_current,
                chr_substance = 'Crave' )
        )

        # Close 'Adding substances'
      }

      # Standard case
      if ( chr_special == '' ) {

        dtf_current <- do.call(
          chr_add[j],
          list( dtf_data = dtf_current )
        )

        # Close 'Standard case'
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

      lst_args <- list( chr_input = chr_columns )

      if ( 'swaap_select.substances' %in% chr_select[j] )
        lst_args$lgc_SBIRT <- lgc_SBIRT

      if ( 'swaap_select.inventories' %in% chr_select[j] )
        lst_args$lgc_items <- lgc_items

      # Special case for linking items
      if ( 'swaap_select.linking' %in% chr_select[j] )
        lst_args$lgc_all <- TRUE

      chr_columns <- do.call(
        chr_select[j],
        lst_args
      )

      # Close 'Loop over select functions'
    }

    dtf_current <- dtf_current |>
      swaap_select( chr_columns )

    # If not first data set
    if ( d > 1 ) {

      chr_long <- colnames(dtf_long)
      chr_current <- colnames(dtf_current)

      # Missing columns in current
      if ( any(!chr_long %in% chr_current) ) {

        chr_missing <- chr_long[
          !chr_long %in% chr_current
        ]

        # Loop over missing columns
        for ( k in seq_along(chr_missing) ) {

          dtf_current[[ chr_missing[k] ]] <- NA

          # Close 'Loop over missing columns'
        }


        # Close 'Missing columns in current'
      }

      # Missing columns in merged
      if ( any(!chr_current %in% chr_long) ) {

        chr_missing <- chr_current[
          !chr_current %in% chr_long
        ]

        # Loop over missing columns
        for ( k in seq_along(chr_missing) ) {

          dtf_long[[ chr_missing[k] ]] <- NA

          # Close 'Loop over missing columns'
        }

        # Close 'Missing columns in merged'
      }

      # Close 'If not first data set'
    }

    # Initialize data
    if ( d == 1 ) {

      # Merge details on rows to remove
      if ( 'QLT.LGC.Remove' %in% colnames(dtf_current) ) {

        # Add column with data set
        dtf_remove_current <- swaap::swaap_data.attr(
          dtf_current$QLT.LGC.Remove
        )
        dtf_remove_current$Data <- unique(
          dtf_current$SSS.CHR.DataSet
        )
        attributes( dtf_current$QLT.LGC.Remove ) <- list(
          swaap.summary_removed = dtf_remove_current
        )

        # Close 'Merge details on rows to remove'
      }

      dtf_long <- dtf_current

      # Close 'Initialize data'
    } else {

      # Merge details on rows to remove
      if ( 'QLT.LGC.Remove' %in% colnames(dtf_current) ) {

        # Merge relevant attributes
        dtf_remove_long <- swaap::swaap_data.attr(
          dtf_long$QLT.LGC.Remove
        )
        # Add column with data set
        dtf_remove_current <- swaap::swaap_data.attr(
          dtf_current$QLT.LGC.Remove
        )
        dtf_remove_current$Data <- unique(
          dtf_current$SSS.CHR.DataSet
        )
        # Update data frame
        dtf_remove_long <- rbind(
          dtf_remove_long,
          dtf_remove_current
        )

        # Close 'Merge details on rows to remove'
      }

      # Add current data
      dtf_long <- rbind(
        dtf_long,
        dtf_current
      )

      # Merge details on rows to remove
      if ( 'QLT.LGC.Remove' %in% colnames(dtf_current) ) {

        attributes( dtf_long$QLT.LGC.Remove ) <- list(
          swaap.summary_removed = dtf_remove_long
        )

        # Close 'Merge details on rows to remove'
      }

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
#' @author Kevin Potter
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
#' @author Kevin Potter
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
        chr_cur <-
          gsub( chr_tag, '', chr_files[int_index][s], fixed = TRUE )
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
#' @author Kevin Potter
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
#' Subset Data Frame While Preserving Attributes
#'
#' Function to subset a data frame while preserving
#' any column attributes.
#'
#' @param dtf_data A data frame.
#' @param vec_subset A vector, either a logical vector
#'   or a vector of indices specifying a subset of
#'   \code{dtf_data} to take.
#'
#' @author Kevin Potter
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
# @author Kevin Potter
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

#### 6) swaap_data.download ####
#' Download School-wide assessment Data
#'
#' Function to copy school-wide assessment
#' data from Dropbox to a local folder, or
#' at least provide path to specified data set.
#'
#' @param chr_dropbox A character string, the local
#'   path to the user's Dropbox folder.
#' @param chr_data A character vector, the data sets
#'   to copy, in the format \code{'<Year> <Semester>'}
#'   (e.g., \code{c( '2023 Fall', '2024 Fall' )}).
#'   Can also be either \code{'Merged Linked Data'} or
#'   \code{'CAM External Share'}, in which case
#'   pre-prepared merged and linked data will be downloaded
#'   instead.
#' @param lgc_complete A logical value; if \code{TRUE}
#'   downloads the full data set that includes
#'   confidential patient health information
#'   (only set to \code{TRUE} if you are positive you
#'   have full access to the data).
#' @param lgc_rename A logical value; if \code{TRUE}
#'   renames source files to the format:
#'   \code{'SWA-Surveys-<Year><Semester>-<Date>.csv'}
#' @param chr_copy_to A character string, the folder
#'   to which data should be copied. If \code{NULL}
#'   files will not be copied.
#' @param lgc_silent A logical value; if \code{TRUE}
#'   suppresses warning messages.
#'
#' @author Kevin Potter
#'
#' @returns A data frame with the file paths (both full
#' and truncated to within the CAM Data Dropbox folder)
#' for the requested data sets. If \code{chr_copy_to}
#' is not \code{NULL} as a side effect copies the files
#' from the Dropbox folder to the user-specified local
#' folder.
#'
#' @export

swaap_data.download <- function(
    chr_dropbox,
    chr_data = '2023 Fall',
    lgc_complete = FALSE,
    lgc_rename = TRUE,
    chr_copy_to = NULL,
    lgc_silent = FALSE ) {

  # chr_dropbox = "C:/Users/tempp/Partners HealthCare Dropbox/Kevin Potter"

  chr_CAM <- "CAM Data/SWA-2015/Output"

  # Path to Dropbox folder with SWA data
  chr_path_part_1 <- paste0(
    chr_dropbox, "/", chr_CAM
  )

  chr_special <- c(
    'Merged Linked Data',
    'CAM External Share'
  )

  # Special cases of pre-prepared data
  if ( all( chr_data %in% chr_special ) ) {

    # Merged linked data
    if ( chr_data %in% chr_special[1] ) {

      chr_folder <- paste0(
        chr_dropbox,
        "/",
        "CAM Data/SWA-2015/Output/Merged Linked Data/Output"
      )

      chr_files <- dir(
        chr_folder
      )

      if (!lgc_complete)
        stop( paste0( 'Cannot download data as it includes ',
                      'confidential patient health info' ) )

      if (!lgc_silent)
        warning( 'Complete data includes confidential patient health info' )

      chr_files <- chr_files[
        !chr_files %in% 'Archive'
      ]

      # Copy to new location
      if ( !is.null( chr_copy_to ) ) {

        # Sub-folder
        if ( chr_copy_to != '' ) {

          chr_path_new <- paste0(
            chr_copy_to, '/', chr_files
          )

          # Close 'Sub-folder'
        } else {

          chr_path_new <- chr_files

          # Close else for 'Sub-folder'
        }

        lgc_success <- file.copy(
          from = paste0( chr_folder, '/', chr_files ),
          to = chr_path_new
        )

        if (!lgc_success)
          stop( 'Failed to copy to new location' )

        if (!lgc_silent)
          message( 'File copied to specified location' )

        # Close 'Copy to new location'
      }

      dtf_output <- data.frame(
        Year = '2022 - 2024',
        Semester = 'Fall',
        Full = paste0( chr_folder, '/', chr_files ),
        Partial = chr_files,
        New = '',
        PHI = lgc_complete
      )

      if ( !is.null( chr_copy_to ) )
        dtf_output$New <- chr_path_new

      # Close 'Merged linked data'
    }

    # Data to share externally
    if ( chr_data %in% chr_special[2] ) {

      chr_folder <- paste0(
        chr_dropbox,
        "/",
        "CAM External Share/SWA-2015/Data to Share"
      )

      chr_files <- dir(
        chr_folder
      )

      chr_files <- chr_files[
        !chr_files %in% 'Archive'
      ]

      # Copy to new location
      if ( !is.null( chr_copy_to ) ) {

        # Sub-folder
        if ( chr_copy_to != '' ) {

          chr_path_new <- paste0(
            chr_copy_to, '/', chr_files
          )

          # Close 'Sub-folder'
        } else {

          chr_path_new <- chr_files

          # Close else for 'Sub-folder'
        }

        lgc_success <- file.copy(
          from = paste0( chr_folder, '/', chr_files ),
          to = chr_path_new
        )

        if (!lgc_success)
          stop( 'Failed to copy to new location' )

        if (!lgc_silent)
          message( 'File copied to specified location' )

        # Close 'Copy to new location'
      }

      dtf_output <- data.frame(
        Year = '2022 - 2024',
        Semester = 'Fall',
        Full = paste0( chr_folder, '/', chr_files ),
        Partial = chr_files,
        New = '',
        PHI = FALSE
      )

      if ( !is.null( chr_copy_to ) )
        dtf_output$New <- chr_path_new

      # Close 'Data to share externally'
    }

    return( dtf_output )

    # Close 'Special cases of pre-prepared data'
  }

  dtf_output <- data.frame(
    Year = rep( '', length(chr_data) ),
    Semester = '',
    Full = '',
    Partial = '',
    New = '',
    PHI = lgc_complete
  )

  # Loop over data sets
  for (d in seq_along(chr_data) ) {

    chr_parts <- strsplit(
      chr_data[d], split = ' ', fixed = TRUE
    )[[1]]
    chr_year <- chr_parts[1]
    chr_semester <- chr_parts[2]

    # Initialize full path to data
    chr_path_full <- chr_path_part_1
    # Initialize partial path to data
    chr_path_partial <- chr_CAM

    chr_contents <- dir( path = chr_path_part_1 )

    if ( length(chr_contents) == 0 )
      stop( "Dropbox folder with CAM Data not found" )

    # Folder is either (1) <year semester> or (2) <year>

    chr_year_semester <-
      paste0( chr_year, ' ', chr_semester )

    lgc_found <- FALSE

    # First try year and semester combo
    if ( !lgc_found & chr_year_semester %in% chr_contents ) {

      # Sub-folder with data
      chr_path_part_2 <- paste0( '/', chr_year_semester )
      chr_path_full <- paste0(
        chr_path_full, chr_path_part_2
      )
      chr_path_partial <- paste0(
        chr_path_partial, chr_path_part_2
      )

      lgc_found <- TRUE

      # Close 'First try year and semester combo'
    }

    # Next try only year
    if ( !lgc_found & chr_year %in% chr_contents ) {

      # Sub-folder with data
      chr_path_part_2 <- paste0( '/', chr_year )
      chr_path_full <- paste0(
        chr_path_full, chr_path_part_2
      )
      chr_path_partial <- paste0(
        chr_path_partial, chr_path_part_2
      )

      # If only year found, then semester must be Fall
      chr_semester <- 'Fall'

      lgc_found <- TRUE

      # Close 'Next try only year'
    }

    if (!lgc_found)
      stop( 'Folder for year and semester not found' )

    # Check if data in sub-folders
    chr_contents_sub <- dir( path = chr_path_full )

    lgc_csv <- grepl(
      '.csv', chr_contents_sub, fixed = TRUE
    )

    # Check sub-folders
    if ( !any( lgc_csv ) ) {

      # Download complete data
      if ( lgc_complete ) {

        lgc_subfolder <- grepl(
          'Complete', chr_contents_sub, fixed = TRUE
        )

        # Files in complete sub-folder
        chr_path_part_3 <- paste0( '/', chr_contents_sub[lgc_subfolder][1] )
        chr_path_full <- paste0(
          chr_path_full, chr_path_part_3
        )
        chr_path_partial <- paste0(
          chr_path_partial, chr_path_part_3
        )

        if (!lgc_silent)
          warning( 'Complete data includes confidential patient health info' )

        # Close 'Download complete data'
      } else {

        lgc_subfolder <- grepl(
          'De-Identified', chr_contents_sub, fixed = TRUE
        )

        # Files in de-identified sub-folder
        chr_path_part_3 <- paste0( '/', chr_contents_sub[lgc_subfolder][1] )
        chr_path_full <- paste0(
          chr_path_full, chr_path_part_3
        )
        chr_path_partial <- paste0(
          chr_path_partial, chr_path_part_3
        )

        # Close else for 'Download complete data'
      }

      # Close 'Check sub-folders'
    } else {

      if (!lgc_complete) stop( 'De-identified data not prepped yet' )

      if (!lgc_silent)
        warning( 'Complete data includes confidential patient health info' )

      # Close 'Check sub-folders'
    }

    chr_contents_csv <- dir(
      path = chr_path_full
    )

    lgc_csv <- grepl(
      '.csv', chr_contents_csv, fixed = TRUE
    )

    if ( !any( lgc_csv ) )
      stop( "No .csv file found" )

    # Isolate .csv files
    chr_contents_csv <- chr_contents_csv[lgc_csv]

    # Take most recent file
    lst_info <- lapply(
      chr_contents_csv, function(f) {

        obj_info <- file.info(
          paste0( chr_path_full, '/', f )
        )

      }
    )

    int_most_recent <- which.max(
      sapply( seq_along(lst_info), function(l) lst_info[[l]]$ctime )
    )

    chr_contents_csv <- chr_contents_csv[int_most_recent[1]]
    chr_path_full <- paste0(
      chr_path_full, '/', chr_contents_csv
    )
    chr_path_partial <- paste0(
      chr_path_partial, '/', chr_contents_csv
    )

    chr_new <- chr_contents_csv

    # Rename file
    if ( lgc_rename ) {

      chr_new <- paste0(
        'SWA-Surveys-',
        chr_year, chr_semester,
        '-',
        format( lst_info[[int_most_recent[1]]]$ctime, '%Y_%m_%d-%H_%M' ),
        '.csv'
      )

      # Close 'Rename file'
    }

    # Copy to new location
    if ( !is.null( chr_copy_to ) ) {

      # Sub-folder
      if ( chr_copy_to != '' ) {

        chr_path_new <- paste0(
          chr_copy_to, '/', chr_new
        )

        # Close 'Sub-folder'
      } else {

        chr_path_new <- chr_new

        # Close else for 'Sub-folder'
      }

      lgc_success <- file.copy(
        from = chr_path_full,
        to = chr_path_new
      )

      if (!lgc_success)
        stop( 'Failed to copy to new location' )

      if (!lgc_silent)
        message( 'File copied to specified location' )

      # Close 'Copy to new location'
    }

    chr_output <- c( chr_path_full, chr_path_partial )
    if ( !is.null( chr_copy_to ) )
      chr_output <- c( chr_output, chr_path_new )

    dtf_output$Year[d] <- chr_year
    dtf_output$Semester[d] <- chr_semester
    dtf_output$Full[d] = chr_output[1]
    dtf_output$Partial[d] = chr_output[2]
    dtf_output$New[d] = chr_output[3]

    # Close 'Loop over data sets'
  }

  # Include .csv with source info
  if ( !is.null( chr_copy_to ) ) {

    chr_source_info <- paste0(
      'SWA-Download-Source-',
      format( Sys.time(), '%Y_%m_%d-%H_%M' ),
      '.csv'
    )
    if ( chr_copy_to != '' )
      chr_source_info <-
        paste0( chr_copy_to, '/', chr_source_info )

    write.csv(
      dtf_output,
      file = chr_source_info,
      row.names = FALSE
    )

    # Close 'Include .csv with source info'
  }

  return( dtf_output )
}

#### 7) swaap_data.replace ####
#' Replace Values per User Specification
#'
#' Function to replace a vector of values with a
#' single value based on conditional logic.
#'
#' @param vec_values A vector of values.
#' @param lst_comparison A list of of vectors, different
#'   comparisons to test against \code{vec_values}.
#' @param chr_action A character vector, the type of
#'   action to conduct for each comparison given in
#'   \code{lst_comparison}.
#' @param vec_replacement A vector of values matching
#'   in length with \code{lst_comparison} and \code{chr_action},
#'   what to return when a given condition is met.
#' @param obj_default A value, the default output to return
#'   if no conditions are met.
#'
#' @author Kevin Potter
#'
#' @returns A single value as defined in \code{vec_replacement}.
#'
#' @examples
#' swaap_data.replace( c( 'A', NA ), list( 'A' ), 'all', 'A', NA )
#'
#' @export

swaap_data.replace <- function(
    vec_values,
    lst_comparison,
    chr_action,
    vec_replacement,
    obj_default ) {

  # Loop over options
  for (o in seq_along(lst_comparison) ) {

    # All non-missing match
    if ( chr_action[o] == 'all' ) {

      lgc_check <- all(
        vec_values[ !is.na(vec_values) ] %in% lst_comparison[[o]]
      )

      if ( lgc_check )
        return( vec_replacement[o] )

      # Close 'All non-missing match'
    }

    # Any non-missing match
    if ( chr_action[o] == 'any' ) {

      lgc_check <- any(
        vec_values[ !is.na(vec_values) ] %in% lst_comparison[[o]]
      )

      if ( lgc_check )
        return( vec_replacement[o] )

      # Close 'Any non-missing match'
    }

    # At least 2 different matches
    if ( chr_action[o] == 'combo' ) {

      lgc_check <- sum(
        lst_comparison[[o]] %in% vec_values[ !is.na(vec_values) ]
      ) > 1

      if ( lgc_check )
        return( vec_replacement[o] )

      # Close 'At least 2 different matches'
    }

    # Close 'Loop over options'
  }

  return( obj_default )
}

#### 8) swaap_data.replace_defaults ####
#' Default Options for swaap_data.replace
#'
#' Function that generates a list of inputs for
#' [swaap::swaap_data.replace] based on a given
#' column and additional options.
#'
#' @param chr_column A character string, the column
#'   name.
#' @param chr_option A character string, additional
#'   options to consider when generating inputs.
#' @param chr_coding A character string. Use the
#'   options \code{'dummy'} or \code{'effect'} to
#'   convert values to a desired numeric coding scheme.
#'
#' @author Kevin Potter
#'
#' @returns A list of inputs for [swaap::swaap_data.replace].
#'
#' @export

swaap_data.replace_defaults <- function(
    chr_column,
    chr_option = '',
    chr_coding = '' ) {

  chr_columns <- c(
    Sex = 'SBJ.CHR.Sex',
    Race = 'SBJ.CHR.Race',
    Ethnicity = 'SBJ.CHR.Ethnicity'
  )

  lst_coding <- list(
    dummy = c(
      "dummy",
      "Dummy",
      "dummy coding",
      "Dummy coding",
      "dummy coded",
      "Dummy coded",
      "indicator",
      "Indicator",
      "indicator coding",
      "Indicator coding"
    ),
    effect = c(
      "effect",
      "Effect",
      "effect coding",
      "Effect coding",
      "deviation",
      "Deviation",
      "deviation coding",
      "Deviation coding"
    )
  )

  # Biological sex
  if ( chr_column == chr_columns['Sex'] ) {

    lst_replace <- list(
      # Comparison
      list(
        "Male",
        "Female",
        c(
          "Male",
          "Female"
        )
      ),
      # Action
      c( "all", "all", "combo" ),
      # Replacement
      c( "Male",
         "Female",
         NA ),
      # Default
      NA,
      # Static
      c( TRUE, TRUE, FALSE )
    )

    # Use dummy coding
    if ( chr_coding %in% lst_coding$dummy ) {

      lst_replace[[3]] <- c(
        0, # Referent: Male
        1,
        1
      )

      # Close 'Use dummy coding'
    }

    # Use effect coding
    if ( chr_coding %in% lst_coding$effect ) {

      lst_replace[[3]] <- c(
        -1, # Referent: Male
         1,
         1
      )

      # Close 'Use effect coding'
    }

    return( lst_replace )

    # Close 'Ethnicity'
  }

  # Race
  if ( chr_column == chr_columns['Race'] ) {

    lst_replace <- list(
      # Comparison
      list(
        # All x 8
        "American Indian/Alaska Native",
        "Asian",
        "Haitian, Black or African American",
        "Hawaiian or Other Pacific Islander",
        "Middle Eastern/North African",
        "Multiracial",
        "Other",
        "White",
        c(
          "American Indian/Alaska Native",
          "Asian",
          "Haitian, Black or African American",
          "Hawaiian or Other Pacific Islander",
          "Middle Eastern/North African",
          "Multiracial",
          "Other",
          "White"
        )
      ),
      # Action
      c(
        # All x 8
        rep( "all", 8 ),
        # Convert to multiracial
        "combo"
      ),
      # Replacement
      c(
        # All x 8
        "American Indian/Alaska Native",
        "Asian",
        "Haitian, Black or African American",
        "Hawaiian or Other Pacific Islander",
        "Middle Eastern/North African",
        "Multiracial",
        "Other",
        "White",
        # Convert to multiracial
        "Multiracial"
      ),
      # Default
      NA,
      # Static
      c(
        # All x 8
        rep( TRUE, 8 ),
        # Convert to multiracial
        FALSE
      )
    )

    return( lst_replace )

    # Close 'Race'
  }

  # Ethnicity
  if ( chr_column == chr_columns['Ethnicity'] ) {

    lst_replace <- list(
      # Comparison
      list(
        "Hispanic/ Latino(a)",
        "Not Hispanic/ Latino(a)",
        c(
          "Hispanic/ Latino(a)",
          "Not Hispanic/ Latino(a)"
        )
      ),
      # Action
      c( "all", "all", "combo" ),
      # Replacement
      c( "Hispanic/Latino(a)",
         "Not Hispanic/Latino(a)",
         "Hispanic/Latino(a)" ),
      # Default
      NA,
      # Static
      c( TRUE, TRUE, FALSE )
    )

    # Use dummy coding
    if ( chr_coding %in% lst_coding$dummy ) {

      lst_replace[[3]] <- c(
        1,
        0, # Referent: Not Hispanic/Latino(a)
        1
      )

      # Close 'Use dummy coding'
    }

    # Use effect coding
    if ( chr_coding %in% lst_coding$effect ) {

      lst_replace[[3]] <- c(
         1,
        -1, # Referent: Not Hispanic/Latino(a)
         1
      )

      # Close 'Use effect coding'
    }

    return( lst_replace )

    # Close 'Ethnicity'
  }

  stop(
    "Check argument 'chr_column'"
  )
}

#### 9) swaap_data.static ####
#' Create Static Variant of a Variable
#'
#' Function that will create a static variable
#' in which values are consistent across multiple
#' time points.
#'
#' @param dtf_data A data frame, assumed to
#'   follow the standardized format for the
#'   school-wide assessment data.
#' @param chr_variable A character string, the
#'   column to update to ensure static values
#'   across time points.
#' @param chr_new A character vector of up to
#'   two values, the new column names for
#'   (1) the revised static variable, and
#'   (2) a quality check variable indicating
#'   cases that had to be recoded to be static.
#' @param chr_ID A character string, the column
#'   name for the identifier.
#' @param lst_replace An optional list of
#'   inputs for [swaap::swaap_data.replace].
#'
#' @returns A data frame with up to two new columns.
#'
#' @export

swaap_data.static <- function(
    dtf_data,
    chr_variable,
    chr_new = '',
    chr_ID = 'IDN.CHR.Linked.ID',
    lst_replace = NULL ) {

  # Debugging
  if ( FALSE ) {

    dtf_data <- data.frame(
      IDN.CHR.Linked.ID = c(
        1, 1, 1, 2, 2, 2, 3, 3, 3
      ),
      SBJ.CHR.Ethnicity = c(
        "Hispanic/ Latino(a)",
        "Hispanic/ Latino(a)",
        NA,
        NA,
        "Not Hispanic/ Latino(a)",
        "Not Hispanic/ Latino(a)",
        "Not Hispanic/ Latino(a)",
        NA,
        "Hispanic/ Latino(a)"
      )
    )

    # Close 'Debugging'
  }

  # Default option for replacement parameters
  if ( is.null(lst_replace) ) {

    lst_replace <- swaap_data.replace_defaults(
      chr_variable
    )

    # Close 'Default option for replacement parameters'
  }

  # If indicated track if variable is static
  if ( !is.null(lst_replace[[5]] ) ) {

    lst_replace[[3]] <- paste0(
      lst_replace[[3]], 'STATIC=', lst_replace[[5]]
    )

    # Close 'If indicated track if variable is static'
  }

  if ( !chr_ID %in% colnames(dtf_data) )
    stop( 'ID column not found' )

  chr_IDs <- unique( dtf_data[[ chr_ID ]] )

  # Ensure two column names
  if ( length(chr_new) == 1 ) {

    chr_new <- c( chr_new, '' )

    # Close 'Ensure two column names'
  }

  # Default name for new column of values
  if ( chr_new[1] == '' ) {

    chr_new[1] <- paste0( chr_variable, '.Static' )

    # Close 'Default name for new column of values'
  }

  # Default name for quality control variable
  if ( chr_new[2] == '' ) {

    chr_new[2] <- gsub(
      substr( chr_new[1], 1, 3 ),
      'QLT',
      chr_new[1],
      fixed = TRUE
    )
    chr_new[2] <- gsub(
      substr( chr_new[1], 5, 7 ),
      'LGC',
      chr_new[2],
      fixed = TRUE
    )

    # Close 'Default name for quality control variable'
  }

  dtf_collapsed <- dtf_data |>
    dplyr::group_by_at( chr_ID ) |>
    dplyr::summarise_at(
      chr_variable,
      swaap_data.replace,
      lst_comparison = lst_replace[[1]],
      chr_action = lst_replace[[2]],
      vec_replacement = lst_replace[[3]],
      obj_default = lst_replace[[4]]
    ) |>
    data.frame()

  colnames(dtf_collapsed)[2] <- chr_new[1]
  dtf_collapsed[[ chr_new[2] ]] <- grepl(
    'STATIC=TRUE',
    dtf_collapsed[[ chr_new[1] ]],
    fixed = TRUE
  )
  dtf_collapsed[[ chr_new[1] ]] <- gsub(
    'STATIC=TRUE', '',
    dtf_collapsed[[ chr_new[1] ]],
    fixed = TRUE
  )
  dtf_collapsed[[ chr_new[1] ]] <- gsub(
    'STATIC=FALSE', '',
    dtf_collapsed[[ chr_new[1] ]],
    fixed = TRUE
  )

  if ( all( !dtf_collapsed[[ chr_new[2] ]] ) )
    dtf_collapsed[[ chr_new[2] ]] <- NULL

  dtf_data <- dtf_data |>
    dplyr::left_join(
      dtf_collapsed,
      by = chr_ID
    )

  return( dtf_data )
}

#### 10) swaap_data.at ####
#' Take Slice of Values at Specified Time
#'
#' Function that will extract values for a
#' variable at a specified time point (and
#' if a new variable is given, update the
#' data frame with those values propagated
#' by ID).
#'
#' @param dtf_data A data frame, assumed to
#'   have linked records across time points
#'   and a column \code{'SSS.INT.TimePoint'}.
#' @param int_time An integer value, the time
#'   point of interest (must be a value in
#'   \code{'SSS.INT.TimePoint'}),
#' @param chr_column A character string, the
#'   column name with the values to subset.
#' @param chr_ID A character string, the column
#'   name for the linked record identifier.
#' @param chr_new A character string, the new
#'   column name to add to \code{dtf_data}
#'   with the propagated values. If blank
#'   instead returns a new data frame.
#' @param obj_default A value, the default to
#'   return when the time point is missing.
#'
#' @returns A data frame, either the extracted
#'   values at the specified time for each
#'   ID or the updated original data frame.
#'
#' @export

swaap_data.at <- function(
    dtf_data,
    int_time,
    chr_column,
    chr_ID = 'IDN.CHR.Linked.ID',
    chr_new = '',
    obj_default = NA ) {

  dtf_data$Intermediate <- dtf_data[[ chr_column ]]

  fun_slice <- function(
    vec_values,
    obj_default ) {

    # No values
    if ( length(vec_values) == 0 ) {

      return( obj_default )

      # Close 'No values'
    } else {

      return( vec_values[1] )

      # Close else for 'No values'
    }

  }

  fun_time <- function(
    vec_times,
    int_time ) {

    if ( int_time %in% vec_times )
      return( int_time )

    return( sort( unique(vec_times) )[1] )

  }

  dtf_slice <- dtf_data |>
    dplyr::group_by_at(
      chr_ID
    ) |>
    dplyr::summarize(
      Time = fun_time( SSS.INT.TimePoint, int_time ),
      Value = fun_slice(
        Intermediate[SSS.INT.TimePoint %in% int_time],
        obj_default
      ),
      .groups = 'drop'
    ) |>
    data.frame()

  # Add to existing data
  if ( chr_new != '' ) {

    colnames( dtf_slice ) <- c(
      chr_ID,
      'Time',
      chr_new
    )
    dtf_slice <- dtf_slice |>
      dplyr::filter(
        Time %in% int_time
      )
    dtf_slice$Time <- NULL

    dtf_data <- dtf_data |>
      dplyr::left_join(
        dtf_slice
      )

    return( dtf_data )

    # Close 'Add to existing data'
  }

  return( dtf_slice )
}

#### 11) swaap_data.deidentified ####
#' Deidentify a Standardized Data Set
#'
#' Function to deidentify data following
#' the standardized format for school-wide
#' assessment data.
#'
#' @param dtf_data A data frame, assumed to
#'   follow the standardized format for the
#'   school-wide assessment data.
#'
#' @returns A data frame with columns containing
#' potential identifying data removed.
#'
#' @export

swaap_data.deidentified <- function(
    dtf_data,
    lgc_codebook_names = TRUE ) {

  chr_columns <- colnames(dtf_data)

  chr_remove <- c(
    'IDN.CHR.LocallyAssignedSchool.ID',
    'IDN.CHR.LAS.ID',
    swaap::swaap_select.contact(),
    swaap::swaap_select.linking(lgc_all = TRUE),
    chr_columns[
      grepl( '.Link.', chr_columns, fixed = TRUE )
    ],
    chr_columns[
      grepl( '.DTT.', chr_columns, fixed = TRUE )
    ],
    'QLT.CHR.Remove',
    'SSS.INT.RecruitmentWave',
    'SBJ.INT.Experience.GradesInSchool'
  )

  chr_columns <- chr_columns[
    !chr_columns %in% chr_remove
  ]

  dtf_data <- dtf_data[, chr_columns]

  if (lgc_codebook_names)
    dtf_data <- swaap::swaap_data.codebook_names(
      dtf_data = dtf_data
    )

  return( dtf_data )
}

#### 12) swaap_data.codebook_names #####
#' Match Variables Names to Codebook
#'
#' Convenience function that will convert
#' variable names to be consistent with
#' the codebook for shared linked SURF data.
#'
#' @param dtf_data A data frame.
#' @param chr_columns A character vector.
#' @param lgc_reverse A logical value; if \code{TRUE}
#'   converts back to original variable names instead.
#'
#' @returns Either a data frame or character vector,
#' depending if \code{dtf_data} or \code{chr_columns}
#' is provided, respectively. If no input is given,
#' returns a data frame with the pairs of
#' original and codebook variable names.
#'
#' @export

swaap_data.codebook_names <- function(
    dtf_data = NULL,
    chr_columns = NULL,
    lgc_reverse = FALSE ) {

  dtf_names <- rbind(

    # SRV: Survey Metadata
    c( 'SSS.CHR.DataSet', 'SRV.CHR.DataSet' ),
    c( 'SSS.INT.SurveyYear', 'SRV.INT.SurveyYear' ),
    c( 'SSS.CHR.Semester', 'SRV.CHR.Semester' ),
    c( 'SSS.CHR.SurveyYearSemester', 'SRV.CHR.SurveyYearSemester' ),
    c( 'SSS.INT.DistrictCode', 'SRV.INT.DistrictCode' ),
    c( 'SSS.INT.SchoolCode', 'SRV.INT.SchoolCode' ),
    c( 'SSS.INT.SchoolEnrollment', 'SRV.INT.SchoolEnrollment' ),
    c( 'SSS.INT.SchoolEnrollmentTotal', 'SRV.INT.SchoolEnrollmentTotal' ),
    c( 'SSS.INT.TimePoint', 'SRV.INT.TimePoint' ),
    c( 'SSS.INT.LongitudinalWave', 'SRV.INT.LongitudinalWave' ),
    c( 'SSS.LGC.SBIRT', 'SRV.LGC.SBIRT' ),
    c( 'SSS.CHR.SurveyLanguage', 'SRV.CHR.SurveyLanguage' ),

    # IDN; Identifiers
    c( 'IDN.CHR.Record.ID', 'IDN.CHR.Record.ID' ),
    c( 'IDN.CHR.Linked.ID', 'IDN.CHR.Linked.ID' ),

    # LNK: Linkage of Records
    rep( 'LNK.LGC.Attempted', 2 ),
    rep( 'LNK.LGC.NoIssues', 2 ),
    rep( 'LNK.CHR.TimePoints', 2 ),
    rep( 'LNK.CHR.SchoolYears', 2 ),
    rep( 'LNK.CHR.Grades', 2 ),

    # DMG: Demographics
    c( 'SSS.INT.Grade', 'DMG.INT.Grade' ),
    c( 'SBJ.CHR.Sex', 'DMG.CHR.Sex' ),
    c( 'SBJ.INT.AgeInYears', 'DMG.INT.AgeInYears' ),
    c( 'SBJ.CHR.GenderIdentity', 'DMG.CHR.GenderIdentity' ),
    c( 'SBJ.CHR.Sexuality', 'DMG.CHR.Sexuality' ),
    c( 'SBJ.CHR.Race', 'DMG.CHR.Race' ),
    c( 'SBJ.CHR.Ethnicity', 'DMG.CHR.Ethnicity' ),
    c( 'SBJ.LGC.Language.EnglishWasFirst',
       'DMG.LGC.Language.EnglishWasFirst' ),
    c( 'SBJ.LGC.Language.EnglishAtHome',
       'DMG.LGC.Language.EnglishAtHome' ),

    # MNT: Mental Health
    c( 'SBJ.CHR.PrescribedMedicationHealth',
       'MNT.CHR.PrescribedMedicationHealth' ),
    c( 'SBJ.LGC.SoughtHelp.ParentCaregiver',
       'MNT.LGC.SoughtHelp.ParentCaregiver' ),
    c( 'SBJ.LGC.SoughtHelp.OtherFamily',
       'MNT.LGC.SoughtHelp.OtherFamily' ),
    c( 'SBJ.LGC.SoughtHelp.FriendPartner',
       'MNT.LGC.SoughtHelp.FriendPartner' ),
    c( 'SBJ.LGC.SoughtHelp.TeacherCoachAdmin',
       'MNT.LGC.SoughtHelp.TeacherCoachAdmin' ),
    c( 'SBJ.LGC.SoughtHelp.SchoolCounselor',
       'MNT.LGC.SoughtHelp.SchoolCounselor' ),
    c( 'SBJ.LGC.SoughtHelp.OutsideCounselor',
       'MNT.LGC.SoughtHelp.OutsideCounselor' ),
    c( 'SBJ.LGC.SoughtHelp.Pediatrician',
       'MNT.LGC.SoughtHelp.Pediatrician' ),
    c( 'SBJ.LGC.SoughtHelp.ReligiousLeader',
       'MNT.LGC.SoughtHelp.ReligiousLeader' ),
    c( 'SBJ.LGC.SoughtHelp.Helpline',
       'MNT.LGC.SoughtHelp.Helpline' ),
    c( 'SBJ.LGC.SoughtHelp.SocialMediaSupport',
       'MNT.LGC.SoughtHelp.SocialMediaSupport' ),
    c( 'SBJ.LGC.SoughtHelp.EmergencyServices',
       'MNT.LGC.SoughtHelp.EmergencyServices' ),
    c( 'SBJ.LGC.SoughtHelp.RehabCenter',
       'MNT.LGC.SoughtHelp.RehabCenter' ),
    c( 'SBJ.LGC.SoughtHelp.NotListed',
       'MNT.LGC.SoughtHelp.NotListed' ),
    c( 'SBJ.LGC.SoughtHelp.Nurse',
       'MNT.LGC.SoughtHelp.Nurse' ),
    c( 'SBJ.LGC.SoughtHelp.YouthWellnessCoach',
       'MNT.LGC.SoughtHelp.YouthWellnessCoach' ),
    c( 'SBJ.LGC.SoughtHelp.None',
       'MNT.LGC.SoughtHelp.None' ),
    c( 'SBJ.CHR.SoughtHelp.Other',
       'MNT.CHR.SoughtHelp.Other' ),
    c( 'SBJ.CHR.SoughtHelp.FrequencyPast6Months',
       'MNT.CHR.SoughtHelp.FrequencyPast6Months' ),
    c( 'SBJ.CHR.SoughtHelp.HelpfulnessPast6Months',
       'MNT.CHR.SoughtHelp.HelpfulnessPast6Months' ),

    # SBS: Substance Use
    rep( 'SBS.LGC.ALC.Lifetime.Any', 2 ),
    rep( 'SBS.INT.ALC.Past31.UseRating', 2 ),
    rep( 'SBS.CHR.ALC.Past31.UseRating', 2 ),
    rep( 'SBS.LGC.CNN.Lifetime.Any', 2 ),
    rep( 'SBS.INT.CNN.Past31.UseRating', 2 ),
    rep( 'SBS.CHR.CNN.Past31.UseRating', 2 ),
    rep( 'SBS.LGC.VPS.Lifetime.Any', 2 ),
    rep( 'SBS.INT.VPS.Past31.UseRating', 2 ),
    rep( 'SBS.CHR.VPS.Past31.UseRating', 2 ),
    rep( 'SBS.LGC.CIG.Lifetime.Any', 2 ),
    rep( 'SBS.INT.CIG.Past31.UseRating', 2 ),
    rep( 'SBS.CHR.CIG.Past31.UseRating', 2 ),
    rep( 'SBS.LGC.CGR.Lifetime.Any', 2 ),
    rep( 'SBS.INT.CGR.Past31.UseRating', 2 ),
    rep( 'SBS.CHR.CGR.Past31.UseRating', 2 ),
    rep( 'SBS.LGC.SMK.Lifetime.Any', 2 ),
    rep( 'SBS.INT.SMK.Past31.UseRating', 2 ),
    rep( 'SBS.CHR.SMK.Past31.UseRating', 2 ),
    rep( 'SBS.LGC.OTH.Lifetime.Any', 2 ),
    rep( 'SBS.CHR.OTH.Lifetime.Any', 2 ),
    rep( 'SBS.CHR.ALC.ConsiderQuitting', 2 ),
    rep( 'SBS.CHR.CNN.ConsiderQuitting', 2 ),
    rep( 'SBS.CHR.VPS.ConsiderQuitting', 2 ),
    rep( 'SBS.CHR.CNN.CravingOnWakingUp', 2 ),
    rep( 'SBS.CHR.NCT.CravingOnWakingUp', 2 ),
    rep( 'SBS.INT.ALC.Past31.BingeRating', 2 ),
    rep( 'SBS.CHR.ALC.Past31.BingeRating', 2 ),

    # INV: Inventories
    rep( 'INV.INT.DISC.Q1.Gender', 2 ),
    rep( 'INV.INT.DISC.Q2.Sexuality', 2 ),
    rep( 'INV.INT.DISC.Q3.Religion', 2 ),
    rep( 'INV.INT.DISC.Q4.Disability', 2 ),
    rep( 'INV.INT.DISC.Q5.Money', 2 ),
    rep( 'INV.INT.DISC.Q6.Other', 2 ),
    rep( 'INV.CHR.DISC.Other', 2 ),
    rep( 'INV.INT.SI.Thoughts', 2 ),
    rep( 'INV.INT.SI.How', 2 ),
    rep( 'INV.INT.SI.Attempt', 2 ),
    rep( 'INV.INT.SI.Selfharm', 2 ),
    rep( 'INV.INT.SI.Total', 2 ),
    rep( 'INV.INT.PHQ4.Q1.Anxious', 2 ),
    rep( 'INV.INT.PHQ4.Q2.Worried', 2 ),
    rep( 'INV.INT.PHQ4.Q3.Depressed', 2 ),
    rep( 'INV.INT.PHQ4.Q4.Anhedonia', 2 ),
    rep( 'INV.INT.ERS.Q1.P.UpsetLongTime', 2 ),
    rep( 'INV.INT.ERS.Q2.S.HurtEasily', 2 ),
    rep( 'INV.INT.ERS.Q3.I.FeelIntensely', 2 ),
    rep( 'INV.INT.ERS.Q4.I.PhysicallyUpset', 2 ),
    rep( 'INV.INT.ERS.Q5.S.EmotionalEasily', 2 ),
    rep( 'INV.INT.ERS.Q6.I.EmotionsStrongly', 2 ),
    rep( 'INV.INT.ERS.Q7.S.OftenAnxious', 2 ),
    rep( 'INV.INT.ERS.Q8.P.FeelOther', 2 ),
    rep( 'INV.INT.ERS.Q9.S.LittlestThings', 2 ),
    rep( 'INV.INT.ERS.Q10.P.DisagreementLong', 2 ),
    rep( 'INV.INT.ERS.Q11.P.LongerToCalmDown', 2 ),
    rep( 'INV.INT.ERS.Q12.S.AngryEasily', 2 ),
    rep( 'INV.INT.ERS.Q13.S.Bothered', 2 ),
    rep( 'INV.INT.ERS.Q14.S.EasilyAgitated', 2 ),
    rep( 'INV.INT.ERS.Q15.S.EmotionsInstant', 2 ),
    rep( 'INV.INT.ERS.Q16.S.ShortFuse', 2 ),
    rep( 'INV.INT.ERS.Q17.I.EmotionsTooIntense', 2 ),
    rep( 'INV.INT.ERS.Q18.S.SensitivePerson', 2 ),
    rep( 'INV.INT.ERS.Q19.I.MoodsPowerful', 2 ),
    rep( 'INV.INT.ERS.Q20.I.HardToThink', 2 ),
    rep( 'INV.INT.ERS.Q21.I.Overreacting', 2 ),
    rep( 'INV.INT.ADDI.Q1.D.Class', 2 ),
    rep( 'INV.INT.ADDI.Q2.D.Disciplined', 2 ),
    rep( 'INV.INT.ADDI.Q3.D.Grade', 2 ),
    rep( 'INV.INT.ADDI.Q4.D.Club', 2 ),
    rep( 'INV.INT.ADDI.Q5.D.Activities', 2 ),
    rep( 'INV.INT.ADDI.Q6.D.More', 2 ),
    rep( 'INV.INT.ADDI.Q7.D.Less', 2 ),
    rep( 'INV.INT.ADDI.Q8.D.English', 2 ),
    rep( 'INV.INT.ADDI.Q9.D.Police', 2 ),
    rep( 'INV.INT.ADDI.Q10.D.Store', 2 ),
    rep( 'INV.INT.ADDI.Q11.D.Insulted', 2 ),
    rep( 'INV.INT.ADDI.Q12.D.Service', 2 ),
    rep( 'INV.INT.ADDI.Q13.D.Smart', 2 ),
    rep( 'INV.INT.ADDI.Q14.D.Afraid', 2 ),
    rep( 'INV.INT.ADDI.Q15.D.Threatened', 2 ),
    rep( 'INV.INT.ADDI.Q1.U.Class', 2 ),
    rep( 'INV.INT.ADDI.Q2.U.Disciplined', 2 ),
    rep( 'INV.INT.ADDI.Q3.U.Grade', 2 ),
    rep( 'INV.INT.ADDI.Q4.U.Club', 2 ),
    rep( 'INV.INT.ADDI.Q5.U.Activities', 2 ),
    rep( 'INV.INT.ADDI.Q6.U.More', 2 ),
    rep( 'INV.INT.ADDI.Q7.U.Less', 2 ),
    rep( 'INV.INT.ADDI.Q8.U.English', 2 ),
    rep( 'INV.INT.ADDI.Q9.U.Police', 2 ),
    rep( 'INV.INT.ADDI.Q10.U.Store', 2 ),
    rep( 'INV.INT.ADDI.Q11.U.Insulted', 2 ),
    rep( 'INV.INT.ADDI.Q12.U.Service', 2 ),
    rep( 'INV.INT.ADDI.Q13.U.Smart', 2 ),
    rep( 'INV.INT.ADDI.Q14.U.Afraid', 2 ),
    rep( 'INV.INT.ADDI.Q15.U.Threatened', 2 ),
    rep( 'INV.DBL.APSS.Q1.MindReading', 2 ),
    rep( 'INV.DBL.APSS.Q2.TVRadio', 2 ),
    rep( 'INV.DBL.APSS.Q3.Spying', 2 ),
    rep( 'INV.DBL.APSS.Q4.Auditory', 2 ),
    rep( 'INV.DBL.APSS.Q5.Controlled', 2 ),
    rep( 'INV.DBL.APSS.Q6.Visual', 2 ),
    rep( 'INV.DBL.APSS.Q7.Grandiosity', 2 ),
    rep( 'INV.INT.PHQ4.Anxiety', 2 ),
    rep( 'INV.INT.PHQ4.Depression', 2 ),
    rep( 'INV.INT.PHQ4.Total', 2 ),
    rep( 'INV.CHR.PHQ4.CutOffs', 2 ),
    rep( 'INV.LGC.PHQ4.Distress', 2 ),
    rep( 'INV.LGC.PHQ4.Anxiety', 2 ),
    rep( 'INV.LGC.PHQ4.Depression', 2 ),
    rep( 'INV.INT.ERS.Total', 2 ),
    rep( 'INV.INT.ERS.Sensitivity', 2 ),
    rep( 'INV.INT.ERS.Persistence', 2 ),
    rep( 'INV.INT.ERS.Intensity', 2 ),
    rep( 'INV.CHR.AUDIT.CutOffs', 2 ),
    rep( 'INV.INT.ADDI.D.Total', 2 ),
    rep( 'INV.INT.ADDI.U.Total', 2 ),
    rep( 'INV.DBL.APSS.Total', 2 ),
    rep( 'INV.CHR.APSS.CutOffs', 2 ),
    rep( 'INV.LGC.APSS.AtRisk', 2 ),
    rep( 'INV.INT.AUDIT.Q1.Frequency', 2 ),
    rep( 'INV.INT.AUDIT.Q2.Drinks', 2 ),
    rep( 'INV.INT.AUDIT.Q3.Binge', 2 ),
    rep( 'INV.INT.AUDIT.Q4.Stopping', 2 ),
    rep( 'INV.INT.AUDIT.Q5.Failure', 2 ),
    rep( 'INV.INT.AUDIT.Q6.Morning', 2 ),
    rep( 'INV.INT.AUDIT.Q7.Guilt', 2 ),
    rep( 'INV.INT.AUDIT.Q8.Memory', 2 ),
    rep( 'INV.INT.AUDIT.Q9.Injured', 2 ),
    rep( 'INV.INT.AUDIT.Q10.Concern', 2 ),
    rep( 'INV.INT.AUDIT.Total', 2 ),

    # SCH: School Experiences
    c('SBJ.LGC.Experience.PlaySports',
      'SCH.LGC.PlaySports' ),
    c('SBJ.CHR.Experience.GradesInSchool',
      'SCH.CHR.GradesInSchool' ),
    c('SBJ.INT.Experience.GradesInSchool',
      'SCH.INT.GradesInSchool' ),
    c('SBJ.LGC.Experience.SuspensionsAny',
      'SCH.LGC.Suspensions.Any' ),
    c('SBJ.LGC.Experience.SuspensionsDrug',
      'SCH.LGC.Suspensions.Drug' ),
    c('SBJ.LGC.Experience.UsedDrugsAtSchool',
      'SCH.LGC.Suspensions.UsedDrugsAtSchool' ),
    c('SBJ.CHR.Experience.IEP',
      'SCH.CHR.IEP' ),

    c( 'SBJ.LGC.CloseConnection.Friend',
       'SCH.LGC.CloseConnection.Friend' ),
    c( 'SBJ.LGC.CloseConnection.Parent',
       'SCH.LGC.CloseConnection.Parent' ),
    c( 'SBJ.LGC.CloseConnection.Teacher',
       'SCH.LGC.CloseConnection.Teacher' ),
    c( 'SBJ.INT.CloseConnection.Happiness',
       'SCH.INT.CloseConnection.Happiness' ),
    c( 'SBJ.CHR.CloseConnection.Happiness',
       'SCH.CHR.CloseConnection.Happiness' ),
    c( 'SBJ.CHR.Sleep.TimeWakeUp',
       'SCH.CHR.Sleep.TimeWakeUp' ),
    c( 'SBJ.CHR.Sleep.TimeGoToBed',
       'SCH.CHR.Sleep.TimeGoToBed' ),
    c( 'SBJ.INT.Sleep.TirednessDuringDay',
       'SCH.INT.Sleep.TirednessDuringDay' ),

    c( 'SBJ.LGC.ClimateChange.Worried',
       'SCH.LGC.ClimateChange.Worried' ),
    c( 'SBJ.CHR.ClimateChange.ImpactOnDailyLife',
       'SCH.CHR.ClimateChange.ImpactOnDailyLife' ),
    c( 'SBJ.CHR.ClimateChange.CopingStrategies',
       'SCH.CHR.ClimateChange.CopingStrategies' ),
    c( 'SBJ.CHR.SocialMediaUseFrequency',
       'SCH.CHR.SocialMediaUseFrequency' ),
    c( 'SBJ.LGC.ConnectWithSchoolServices',
       'SCH.LGC.ConnectWithSchoolServices' ),

    # QLT: Quality checks
    rep( 'QLT.DBL.ProportionCompleted.Total', 2 ),
    rep( 'QLT.LGC.AttentionChecks.MetAll', 2 ),
    rep( 'QLT.LGC.AttentionChecks.MetAtLeastOne', 2 ),

    # WGH: Weights
    c( 'SBJ.DBL.Weights.State', 'WGH.DBL.Weights.State' ),
    c( 'SBJ.DBL.Weights.District', 'WGH.DBL.Weights.District' ),
    c( 'SBJ.DBL.Weights.County', 'WGH.DBL.Weights.County' )

  )
  colnames(dtf_names) <- c( 'swaap', 'codebook' )
  dtf_names <- as.data.frame(dtf_names)

  # Rename variables in a data frame
  if ( !is.null(dtf_data ) ) {

    chr_columns <- colnames(dtf_data)

    # Change from swaap to codebook
    if ( !lgc_reverse ) {

      for ( k in seq_along(chr_columns) )
        if ( any( dtf_names$swaap %in% chr_columns[k] ) )
          chr_columns[k] <- dtf_names$codebook[
            dtf_names$swaap %in% chr_columns[k]
          ]

      # Close 'Change from swaap to codebook'
    } else {

      for ( k in seq_along(chr_columns) )
        if ( any( dtf_names$codebook %in% chr_columns[k] ) )
          chr_columns[k] <- dtf_names$swaap[
            dtf_names$codebook %in% chr_columns[k]
          ]

      # Close else for 'Change from swaap to codebook'
    }

    colnames(dtf_data) <- chr_columns

    return( dtf_data )

    # Close 'Rename variables in a data frame'
  }


  # Rename variables in a character vector
  if ( !is.null(chr_columns ) ) {

    # Change from swaap to codebook
    if ( !lgc_reverse ) {

      for ( k in seq_along(chr_columns) )
        if ( any( dtf_names$swaap %in% chr_columns[k] ) )
          chr_columns[k] <- dtf_names$codebook[
            dtf_names$swaap %in% chr_columns[k]
          ]

      # Close 'Change from swaap to codebook'
    } else {

      for ( k in seq_along(chr_columns) )
        if ( any( dtf_names$codebook %in% chr_columns[k] ) )
          chr_columns[k] <- dtf_names$swaap[
            dtf_names$codebook %in% chr_columns[k]
          ]

      # Close else for 'Change from swaap to codebook'
    }

    return( chr_columns )

    # Close 'Rename variables in a data frame'
  }

  return( dtf_names )
}

#### 13) swaap_data.survey_summary ####
#' Summary of Survey Details
#'
#' Function that provides summary of
#' survey (number of surveys, number removed
#' due to data cleaning, breakdowns by
#' school type, etc.).
#'
#' @param dtf_SRV A data frame, assumed to be
#'   output from [swaap::swaap_data.merge].
#' @param chr_times A character vector with
#'   elements in the form \code{'YYYY Fall'}
#'   or \code{'YYYY Spring'}, indicating which
#'   time points to include.
#'
#' @returns Output to the console window with
#' relevant details.
#'
#' @export

swaap_data.survey_summary <- function(
    dtf_SRV,
    chr_times = NULL ) {

  if ( is.null(chr_times) )
    chr_times <- sort( unique( dtf_SRV$SSS.CHR.SurveyYearSemester ) )

  dtf_SMM <- dtf_SRV[
    dtf_SRV$SSS.CHR.SurveyYearSemester %in% chr_times,
  ]

  dtf_SCH <- aggregate(
    dtf_SMM$SSS.INT.SchoolEnrollment,
    list( dtf_SMM$SSS.CHR.SurveyYearSemester,
          dtf_SMM$SSS.INT.SchoolCode,
          dtf_SMM$SSS.INT.Grade ),
    function(x) {
      num_out <- c(
        max( length(x), unique(x) ),
        length(x),
        NA
      )
      num_out[3] <- num_out[2] / num_out[1]
      return( num_out )
    }
  )
  colnames(dtf_SCH) <- c(
    'Time', 'School', 'Grade', 'Stats'
  )

  message( 'Schools' )
  message( paste0( '  ', length( unique( dtf_SCH$School ) ) ) )

  dtf_SCH_extra <- aggregate(
    dtf_SCH$Grade,
    list( dtf_SCH$School ),
    function (x) {
      chr_out <- 'High school'
      if ( any( x %in% 6:8 ) )
        chr_out <- 'Middle school'
      if ( any( x %in% 6:8 ) & any(x %in% 9:12 ) )
        chr_out <- 'Combined'

      return( chr_out )
    }
  )

  print( table( dtf_SCH_extra[[2]] ) )

  dtf_RMV <- attributes( dtf_SRV$QLT.LGC.Remove )$swaap.summary_removed
  dtf_RMV <- dtf_RMV[
    dtf_RMV$Data %in% chr_times,
  ]

  int_surveyed <-
    nrow( dtf_SMM ) + sum( dtf_RMV$Records.Removed )
  int_removed <- sum( dtf_RMV$Records.Removed )

  message( 'Total surveyed' )
  message( paste0( '  ', int_surveyed ) )
  message( 'Met initial exclusion criteria' )
  message(
    paste0( '  ', int_removed, ' (',
            round( 100*int_removed/int_surveyed, 1 ), '%)' )
  )
  message( 'Viable surveys' )
  message( paste0( '  ', nrow(dtf_SMM) ) )

  # Loop over times
  for ( j in seq_along(chr_times) ) {

    lgc_time <- dtf_SMM$SSS.CHR.SurveyYearSemester %in%
      chr_times[j]

    message( paste0( '    ', chr_times[j], ': ', sum(lgc_time) ) )

    # Close 'Loop over times'
  }

  dtf_SCH <- cbind(
    dtf_SCH[, 1:3],
    dtf_SCH[[4]]
  )
  colnames(dtf_SCH) <- c( 'Time', 'School', 'Grade',
                          'Enrolled', 'Surveyed', 'Proportion' )

  dtf_SCH_extra <- aggregate(
    dtf_SCH$Proportion,
    list( Time = dtf_SCH$Time, School = dtf_SCH$School ),
    mean
  )

  # Loop over times and schools
  for ( j in 1:nrow(dtf_SCH_extra) ) {

    lgc_rows <-
      dtf_SCH$Time %in% dtf_SCH_extra$Time[j] &
      dtf_SCH$School %in% dtf_SCH_extra$School[j]

    dtf_SCH_extra[[3]][j] <- sum(
      dtf_SCH$Surveyed[lgc_rows]
    ) / sum( dtf_SCH$Enrolled[lgc_rows] )

    # Close 'Loop over times and schools'
  }

  dtf_AGG <- aggregate(
    dtf_SCH_extra[[3]],
    list( dtf_SCH_extra$Time ),
    function(x) c( mean(x, na.rm = T ), sd(x, na.rm = T ) )
  )

  message( 'Average response rate across schools' )

  # Loop over times
  for ( j in 1:nrow(dtf_AGG) ) {

    message( paste0(
      dtf_AGG[[1]], ': ',
      round( 100*dtf_AGG[[2]][j, 1], 1 ),
      '% (',
      round( 100*dtf_AGG[[2]][j, 2], 1 ),
      ')'
    ) )

    # Close 'Loop over times'
  }

}

#### 14) swaap_data.enrollment ####

swaap_data.enrollment <- function(
    dtf_SRV,
    chr_times = NULL,
    chr_linked = '',
    chr_outcome = '' ) {

  if ( is.null(chr_times) )
    chr_times <- sort( unique( dtf_SRV$SSS.CHR.SurveyYearSemester ) )

  dtf_SMM <- dtf_SRV[
    dtf_SRV$SSS.CHR.SurveyYearSemester %in% chr_times,
  ]

  chr_LTP <-
    dtf_SMM |> swaap::swaap_link.timepoints()
  chr_ATP <- c(
    chr_LTP,
    sort( unique( dtf_SMM$SSS.INT.TimePoints ) )
  )

  # Check for exact linkage
  if ( chr_linked %in% c( 'Exact', 'exact' ) ) {

    lgc_linked <- grepl(
      'LNK.CHR.TimePoints', colnames(dtf_SMM), fixed = TRUE
    ) &
    grepl(
      'Exact', colnames(dtf_SMM), fixed = TRUE
    )
    if ( any(lgc_linked) )
      chr_linked <- colnames(dtf_SMM)[lgc_linked][1]

    # Close 'Check for exact linkage'
  }

  # Check for linkage
  if ( chr_linked == '' ) {

    lgc_linked <- colnames(dtf_SMM) %in% 'LNK.CHR.TimePoints'
    if ( any(lgc_linked) )
      chr_linked <- 'LNK.CHR.TimePoints'

    # Close 'Check for linkage'
  }

  dtf_SCH <- aggregate(
    dtf_SMM$SSS.INT.SchoolEnrollment,
    list(
      dtf_SMM$SSS.CHR.SurveyYearSemester,
      dtf_SMM$SSS.INT.SchoolCode,
      dtf_SMM$SSS.INT.Grade,
      dtf_SMM$SSS.LGC.SBIRT
    ),
    function(x) {
      num_out <- c(
        max( length(x), unique(x) ),
        length(x)
      )
      names(num_out) <- c(
        'Enrolled',
        'Surveyed'
      )
      return( num_out )
    }
  )
  colnames(dtf_SCH) <- c(
    'Time', 'School', 'Grade', 'SBIRT', 'Stats'
  )
  dtf_SCH <- cbind(
    dtf_SCH[, 1:4 ],
    dtf_SCH$Stats
  )

  int_years <- sort(
    unique( dtf_SMM$SSS.INT.SurveyYear )
  )
  chr_year_semester <- unique(
    dtf_SMM$SSS.CHR.SurveyYearSemester
  )

  dtf_SCH$Index <-
    as.numeric( substr( dtf_SCH$Time, 1, 4 ) ) - 2021
  dtf_SCH$Index[
    grepl( 'Spring', dtf_SCH$Time )
  ] <- dtf_SCH$Index[
    grepl( 'Spring', dtf_SCH$Time )
  ] + .5

  int_times <- length( unique( dtf_SCH$Index ) )
  int_schools <- length( unique( dtf_SCH$School ) )
  int_grades <- length( 6:12 )

  dtf_SCH_ALL <- data.frame(
    Index = rep(
      rep( sort( unique( dtf_SCH$Index ) ), each = int_grades ),
      int_schools
    ),
    Time = '',
    School = rep(
      sort( unique( dtf_SCH$School ) ),
      each = int_times*int_grades
    ),
    Grade = rep(
      6:12,
      int_schools*int_times
    ),
    GradeSurveyed = FALSE,
    SBIRTSchool = FALSE,
    SBIRTGrade = FALSE,
    Times = 0,
    Enrolled = 0,
    Surveyed = 0,
    Linked = 0,
    Outcome = 0
  )

  dtf_SCH_ALL$Time <- as.character(
    round( dtf_SCH_ALL$Index - .4 ) + 2021
  )
  dtf_SCH_ALL$Time[
    ( dtf_SCH_ALL$Index - round(dtf_SCH_ALL$Index) ) != 0
  ] <- paste0(
    dtf_SCH_ALL$Time[
      ( dtf_SCH_ALL$Index - round(dtf_SCH_ALL$Index) ) != 0
    ],
    ' Spring'
  )
  dtf_SCH_ALL$Time[
    ( dtf_SCH_ALL$Index - round(dtf_SCH_ALL$Index) ) == 0
  ] <- paste0(
    dtf_SCH_ALL$Time[
      ( dtf_SCH_ALL$Index - round(dtf_SCH_ALL$Index) ) == 0
    ],
    ' Fall'
  )

  # Loop over rows
  for ( r in 1:nrow(dtf_SCH_ALL) ) {

    lgc_match <-
      dtf_SCH$Index == dtf_SCH_ALL$Index[r] &
      dtf_SCH$School == dtf_SCH_ALL$School[r] &
      dtf_SCH$Grade == dtf_SCH_ALL$Grade[r]

    # Successful match
    if ( any(lgc_match) ) {

      dtf_SCH_ALL$SBIRTGrade[r] <-
        dtf_SCH$SBIRT[lgc_match]
      dtf_SCH_ALL$Enrolled[r] <-
        dtf_SCH$Enrolled[lgc_match]
      dtf_SCH_ALL$Surveyed[r] <-
        dtf_SCH$Surveyed[lgc_match]

      # If linked time points found
      if ( chr_linked != "" ) {

        lgc_linked <-
          dtf_SMM$SSS.CHR.SurveyYearSemester %in% dtf_SCH_ALL$Time[r] &
          dtf_SMM$SSS.INT.SchoolCode %in% dtf_SCH_ALL$School[r] &
          dtf_SMM$SSS.INT.Grade %in% dtf_SCH_ALL$Grade[r]

        dtf_SCH_ALL$Linked[r] <- sum(
          dtf_SMM[[ chr_linked ]][lgc_linked] %in% chr_LTP
        )

        # Close 'If linked time points found'
      }

      # If outcome provided
      if ( chr_outcome != '' ) {

        lgc_outcome <-
          dtf_SMM$SSS.CHR.SurveyYearSemester %in% dtf_SCH_ALL$Time[r] &
          dtf_SMM$SSS.INT.SchoolCode %in% dtf_SCH_ALL$School[r] &
          dtf_SMM$SSS.INT.Grade %in% dtf_SCH_ALL$Grade[r]

        dtf_SCH_ALL$Outcome[r] <- sum(
          !is.na( dtf_SMM[[ chr_outcome ]][lgc_outcome] )
        )

        # Close 'If outcome provided'
      }

      # Close 'Successful match'
    }

    # Close 'Loop over rows'
  }

  dtf_SCH_ALL$Rate <- dtf_SCH_ALL$Surveyed / dtf_SCH_ALL$Enrolled
  dtf_SCH_ALL$Rate[
    dtf_SCH_ALL$Enrolled == 0
  ] <- 0

  # Loop over schools
  for (s in unique(dtf_SCH_ALL$School) ) {

    lgc_school <- dtf_SCH_ALL$School == s

    dtf_SCH_ALL$SBIRTSchool[lgc_school] <- any(
      dtf_SCH_ALL$SBIRTGrade[lgc_school]
    )

    dtf_SCH_ALL$Times[lgc_school] <- length(
      unique( dtf_SCH_ALL$Time[
        lgc_school & dtf_SCH_ALL$Surveyed > 0
      ] )
    )

    # Close 'Loop over schools'
  }

  dtf_SCH_ALL$GradeSurveyed <- dtf_SCH_ALL$Surveyed > 0

  return( dtf_SCH_ALL )
}


if ( FALSE ) {

  swaap_data.enrollment_plot <- function(
    dtf_SRV,
    chr_measure,
    chr_groups,
    chr_function = 'Mean',
    num_yl = c( 0, 1 ),
    num_yl_inc = seq( 0, 1, .1 ),
    chr_linked = 'Exact',
    chr_times = NULL,
    chr_outcome = '',
    num_mar = c( 6, 2, 1, .5 ),
    num_shift = 2.75,
    num_labels = NULL,
    lgc_new = TRUE ) {

    dtf_TBA <- dtf_SRV

    if ( !is.null( dtf_SRV$SRV.CHR.DataSet ) )
      dtf_TBA <- dtf_SRV |>
        swaap::swaap_data.codebook_names( lgc_reverse = TRUE )

    dtf_SCH_ALL <- swaap_data.enrollment(
      dtf_TBA,
      chr_times = chr_times,
      chr_linked = chr_linked,
      chr_outcome = chr_outcome
    )

    dtf_SCH_ALL$SurveyRate <-
      dtf_SCH_ALL$Surveyed / dtf_SCH_ALL$Enrolled
    dtf_SCH_ALL$LinkRate <-
      dtf_SCH_ALL$Linked / dtf_SCH_ALL$Surveyed
    dtf_SCH_ALL$CompleteRate <-
      dtf_SCH_ALL$Outcome / dtf_SCH_ALL$Surveyed

    lst_functions <- list(
      Mean = function(x) mean(x, na.rm = T ),
      Distinct = function(x) dplyr::n_distinct(x[!is.na(x)])
    )

    if (lgc_new) x11(width = 5, height = 5)

    par( mar = num_mar )

    num_xl <- c( 0, 1 )

    dtf_TBP <- NULL

    # Any groups
    if ( length(chr_groups) != 0 ) {

      dtf_TBP <- dtf_SCH_ALL |>
        dplyr::filter(
          GradeSurveyed
        ) |>
        dplyr::group_by_at(
          chr_groups
        ) |>
        dplyr::summarise_at(
          chr_measure, lst_functions[[ chr_function ]]
        ) |>
        data.frame()

      num_xl <- c( 0, nrow(dtf_TBP) + 1 )

      # Close 'Any groups'
    }

    plot(
      num_xl, num_yl,
      type = 'n', xaxt = 'n', yaxt = 'n',
      ylab = '', xlab = '', bty = 'n'
    )

    segments(
      rep( num_xl[1], length(num_yl_inc) ),
      num_yl_inc,
      rep( num_xl[2], length(num_yl_inc) ),
      num_yl_inc,
      col = 'grey90'
    )

    segments(
      c( num_xl[1], num_xl[2], num_xl[1], num_xl[1] ),
      c( num_yl[1], num_yl[1], num_yl[1], num_yl[2] ),
      c( num_xl[1], num_xl[2], num_xl[2], num_xl[2] ),
      c( num_yl[2], num_yl[2], num_yl[1], num_yl[2] ),
      lwd = 2
    )

    # Any groups
    if ( length(chr_groups) != 0 ) {

      # More than one group
      if ( length(chr_groups) > 1 ) {

        int_L <- dplyr::n_distinct(
          dtf_TBP[[1]]
        )

        chr_col <- paste0(
          'grey',
          round( 100*seq( .3, .9, length.out = int_L ) )
        )
        chr_col <- chr_col[
          as.numeric(
            as.factor( dtf_TBP[[1]] )
          )
        ]

        # Close 'More than one group'
      } else {

        chr_col <- rep( 'grey', nrow(dtf_TBP ) )

        # Close else for 'More than one group'
      }

      # Loop over rows
      for ( r in 1:nrow(dtf_TBP) ) {

        polygon(
          ( 1:nrow(dtf_TBP) )[r] + .5*c( -1, -1, 1, 1 ),
          dtf_TBP[[ ncol(dtf_TBP) ]][r]*c( 0, 1, 1, 0 ),
          col = chr_col[r], border = 'black'
        )

        # Close 'Loop over rows'
      }

      # Close 'Any groups'
    }

    if ( is.null(num_labels) )
      num_labels <- num_xl[1]

    # If data exists
    if ( !is.null(dtf_TBP) ) {

      # Loop over grouping variables
      for ( g in 1:( ncol(dtf_TBP) - 1 ) ) {

        axis(
          side = 1,
          1:nrow(dtf_TBP),
          as.character( dtf_TBP[[g]] ),
          las = 3,
          cex.axis = .9,
          line = -1 + num_shift*(g-1),
          tick = FALSE
        )

        axis(
          side = 1,
          num_labels,
          chr_groups[g],
          line = -1 + num_shift*(g-1),
          tick = FALSE,
          xpd = NA
        )

        # Close 'Loop over grouping variables'
      }

      # Close 'If data exists'
    }

    axis(
      side = 2,
      num_yl_inc,
      cex.axis = .9,
      line = -1.25,
      tick = FALSE
    )

    mtext(
      chr_measure, side = 2, line = .75, cex = .9
    )

    return( dtf_TBP )
  }

  dtf_SRV |>
    swaap_data.enrollment_plot(
      'School', 'Times',
      chr_function = 'Distinct',
      num_yl = c(0, 50),
      num_yl_inc = seq(0, 50, 10)
    )

  dtf_SRV |>
    swaap_data.enrollment_plot(
      'SurveyRate', 'Grade'
    )

  dtf_SRV |>
    swaap_data.enrollment_plot(
      'SurveyRate', 'SBIRTSchool',
      num_mar = c( 2, 6, 2, .5 )
    )

  dtf_SRV |>
    swaap_data.enrollment_plot(
      'SurveyRate', 'SBIRTGrade',
      num_mar = c( 2, 6, 2, .5 )
    )

  dtf_SRV |>
    swaap_data.enrollment_plot(
      'SurveyRate', c( 'SBIRTGrade', 'Time' ),
      num_mar = c( 6, 4, 1, .5 ),
      num_labels = -1
    )

}




