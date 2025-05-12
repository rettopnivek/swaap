# Misc. data processing functions
# Written by Kevin Potter
# email: kpotter5@mgh.harvard.edu
# Please email me directly if you
# have any questions or comments
# Last updated 2025-05-12

# Table of contents
# 1) swaap_data.merge
# 2) swaap_data.missing
# 3) swaap_data.files
# 4) swaap_data.attr
# 5) swaap_data.subset
#   5.1) swaap_data.internal.copy_attr
# 6) swaap_data.download

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
    message( "  'experience'," )
    message( "  'inventories'," )
    message( "  'linking'," )
    message( "  'quality'" )
    message( ")" )
    message("")

    # Select functions
    message( "chr_select = c(" )
    message( "  'base'," )
    message( "  'contact'," )
    message( "  'demographics'," )
    message( "  'experience'," )
    message( "  'inventories'" )
    message( "  'linking'," )
    message( "  'quality'," )
    message( "  'SBIRT'," )
    message( "  'substances'" )
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
      'swaap_select.contact',
      'swaap_select.demographics',
      'swaap_select.linking',
      'swaap_select.SBIRT',
      'swaap_select.substances',
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

  # Use all provided grades
  if ( is.null( int_grades ) ) {

    int_grades <- sapply(
      seq_along(lst_data), function(d) {
        unique( lst_data[[d]]$SSS.INT.Grade )
      }
    ) |> unique() |> sort()

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

#### 7) ... ####

# swaap_data.normalize <- function(
#     dtf_data,
#     chr_variable ) {
#
#   lst_variables <- list(
#     race = c(
#       'SBJ.CHR.Race'
#     )
#   )
#
# }


