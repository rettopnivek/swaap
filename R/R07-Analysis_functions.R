# Analysis functions
# Written by Kevin Potter
# email: kpotter5@mgh.harvard.edu
# Please email me directly if you
# have any questions or comments
# Last updated 2025-07-03

# Table of contents
# 1) swaap_analysis.prep_project
# 2) swaap_analysis.source_scripts

#### 1) swaap_analysis.prep_project ####
#' Prep Analysis Project
#'
#' Function to generate folders and R script
#' templates for an analysis project looking
#' at the school-wide assessment data.
#'
#' @param chr_name The user's name.
#' @param chr_email The user's email.
#' @param chr_project The project label to begin
#'   each function with.
#'
#' @author Kevin Potter
#'
#' @returns As a side effect creates the 'Source'
#' and 'R' folders, and creates a '_targets.R' and
#' 'R/R01-Data_processing.R' scripts.
#'
#' @export

swaap_analysis.prep_project <- function(
    chr_name,
    chr_email,
    chr_project ) {

  # Current working directory
  chr_dir <- getwd()

  chr_files_folders <- dir()

  lgc_exists <- 'R' %in% chr_files_folders

  # Create folder for R scripts
  if ( !lgc_exists ) {

    dir.create( 'R' )

    # Close 'Create source folder'
  }

  lgc_exists <- 'Source' %in% chr_files_folders

  # Create source folder
  if ( !lgc_exists ) {

    dir.create( 'Source' )

    # Close 'Create source folder'
  }

  chr_targets <- c(
    '# Script to generate targets',
    paste0( '# Written by ', chr_name ),
    paste0( '# email: ', chr_email ),
    '# Please email me directly if you',
    '# have any questions or comments',
    paste0( '# Last updated ',
            format( Sys.time(), '%Y-%m-%d' ) ),
    '',
    '# Table of contents',
    '# 0) Template for function documentation',
    '# 1) Initial setup',
    '# 2) Generate targets',
    '',
    '### To generate targets ###',
    "# 1. Click on 'Session' and in pull-down menu for ",
    "#    'Set Working Directory' select 'To Project Directory'",
    "# 2. <Optional>",
    "#    - Clear workspace (click broom icon)",
    "#    - Restart R (Click on 'Session' and select 'Restart R')",
    "# 3. Type in console: targets::tar_make()",
    "",
    "#### 0) Template for function documentation ####",
    "# Title ",
    "# ",
    "# ... ",
    "# ",
    "# @param 'obj_x' An R object (see target output ",
    paste0( "#   from the '", chr_project, ".RXX.example' function)." ),
    "# ",
    "# @details ",
    "# Prerequisites:",
    "#   * The R package '?' (version ?)",
    paste0( "#   * The '", chr_project, '.RXX',
            ".example' function" ),
    "# ",
    "# @returns ...",
    "",
    "# Example path to codebook for variables",
    '# "CAM Data/SWA-2015/Output/Codebooks/2023"',
    '#### 1) Initial setup ####',
    '',
    '# Copy data from source folder',
    "# swaap::swaap_data.download(",
    '#   "<Path to dropbox>",',
    "#   c( '2023 Fall', '2024 Fall' ),",
    "#   chr_copy_to = 'Source'",
    "# )",
    "",
    '# Load in package to manage generation of target outputs',
    "# install.packages( 'targets' )",
    'library(targets)',
    '',
    '# Source in all R scripts with functions',
    "swaap::swaap_analysis.source_scripts()",
    '',
    "# Load in packages",
    "tar_option_set(",
    "  packages = c(",
    "    # Data frame manipulation and '%>%' operator",
    "    #   To install:",
    "    #   install.packages( 'dplyr' )",
    "    'dplyr',",
    "    # Functions for linking records",
    "    #   To install:",
    "    #   install.packages( 'fastLink' )",
    "    'fastLink',",
    "    # School-wide assessment analysis prep functions",
    "    #   To install:",
    "    #   devtools::install_github( 'rettopwnivek/swaap' )",
    "    'swaap'",
    "  )",
    ")",
    "",
    "#### 2) Generate targets ####",
    "",
    "list(",
    "  ",
    "  #### 2.1) Paths to data files ####",
    "  ",
    "  # - Path to .csv files with data",
    "  #   targets::tar_load( chr_path.source_files )",
    "  tar_target(",
    "    chr_path.source_files, ",
    '    swaap::swaap_data.files( "Source" )',
    "  ),",
    "  ",
    "  # - Path to .csv with source info",
    "  #   targets::tar_load( chr_path.source_info )",
    "  tar_target(",
    "    chr_path.source_info, ",
    '    swaap::swaap_data.files( "Source", chr_tag = "SWA-Download-" )',
    "  ),",
    "  ",
    "  #### 2.2) Load in source data ####",
    "  ",
    "  # - Survey data [2023 Fall]",
    "  #   targets::tar_load( dtf_SRV.Y23F )",
    "  tar_target(",
    "    dtf_SRV.Y23F,",
    "    read.csv(",
    "      chr_path.source_files['2023Fall'],",
    "      stringsAsFactors = FALSE",
    "    )",
    "  ),",
    "  ",
    "  # - Survey data [2024 Fall]",
    "  #   targets::tar_load( dtf_SRV.Y24F )",
    "  tar_target(",
    "    dtf_SRV.Y24F,",
    "    read.csv(",
    "      chr_path.source_files['2024Fall'],",
    "      stringsAsFactors = FALSE",
    "    )",
    "  ),",
    "  ",
    "  # - Information on source data",
    "  #   targets::tar_load( dtf_SRV.Source )",
    "  tar_target(",
    "    dtf_SRV.Source,",
    "    read.csv(",
    "      chr_path.source_info,",
    "      stringsAsFactors = FALSE",
    "    )",
    "  )",
    "  ",
    ")"
  )

  chr_targets <- paste(
    chr_targets, collapse = "\n"
  ) |> paste0( "\n" )

  write(
    chr_targets,
    file = '_targets.R'
  )

  chr_R <- c(
    '# Data processing',
    paste0( '# Written by ', chr_name ),
    paste0( '# email: ', chr_email ),
    '# Please email me directly if you',
    '# have any questions or comments',
    paste0( '# Last updated ',
            format( Sys.time(), '%Y-%m-%d' ) ),
    '',
    '# Table of contents',
    paste0( '# 1) ', chr_project, '.R01.process_data' ),
    '',
    paste0( '#### 1) ', chr_project, '.R01.process_data ####' ),
    '',
    paste0( chr_project, 'R01.process_data <- function(' ),
    '  lst_data,',
    '  dtf_SRV.Source ) {',
    '  ',
    '  # Debugging',
    '  if ( FALSE ) {',
    '    ',
    '    # Load in packages',
    '    library(dplyr)',
    '    library(swaap)',
    '    ',
    '    # Load in targets',
    '    targets::tar_load( dtf_SRV.Y23F )',
    '    targets::tar_load( dtf_SRV.Y24F )',
    '    targets::tar_load( dtf_SRV.Source )',
    '    ',
    '    # Test function',
    paste0( '    dtf_SRV.Full <- ', chr_project, '.R01.process_data(' ),
    '      lst_data = list(',
    '        Y23F = dtf_SRV.Y23F,',
    '        Y24F = dtf_SRV.Y24F',
    '      ),',
    '      dtf_SRV.Source',
    '    )',
    '  ',
    "    # Close 'Debugging'",
    '  }',
    '  ',
    '  dtf_SRV.Full <- swaap_data.merge(',
    '    lst_data,',
    '    chr_source_files = dtf_SRV.Source$Partial,',
    '    int_grades = 9:12',
    '  )',
    '  ',
    '  # Identify duplicate records',
    '  dtf_SRV.Full <- dtf_SRV.Full |>',
    '    swaap_link(',
    '      lst_sets = swaap_link.helper.input(',
    "        dtf_SRV.Full, 'sets', obj_extra = 'duplicates'",
    '      )',
    '    )',
    '  dtf_SRV.Full <- dtf_SRV.Full |>',
    '    swaap_link.helper.trim()',
    '  ',
    '  dtf_SRV.Full <- dtf_SRV.Full |>',
    '    dplyr::filter(',
    '      !QLT.LGC.RemoveDuplicate',
    '    )',
    '  chr_linked <- swaap_select.linked(lgc_subset = FALSE)',
    '  dtf_SRV.Full <- dtf_SRV.Full |>',
    '    dplyr::select( -dplyr::all_of(chr_linked) )',
    '  ',
    '  dtf_SRV.Full <- dtf_SRV.Full |>',
    '    swaap_link( lgc_remove_duplicates = TRUE )',
    '  ',
    '  return( dtf_SRV.Full )',
    '}'
  )

  write(
    chr_R,
    file = 'R/R01-Data_processing.R'
  )

  message( 'Files and folders added' )
}

#### 2) swaap_analysis.source_scripts ####
#' Source in R scripts
#'
#' Function to source in code defined in a set of
#' .R files all at once.
#'
#' @param chr_folder A character string, an optional
#'   path to the folder with the R scripts - by
#'   default assumed to be \code{'R'}.
#'
#' @author Kevin Potter
#'
#' @returns As a side effect sources all .R files.
#'
#' @export

swaap_analysis.source_scripts <- function(
    chr_folder = 'R' ) {

  # If folder provided
  if ( chr_folder != '' ) {

    chr_files_folders <- dir()

    if ( !chr_folder %in% chr_files_folders )
      stop( paste0( "Folder ", chr_folder, " with scripts not found" ) )

    # Close 'If folder provided'
  } else {

    chr_folder <- "."

    # Close else for 'If folder provided'
  }

  chr_R <- dir( path = chr_folder )

  # Identify R scripts
  chr_ext <- sapply(
    chr_R, function(s)
      utils::tail( strsplit( s, split = '.', fixed = TRUE )[[1]], n = 1 )
  )
  chr_R <- chr_R[ chr_ext %in% 'R' ]

  # If .R files found
  if ( length(chr_R) > 0 ) {

    # Loop over files
    for ( r in seq_along(chr_R) ) {

      # Source in functions
      source(
        paste0( chr_folder, '/', chr_R[r] )
      )

      # Close 'Loop over files'
    }

    # Close 'If .R files found'
  } else {

    stop( "No .R files found" )

    # Close else for 'If .R files found'
  }

}

#### 3) swaap_analysis.schools ####
#' Create School-Level Summary
#'
#' Create a school-level data set and optionally create
#' school-level summaries for a specified set of measures.
#'
#' @param dtf_data A data frame, assumed to
#'   follow the standardized format for the
#'   school-wide assessment data.
#' @param chr_measure An optional character vector, the
#'   additional columns in \code{dtf_data} over which to
#'   create school-level summaries.
#' @param fun_summary A function to compute the univariate
#'   statistic for the school-level summaries.
#'
#' @returns A data frame.
#'
#' @export

swaap_analysis.schools <- function(
    dtf_data,
    chr_measure = NULL,
    fun_summary = NULL ) {

  # Default summary statistic
  if ( is.null(fun_summary) ) {

    fun_summary <- function(x) mean(x, na.rm = TRUE )

    # Close 'Default summary statistic'
  }

  dtf_SCH <- dtf_data |>
    dplyr::group_by(
      District = SSS.INT.DistrictCode,
      School = SSS.INT.SchoolCode,
      Wave = SSS.INT.LongitudinalWave,
      Time = SSS.INT.TimePoint,
      Grade = SSS.INT.Grade,
      Year = SSS.INT.SurveyYear,
      Semester = SSS.CHR.Semester
    ) |>
    dplyr::summarise(
      Enrolled = unique(
        SSS.INT.SchoolEnrollment
      ),
      Records = length( IDN.CHR.Record.ID ),
      .groups = 'drop'
    ) |>
    data.frame()

  # Additional measures to summarize over
  if ( !is.null( chr_measure ) ) {

    # Vector not named
    if ( is.null( names(chr_measure) ) ) {

      names(chr_measure) <- sapply(
        chr_measure, function(s) {
          strsplit( s, '.', fixed = TRUE )[[1]] |> tail(n = 1)
        }
      )

      # Close 'Vector not named'
    }

    # Loop over measures
    for ( m in seq_along(chr_measure) ) {

      # Initialize summary column
      dtf_SCH[[ names(chr_measure)[m] ]] <- NA

      # Loop over rows
      for ( r in 1:nrow(dtf_SCH) ) {

        lgc_subset <-
          dtf_data$SSS.INT.SurveyYear %in% dtf_SCH$Year[r] &
          dtf_data$SSS.INT.SchoolCode %in% dtf_SCH$School[r] &
          dtf_data$SSS.INT.Grade %in% dtf_SCH$Grade[r] &
          dtf_data$SSS.INT.LongitudinalWave %in% dtf_SCH$Wave[r] &
          dtf_data$SSS.INT.SurveyYear %in% dtf_SCH$Year[r] &
          dtf_data$SSS.INT.TimePoint %in% dtf_SCH$Time[r]

        dtf_SCH[[ names(chr_measure)[m] ]][r] <- fun_summary(
          dtf_data[[ chr_measure[m] ]][lgc_subset]
        )

        # Close 'Loop over rows'
      }

      # Close 'Loop over measures'
    }

    # Close 'Additional measures to summarize over'
  }

  return( dtf_SCH )
}
