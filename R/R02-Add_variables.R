# Functions to add variables
# Written by Kevin Potter
# email: kpotter5@mgh.harvard.edu
# Please email me directly if you
# have any questions or comments
# Last updated 2025-05-08

# Table of contents
# I) swaap_add.ID
# Q) swaap_add.quality
#   Q.1) fun_flag_for_removal
# S) swaap_add.SBIRT
# S) swaap_add.school_enrollment
# S) swaap_add.substances
# T) swaap_add.time_point
# Y) swaap_add.year_and_semester
#   Y.1) fun_update

#### I) swaap_add.ID ####
#' Add Identifier Variable
#'
#' Function to add an identifier variable
#' that includes information on the
#' year, semester, and grade level for
#' a record.
#'
#' @param dtf_data A data frame, assumed to
#'   follow the standardized format for the
#'   school-wide assessment data.
#' @param chr_label A character string, the
#'   label for the identifier column, so
#'   that a new column \code{'IDN.CHR.<Label>.ID'}
#'   is created.
#'
#' @author Kevin Potter
#'
#' @returns A data frame with an additional
#' column.
#'
#' @export

swaap_add.ID <- function(
    dtf_data,
    chr_type ) {

  chr_columns <- colnames(dtf_data)

  # Check if year and semester provided
  if ( !'SSS.CHR.SurveyYearSemester' %in% chr_columns ) {

    dtf_data <- swaap_add.year_and_semester(
      dtf_data
    )

    # Close 'Check if year and semester provided'
  }

  int_rows <- 1:nrow(dtf_data)

  chr_new <- paste0( 'IDN.CHR.', chr_type, '.ID' )

  chr_grade <- paste0(
    'G0', dtf_data$SSS.INT.Grade
  )
  chr_grade[
    chr_grade %in% 'G010'
  ] <- 'G10'
  chr_grade[
    chr_grade %in% 'G011'
  ] <- 'G11'
  chr_grade[
    chr_grade %in% 'G012'
  ] <- 'G12'
  chr_grade[
    chr_grade %in% 'G013'
  ] <- 'G13'

  chr_year <- paste0(
    'Y', sapply(
      dtf_data$SSS.CHR.SurveyYearSemester, function(y) {
        substr(y, 3, 4 )
      }
    )
  )

  chr_semester <- rep( 'SF', nrow(dtf_data) )
  chr_semester[
    grepl( 'Spring', dtf_data$SSS.CHR.SurveyYearSemester )
  ] <- 'SS'

  chr_ID <- paste0(
    'UID_',
    chr_year, '_',
    chr_semester, '_',
    chr_grade, ' ',
    int_rows
  )

  dtf_data[[ chr_new ]] <- chr_ID

  return( dtf_data )
}

#### Q) swaap_add.quality ####
#' Add Variables for Quality Checks
#'
#' Function to add variables with quality
#' check information. Includes a variable
#' flagging records to remove because
#' (a) they are missing a school code,
#' (b) they are missing a grade level,
#' (c) they are in a grade outside of 6-12.
#'
#' @param dtf_data A data frame, assumed to
#'   follow the standardized format for the
#'   school-wide assessment data.
#'
#' @author Kevin Potter
#'
#' @returns A data frame with additional
#' columns
#' \code{'QLT.LGC.Remove'} and
#' \code{'QLT.CHR.Remove'} indicating
#' records that failed to meet various
#' quality standards.
#'
#' @export

swaap_add.quality <- function(
    dtf_data ) {

  # Initialize variables
  dtf_data$QLT.LGC.Remove <- FALSE
  dtf_data$QLT.CHR.Remove <- ''

  chr_columns <- colnames(dtf_data)

  #### Q.1) fun_flag_for_removal ####
  fun_flag_for_removal <- function(
    dtf_data,
    chr_check,
    chr_note,
    chr_type = 'NA',
    vec_value = NULL ) {

    # If column exists
    if ( any( chr_check %in% chr_check ) ) {

      chr_check <- chr_check[ chr_check %in% chr_check ][1]

      # Check for NA value
      if ( chr_type %in% 'NA' ) {

        lgc_remove <- is.na( dtf_data[[ chr_check ]] )

        # Close 'Check for NA value'
      }

      # Check if equals value
      if ( chr_type %in% 'value' ) {

        lgc_remove <- dtf_data[[ chr_check ]] %in% vec_value

        # Close 'Check if equals value'
      }

      dtf_data$QLT.LGC.Remove[lgc_remove] <- TRUE
      dtf_data$QLT.CHR.Remove[lgc_remove] <- chr_note

      # Close 'If column exists'
    }

    return( dtf_data )
  }

  # No school code
  dtf_data <- dtf_data |>
    fun_flag_for_removal(
      c(
        'SSS.INT.SchoolCode',
        'SSS.INT.School.Code'
      ),
      'Missing school code',
      chr_type = 'NA'
    )

  # No grade level
  dtf_data <- dtf_data |>
    fun_flag_for_removal(
      c(
        'SSS.INT.Grade'
      ),
      'Missing grade level',
      chr_type = 'NA'
    )

  # Grade level of 13
  dtf_data <- dtf_data |>
    fun_flag_for_removal(
      c(
        'SSS.INT.Grade'
      ),
      'Not in grades 6-12',
      chr_type = 'value',
      vec_value = 13
    )

  dtf_summary <- dtf_data |>
    dplyr::filter(
      QLT.LGC.Remove
    ) |>
    dplyr::group_by(
      Reason = QLT.CHR.Remove
    ) |>
    dplyr::summarise(
      Records.Removed = sum( QLT.LGC.Remove )
    ) |>
    data.frame()

  attributes( dtf_data$QLT.LGC.Remove ) <- list(
    swaap.summary_removed = dtf_summary
  )

  return( dtf_data )
}

#### S) swaap_add.SBIRT ####
#' Add Variables Tracking SBIRT Sample
#'
#' Function to add variables indicating the
#' subset of the data belonging to the
#' screening, brief intervention, and referral
#' to treatment (SBIRT) sample for the
#' school-wide assessment data, along with
#' variables tracking the time point.
#'
#' @param dtf_data A data frame, assumed to
#'   follow the standardized format for the
#'   school-wide assessment data.
#'
#' @author Jasmeen Kaur; Kevin Potter
#'
#' @returns A data frame with additional
#' columns \code{'SSS.LGC.SBIRT'} (\code{TRUE}
#' for students part of the SBIRT sample),
#' \code{'SSS.INT.SBIRTTimePoint'} (ranging
#' from 0 - 4, where 0 is the baseline time
#' point and 4 is month 24 of the study),
#' and \code{'SSS.INT.RecruitmentWave'}
#' (ranging from 1 - 3).
#'
#' @export

swaap_add.SBIRT <- function(
    dtf_data ) {

  chr_columns <- colnames(dtf_data)

  # Check if year and semester provided
  if ( !'SSS.CHR.SurveyYearSemester' %in% chr_columns ) {

    dtf_data <- swaap_add.year_and_semester(
      dtf_data
    )

    # Close 'Check if year and semester provided'
  }

  chr_year_semester <- unique(
    dtf_data$SSS.CHR.SurveyYearSemester
  )

  # Initialize columns
  dtf_data$SSS.LGC.SBIRT <- FALSE
  dtf_data$SSS.INT.SBIRTTimePoint <- NA
  dtf_data$SSS.INT.RecruitmentWave <- NA

  # Identify SBIRT schools and grades
  lgc_SBIRT_9th <-
    dtf_data$SSS.INT.School.Code %in% c(
      1052,1101,1215,
      1110,1068,1066,
      1208,1113,1216
    )
  lgc_SBIRT_10th <-
    dtf_data$SSS.INT.School.Code %in% c(
      1112,1069,1008,1111,1116,1085,1207,
      1028,1020,1037,1098,1104,1065,1209,
      1013,1032,1105,1117,1213,1064,1212,
      1108,1106,1114,1100,1103,1007,1099,
      1107,1118,1210,1211
    )

  # Grades/waves for 2023 Fall
  if ( chr_year_semester %in% '2023 Fall' ) {

    lgc_SBIRT <-
      ( dtf_data$SSS.INT.Grade %in% 9 & lgc_SBIRT_9th ) |
      ( dtf_data$SSS.INT.Grade %in% 10 & lgc_SBIRT_10th )

    # Update columns
    dtf_data$SSS.LGC.SBIRT[lgc_SBIRT] <- TRUE
    dtf_data$SSS.INT.SBIRTTimePoint[lgc_SBIRT] <- 0
    dtf_data$SSS.INT.RecruitmentWave[lgc_SBIRT] <- 1

    # Close 'Grades/waves for 2023 Fall'
  }

  # Grades/waves for 2024 Spring
  if ( chr_year_semester %in% '2024 Spring' ) {

    lgc_SBIRT <-
      ( dtf_data$SSS.INT.Grade %in% 9 & lgc_SBIRT_9th ) |
      ( dtf_data$SSS.INT.Grade %in% 10 & lgc_SBIRT_10th )

    # Update columns
    dtf_data$SSS.LGC.SBIRT[lgc_SBIRT] <- TRUE
    dtf_data$SSS.INT.SBIRTTimePoint[lgc_SBIRT] <- 1
    dtf_data$SSS.INT.RecruitmentWave[lgc_SBIRT] <- 1

    # Close 'Grades/waves for Spring 2024'
  }

  # Grades/waves for 2024 Fall
  if ( chr_year_semester %in% '2024 Fall' ) {

    # First recruitment wave
    lgc_SBIRT <-
      ( dtf_data$SSS.INT.Grade %in% 10 & lgc_SBIRT_9th ) |
      ( dtf_data$SSS.INT.Grade %in% 11 & lgc_SBIRT_10th )

    # Update columns
    dtf_data$SSS.LGC.SBIRT[lgc_SBIRT] <- TRUE
    dtf_data$SSS.INT.SBIRTTimePoint[lgc_SBIRT] <- 2
    dtf_data$SSS.INT.RecruitmentWave[lgc_SBIRT] <- 1

    # Second recruitment wave
    lgc_SBIRT <-
      ( dtf_data$SSS.INT.Grade %in% 9 & lgc_SBIRT_9th ) |
      ( dtf_data$SSS.INT.Grade %in% 10 & lgc_SBIRT_10th )

    # Update columns
    dtf_data$SSS.LGC.SBIRT[lgc_SBIRT] <- TRUE
    dtf_data$SSS.INT.SBIRTTimePoint[lgc_SBIRT] <- 0
    dtf_data$SSS.INT.RecruitmentWave[lgc_SBIRT] <- 2

    # Close 'Grades/waves for 2023 Fall'
  }

  return( dtf_data )
}

#### S) swaap_add.school_enrollment ####
#' Add School Enrollment by Grade
#'
#' Function that adds the number of students
#' enrolled for the given grade and school
#' for each record in the data set.
#'
#' @param dtf_data A data frame, assumed to
#'   follow the standardized format for the
#'   school-wide assessment data.
#'
#' @author Kevin Potter
#'
#' @returns A data frame with additional
#' column \code{'SSS.INT.SchoolEnrollment'}
#' (the number of students enrolled for
#' the given school and grade) and
#' \code{'QLT.LGC.GradeSettoNA'} (when a
#' student reported a grade not in their
#' listed school).
#'
#' @export

swaap_add.school_enrollment <- function(
    dtf_data ) {

  dtf_data$QLT.LGC.GradeSettoNA <- FALSE
  dtf_data$SSS.INT.SchoolEnrollment <- NA

  # Loop over rows
  for ( r in 1:nrow(dtf_data) ) {

    int_out <- NA
    int_grade <- dtf_data$SSS.INT.Grade[r]

    if ( int_grade %in% 6 )
      int_out <- dtf_data$SSS.INT.Sixth.Grade.Enrollment[r]
    if ( int_grade %in% 7 )
      int_out <- dtf_data$SSS.INT.Seventh.Grade.Enrollment[r]
    if ( int_grade %in% 8 )
      int_out <- dtf_data$SSS.INT.Eighth.Grade.Enrollment[r]
    if ( int_grade %in% 9 )
      int_out <- dtf_data$SSS.INT.Ninth.Grade.Enrollment[r]
    if ( int_grade %in% 10 )
      int_out <- dtf_data$SSS.INT.Tenth.Grade.Enrollment[r]
    if ( int_grade %in% 11 )
      int_out <- dtf_data$SSS.INT.Eleventh.Grade.Enrollment[r]
    if ( int_grade %in% 12 )
      int_out <- dtf_data$SSS.INT.Twelfth.Grade.Enrollment[r]

    # Recode grade if mismatch with school grade range
    if ( int_out %in% 0 ) {

      dtf_data$SSS.INT.Grade[r] <- NA
      dtf_data$QLT.LGC.GradeSettoNA[r] <- TRUE

      # Close 'Recode grade if mismatch with school'
    } else {

      dtf_data$SSS.INT.SchoolEnrollment[r] <- int_out

      # Close else for 'Recode grade if mismatch with school'
    }

    # Close 'Loop over rows'
  }

  return( dtf_data )
}

#### S) swaap_add.source ####
#' Add Details on Sources for Data
#'
#' Function to add a column with the
#' attribute \code{'swaap.source_file_details'}
#' detailing the date and package version
#' used to prep data (and if supplied, the
#' source files for the school-wide assessment
#' data).
#'
#' @param dtf_data A data frame, assumed to
#'   follow the standardized format for the
#'   school-wide assessment data.
#' @param chr_source_files A character vector,
#'   the standardized file names for the
#'   source school-wide assessment data.
#'
#' @author Kevin Potter
#'
#' @returns A data frame with the additional
#' column \code{'SSS.CHR.SourceFileDetails'}.
#' Use the \code{attribute} function to
#' extract the details.
#'
#' @export

swaap_add.source <- function(
    dtf_data,
    chr_source_files = '' ) {

  dtf_data$SSS.CHR.SourceFileDetails <-
    'attributes(dtf_data$SSS.CHR.SourceFileDetails)'

  lst_attr <- list(
    swaap.source_file_details = list(
      date = format( Sys.time(), '%Y-%m-%d %H:%M' ),
      version = paste0(
        'R package swaap (version ',
        installed.packages()['swaap', 'Version'],
        ')'
      )
    )
  )

  # If possible add source file names
  if ( any( chr_source_files != '' ) ) {

    lst_attr$swaap.source_file_details$files <- chr_source_files[
      chr_source_files != ''
    ]

    # Close 'If possible add source file names'
  }

  attributes(
    dtf_data$SSS.CHR.SourceFileDetails
  ) <- lst_attr

  return( dtf_data )
}

#### S) swaap_add.substances ####
#' Add Substance Use Variables
#'
#' Function to add substance use variables
#' along with quality control checks.
#'
#' @param dtf_data A data frame, assumed to
#'   follow the standardized format for the
#'   school-wide assessment data.
#' @param chr_substance A character string,
#'   the substance as originally labeled (either
#'   \code{'Alcohol'}, \code{'Cannabis'},
#'   \code{'Vapes'}, \code{'Cigarettes'},
#'   \code{'Cigars'}, \code{'Smokeless'}
#'
#' @author Kevin Potter
#'
#' @returns A data frame with additional
#' columns for lifetime use, integer
#' and character versions of the 7-point
#' past-month use rating, number of days
#' used, and past-year use. Note that
#' only a subset of grades and time points
#' would have been asked questions for
#' number of days used and past-year use.
#'
#' @export

swaap_add.substances <- function(
    dtf_data,
    chr_substance ) {

  chr_known <- c(
    'Alcohol',
    'Cannabis',
    'Vapes',
    'Cigarettes',
    'Cigars',
    'Smokeless'
  )

  # Check if input matches known substances
  lgc_match <- chr_substance %in% chr_known

  # If no match
  if ( !lgc_match ) {

    stop(
      paste0(
        "Argument 'chr_substance' should be ",
        paste(
          paste0( "'", chr_known, "'" ),
          collapse = ', '
        )
      )
    )

    # Close 'If no match'
  }

  chr_abbr <- switch(
    chr_substance,
    Alcohol = 'ALC',
    Cannabis = 'CNN',
    Vapes = 'VPS',
    Cigarettes = 'CIG',
    Cigars = 'CGR',
    Smokeless = 'SMK'
  )

  # Construct original variables
  chr_old <- c(
    LF = paste0(
      'INV.LGL.SUB.', chr_substance,
      '.Life'
    ),
    UD = paste0(
      'INV.INT.SUB.', chr_substance,
      '.Past30.UseDays'
    ),
    P3 = paste0(
      'INV.INT.SUB.', chr_substance,
      '.Past30'
    ),
    PY = paste0(
      'INV.LGL.SUB.', chr_substance,
      '.PastYear'
    )
  )

  # Check if variables exist
  lgc_exist <- chr_old %in% colnames(dtf_data)
  names(lgc_exist) <- names(chr_old)

  # Stop processing if no variables exist
  if ( all(!lgc_exist) ) {

    stop(
      'No variables for specified substance found'
    )

    # Close 'Stop processing if no variables exist'
  }

  # Define new variables
  chr_new <- c(
    # Lifetime use
    LUA = paste0(
      'SBS.LGC.', chr_abbr, '.Lifetime.Any'
    ),
    # 7-point rating for use [Integer]
    P31URI = paste0(
      'SBS.INT.', chr_abbr, '.Past31.UseRating'
    ),
    # 7-point rating for use [Character]
    P31URC = paste0(
      'SBS.CHR.', chr_abbr, '.Past31.UseRating'
    ),
    # Number of days used [Not all surveys]
    P31DU = paste0(
      'SBS.INT.', chr_abbr, '.Past31.DaysUsed'
    ),
    # Past-year use [Not all surveys]
    PYA = paste0(
      'SBS.LGC.', chr_abbr, '.PastYear.Any'
    ),
    # Which prompts were asked during survey
    PA = paste0(
      'SBS.CHR.', chr_abbr, '.PromptsAsked'
    ),
    # Notes on quality
    N = paste0(
      'SBS.CHR.', chr_abbr, '.Notes'
    ),
    # Indicator for subset with no issues
    NI = paste0(
      'SBS.LGC.', chr_abbr, '.NoIssues'
    )
  )

  chr_ratings <- c(
    '0 times', # 1
    'Only once', # 2
    'Less than once a week', # 3
    'On at least one day a week', # 4
    '2-3 days a week', # 5
    '4-6 days per week', # 6
    'Everyday' # 7
  )

  # If lifetime use exists
  if ( lgc_exist['LF'] ) {

    dtf_data[[ chr_new['LUA'] ]] <-
      dtf_data[[ chr_old['LF'] ]]

    # Close 'If lifetime use exists'
  }

  # If rating exists
  if ( lgc_exist['P3'] ) {

    # Rating value
    dtf_data[[ chr_new['P31URI'] ]] <-
      dtf_data[[ chr_old['P3'] ]] + 1

    # Rating label
    dtf_data[[ chr_new['P31URC'] ]] <-
      chr_ratings[ dtf_data[[ chr_old['P3'] ]] + 1 ]

    # Close 'If rating exists'
  }

  # If days used exists
  if ( lgc_exist['UD'] ) {

    # Number of days used
    dtf_data[[ chr_new['P31DU'] ]] <-
      dtf_data[[ chr_old['UD'] ]]

    # Close 'If days used exists'
  }

  # If past-year use exists
  if ( lgc_exist['PY'] ) {

    # Any past-year use
    dtf_data[[ chr_new['PYA'] ]] <-
      dtf_data[[ chr_old['PY'] ]]

    # Close 'If past-year use exists'
  }

  # Quality control checks
  dtf_data[[ chr_new['N'] ]] <- ''

  # Ratings and days used
  if ( lgc_exist[ c( 'P3', 'UD') ] |> all() ) {

    # Define useful logical vectors
    # + Ratings
    lgc_rating_NA <-
      is.na( dtf_data[[ chr_old['P3'] ]] )
    lgc_rating_0 <-
      dtf_data[[ chr_old['P3'] ]] %in% 0
    lgc_rating_04 <-
      dtf_data[[ chr_old['P3'] ]] %in% 0:4
    lgc_rating_56 <-
      dtf_data[[ chr_old['P3'] ]] %in% 5:6
    # + Days used
    lgc_use_NA <-
      is.na( dtf_data[[ chr_old['UD'] ]] )
    lgc_use_over_31 <-
      !lgc_use_NA &
      dtf_data[[ chr_old['UD'] ]] > 31
    lgc_use_under_0 <-
      !lgc_use_NA &
      dtf_data[[ chr_old['UD'] ]] < 0


    # Rating is missing
    dtf_data[[ chr_new['P31DU'] ]][
      lgc_rating_NA
    ] <- NA
    dtf_data[[ chr_new['N'] ]][
      lgc_rating_NA
    ] <- 'Excluded: Missing rating'

    # Flag special case of rating is missing but not use
    dtf_data[[ chr_new['N'] ]][
      lgc_rating_NA & !lgc_use_NA
    ] <- 'Excluded: Missing rating not days used'

    # Days used is missing
    dtf_data[[ chr_new['N'] ]][
      !lgc_rating_NA & lgc_use_NA
    ] <- 'Excluded: Missing days used'

    # Replace NA with 0 for days used if rating is 0
    dtf_data[[ chr_new['P31DU'] ]][
      lgc_rating_0 & lgc_use_NA
    ] <- 0
    dtf_data[[ chr_new['N'] ]][
      lgc_rating_0 & lgc_use_NA
    ] <- 'Flag: Set NA to 0 due to rating branching logic'

    # Cap inadmissible values
    # + If rating is 5 or 6 [use everyday] set to 31
    dtf_data[[ chr_new['P31DU'] ]][
      lgc_rating_56 & lgc_use_over_31
    ] <- 31
    dtf_data[[ chr_new['N'] ]][
      lgc_rating_56 & lgc_use_over_31
    ] <- 'Flag: Days used > 31'
    # + If rating is less than 5 set to NA
    dtf_data[[ chr_new['P31DU'] ]][
      lgc_rating_04 & lgc_use_over_31
    ] <- NA
    dtf_data[[ chr_new['N'] ]][
      lgc_rating_04 & lgc_use_over_31
    ] <- 'Excluded: Days used > 31 & inconsistent rating'

    lgc_special_case <-
      'INV.INT.SUB.Alcohol.AUDIT1' %in% colnames(dtf_data) &
      chr_substance %in% 'Alcohol'

    # If looking at alcohol and AUDIT is part of data set
    if (lgc_special_case) {

      lgc_cases <-
        dtf_data$INV.INT.SUB.Alcohol.AUDIT1 %in% 0 &
        lgc_rating_NA &
        lgc_use_NA

      dtf_data[[ chr_new['P31URI'] ]][lgc_cases] <- 1
      dtf_data[[ chr_new['P31URC'] ]][lgc_cases] <-
        chr_ratings[1]
      dtf_data[[ chr_new['P31DU'] ]][lgc_cases] <- 0
      dtf_data[[ chr_new['N'] ]][lgc_cases] <-
        'Flag: Set NA to 0 due to AUDIT branching logic'

      # Close 'If looking at alcohol and AUDIT is part of data set'
    }

    # Check consistency: ratings vs. days used
    lst_ratings_imply <- list(
      0, # None
      1, # Only once
      2:3, # Less than once a week
      4:7, # On at least one day a week
      8:13, # 2-3 days a week
      14:27, # 4-6 days per week
      28:31 # Everyday
    )

    # Loop over ratings
    for ( r in 1:7 ) {

      # Match rating
      lgc_rating <-
        dtf_data[[ chr_old['P3'] ]] %in% (r - 1)
      int_days_used <-
        dtf_data[[ chr_old['UD'] ]][lgc_rating]

      lgc_inconsistent <-
        !is.na( int_days_used ) &
        !( int_days_used %in% lst_ratings_imply[[r]] )

      # Flag inconsistent
      if ( any(lgc_inconsistent ) ) {

        dtf_data[[ chr_new['N'] ]][lgc_rating][lgc_inconsistent] <-
          'Flag: Rating and days used inconsistent'

        # Close 'Flag inconsistent'
      }

      # Close 'Loop over ratings'
    }

    # Close 'Ratings and days used'
  }

  # If past-year use exists
  if ( lgc_exist[ c( 'P3', 'PY' ) ] |> all() ) {

    # Check consistency: past year vs. past month
    lgc_inconsistent <-
      dtf_data[[ chr_old['P3'] ]] %in% 1:6 &
      dtf_data[[ chr_old['PY'] ]] %in% FALSE

    # If any inconsistent values
    if ( any(lgc_inconsistent) ) {

      dtf_data[[ chr_new['N'] ]][lgc_inconsistent] <-
        'Flag: Past month use inconsistent with no past year use'

      # Close 'If any inconsistent values'
    }

    # Close 'If past-year use exists'
  }

  return( dtf_data )
}

#### T) swaap_add.time_point ####
#' Add Variables for Time Points
#'
#' Function to add variables indicating the
#' time point, defined either by the
#' survey year and semester, or by grade
#' level. If by year and semester, clusters
#' by grades assuming standard progression
#' from one grade to next. If data consists
#' of the SBIRT sample only, uses the
#' pre-defined SBIRT time points and recruitment
#' wave.
#'
#' @param dtf_data A data frame, assumed to
#'   follow the standardized format for the
#'   school-wide assessment data.
#' @param lgc_grade A logical value; if
#'   \code{TRUE} defines time points using grade
#'   levels.
#'
#' @author Kevin Potter
#'
#' @returns A data frame with additional
#' columns \code{'SSS.INT.TimePoint'}
#' (starting from 0) and
#' \code{'SSS.INT.LongitudinalWave'}.
#'
#' @export

swaap_add.time_point <- function(
    dtf_data,
    lgc_grade = FALSE ) {

  # Check for SBIRT variables
  if ( 'SSS.LGC.SBIRT' %in% colnames(dtf_data) ) {

    # Only SBIRT sample
    if ( all( dtf_data$SSS.LGC.SBIRT) ) {

      # Uses SBIRT time points and wave
      dtf_data$SSS.INT.TimePoint <-
        dtf_data$SSS.INT.SBIRTTimePoint
      dtf_data$SSS.INT.LongitudinalWave <-
        dtf_data$SSS.INT.RecruitmentWave

      return( dtf_data )

      # Close 'Only SBIRT sample'
    }

    # Close 'Check for SBIRT variables'
  }

  dtf_grades <- dtf_data |>
    dplyr::group_by(
      SSS.INT.Grade
    ) |>
    dplyr::summarise(
      N = length( SSS.INT.Grade ),
      .groups = 'drop'
    ) |>
    data.frame()

  dtf_times <- dtf_data |>
    dplyr::group_by(
      SSS.CHR.SurveyYearSemester,
      SSS.INT.SurveyYear,
      SSS.CHR.Semester
    ) |>
    dplyr::summarise(
      N = length( SSS.INT.Grade ),
      .groups = 'drop'
    ) |>
    data.frame()
  dtf_times <- dtf_times |>
    dplyr::arrange(
      SSS.INT.SurveyYear,
      dplyr::desc(SSS.CHR.Semester)
    )

  # Initialize variables
  dtf_data$SSS.INT.TimePoint <- NA
  dtf_data$SSS.INT.LongitudinalWave <- 1

  # Define time by survey year and semester
  if ( !lgc_grade ) {

    # Loop over grades
    for ( g in 1:nrow(dtf_grades) ) {

      int_inc <-
        as.numeric( dtf_times$SSS.CHR.Semester == 'Fall' )

      # Loop over time points
      for ( p in 1:nrow(dtf_times) ) {

        int_grade_current <-
          (dtf_grades$SSS.INT.Grade[g] - 1) + cumsum(int_inc)[p]

        lgc_subset <-
          dtf_data$SSS.INT.Grade %in% int_grade_current &
          dtf_data$SSS.CHR.SurveyYearSemester %in%
          dtf_times$SSS.CHR.SurveyYearSemester[p]

        # If any rows exist
        if ( any(lgc_subset) ) {

          dtf_data$SSS.INT.TimePoint[lgc_subset] <- p - 1
          dtf_data$SSS.INT.LongitudinalWave[lgc_subset] <- g

          # Close 'If any rows exist'
        }

        # Close 'Loop over time points'
      }

      # Close 'Loop over grades'
    }

    # Algorithm misses lowest grade collected at last time point
    int_min_grade <- min( dtf_data$SSS.INT.Grade, na.rm = TRUE )
    lgc_update <-
      dtf_data$SSS.INT.Grade %in% int_min_grade &
      is.na( dtf_data$SSS.INT.TimePoint )
    dtf_data$SSS.INT.TimePoint[
      lgc_update
    ] <- max( dtf_data$SSS.INT.TimePoint, na.rm = TRUE )
    dtf_data$SSS.INT.LongitudinalWave[
      lgc_update
    ] <- max( dtf_data$SSS.INT.LongitudinalWave, na.rm = TRUE ) + 1

    # Close 'Define time by survey year and semester'
  } else {

    int_order <-
      dtf_data$SSS.INT.Grade |> unique() |> sort()

    # Loop over times
    for ( i in seq_along(chr_order) ) {

      lgc_subset <-
        dtf_data$SSS.INT.Grade %in% int_order[i]

      dtf_data$SSS.INT.TimePoint[lgc_subset] <- i - 1

      # Close 'Loop over times'
    }

    # Close else for 'Define time by survey year and semester'
  }

  return( dtf_data )
}

#### Y) swaap_add.year_and_semester ####
#' Add Variables for School Year and Semester
#'
#' Function to add variables indicating the
#' school year and semester for the
#' given school-wide assessment data.
#'
#' @param dtf_data A data frame, assumed to
#'   follow the standardized format for the
#'   school-wide assessment data.
#'
#' @author Kevin Potter
#'
#' @returns A data frame with additional
#' columns
#' \code{'SSS.CHR.DataSet'},
#' \code{'SSS.CHR.SurveyYearSemester'},
#' \code{'SSS.CHR.TimePoint'}, and
#' \code{'SSS.CHR.Semester'} indicating
#' the survey year and semester.
#'
#' @export

swaap_add.year_and_semester <- function(
    dtf_data ) {

  # Check if time points already exist
  if ( !is.null(dtf_data$SSS.INT.TimePoint) ) {

    int_times <-
      dtf_data$SSS.INT.TimePoint |>
      unique() |>
      sort()

    # Define separate subsets for each time point
    lst_subsets <- lapply(
      seq_along(int_times), function(p) {
        return(
          dtf_data$SSS.INT.TimePoint %in% int_times[p]
        )
      }
    )

    # Close 'Check if time points already exist'
  } else {

    # Use whole data set
    lst_subsets <- list(
      rep( TRUE, nrow(dtf_data) )
    )

    # Close else for 'Check if time points already exist'
  }

  # Initialize variables if they do not exist
  chr_columns <- colnames(dtf_data)

  if ( !'SSS.CHR.DataSet' %in% chr_columns )
    dtf_data$SSS.CHR.DataSet <- ''
  if ( !'SSS.CHR.Semester' %in% chr_columns )
    dtf_data$SSS.CHR.Semester <- ''
  if ( !'SSS.CHR.SurveyYearSemester' %in% chr_columns )
    dtf_data$SSS.CHR.SurveyYearSemester <- ''
  if ( !'SSS.CHR.TimePoint' %in% chr_columns )
    dtf_data$SSS.CHR.TimePoint <- ''

  #### Y.1) fun_update ####
  fun_update <- function(
    chr_values,
    chr_new ) {

    # Blank cases
    if ( all(chr_values == '') ) {

      chr_values <- rep( chr_new, length(chr_values) )

      # Close 'Blank cases'
    }

    return( chr_values )
  }

  # Loop over subsets
  for ( s in seq_along(lst_subsets) ) {

    int_year <-
      unique( dtf_data$SSS.INT.SurveyYear[ lst_subsets[[s]] ] )

    # Make sure unique year
    if ( length( int_year ) > 1 ) {

      stop( 'Multiple years detected' )

      # Close 'Make sure unique year'
    }

    # Survey month
    int_months <- sapply(
      dtf_data$SSS.DTM.SurveyStart[ lst_subsets[[s]] ],
      function(dtt_start) {
        return( lubridate::month(dtt_start))
      }
    )

    # Median month
    int_median <- median( int_months, na.rm = TRUE )

    # Define semester based on median month
    chr_semester <- 'Spring'
    if ( int_median %in% 9:12 ) chr_semester <- 'Fall'

    # Update variables
    dtf_data$SSS.CHR.DataSet[ lst_subsets[[s]] ] <-
      fun_update(
        dtf_data$SSS.CHR.DataSet[ lst_subsets[[s]] ],
        paste0( int_year, ' ', chr_semester )
      )
    dtf_data$SSS.CHR.SurveyYearSemester[ lst_subsets[[s]] ] <-
      fun_update(
        dtf_data$SSS.CHR.SurveyYearSemester[ lst_subsets[[s]] ],
        paste0( int_year, ' ', chr_semester )
      )
    dtf_data$SSS.CHR.TimePoint[ lst_subsets[[s]] ] <-
      fun_update(
        dtf_data$SSS.CHR.TimePoint[ lst_subsets[[s]] ],
        paste0( int_year, ' ', chr_semester )
      )
    dtf_data$SSS.CHR.Semester[ lst_subsets[[s]] ] <-
      fun_update(
        dtf_data$SSS.CHR.Semester[ lst_subsets[[s]] ],
        chr_semester
      )

    # Close 'Loop over subsets'
  }

  return( dtf_data )
}
