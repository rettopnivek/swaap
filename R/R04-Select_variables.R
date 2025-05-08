# Functions to recode variables
# Written by Kevin Potter
# email: kpotter5@mgh.harvard.edu
# Please email me directly if you
# have any questions or comments
# Last updated 2025-05-06

# Table of contents
# 1) swaap_select
# 2) swaap_select.merge
# 3) Specific subsets
#   3.B) swaap_select.base
#   3.C) swaap_select.contact_info
#   3.D) swaap_select.demographics
#   3.E) swaap_select.experience
#   3.I) swaap_select.inventories
#   3.L) swaap_select.linked
#   3.L) swaap_select.linking
#   3.Q) swaap_select.quality
#   3.S) swaap_select.SBIRT
#   3.S) swaap_select.substance_use

#### 1) swaap_select ####
#' Select Columns
#'
#' Function to select specified columns
#' in a data set.
#'
#' @param dtf_data A data frame, assumed to
#'   follow the standardized format for the
#'   school-wide assessment data.
#'
#' @returns A data frame.
#'
#' @export

swaap_select <- function(
    dtf_data,
    chr_columns,
    lgc_progress = TRUE ) {

  chr_columns <-
    chr_columns[ chr_columns %in% colnames(dtf_data) ]

  return( dtf_data[, chr_columns] )
}

#### 2) swaap_select.merge ####
# Internal Function to Merge Character Vectors
#
# Internal function that merges two character
# vectors.
#
# @param 'chr_input' A character vector.
# @param 'chr_add' A character vector.
#
# @returns A single character vector with
# empty strings removed.

swaap_select.merge <- function(
    chr_input,
    chr_add ) {

  if ( is.null(chr_input) ) chr_input <- ''

  chr_output <- c(
    chr_input,
    chr_add
  )
  chr_output <- chr_output[ chr_output != '' ]
  chr_output <- unique( chr_output )

  return( chr_output )
}

#### 3) Specific subsets ####

#### 3.B) swaap_select.base ####
#' Select Base Columns
#'
#' Function to select columns with basic
#' session information (e.g.,
#' survey year, grade level, etc.).
#'
#' @param chr_input An optional character
#'   vector, additional columns to include.
#' @param lgc_original A logical value; if
#'   \code{TRUE} uses original variable
#'   names instead of recoded ones.
#'
#' @returns A character vector.
#'
#' @export

swaap_select.base <- function(
    chr_input = NULL,
    lgc_original = FALSE ) {

  chr_add <- c(
    'SSS.CHR.DataSet',
    'SSS.INT.SurveyYear',
    'SSS.CHR.Semester',
    'SSS.CHR.SurveyYearSemester',
    'SSS.CHR.TimePoint',
    'SSS.INT.TimePoint',
    'SSS.INT.DistrictCode',
    'SSS.INT.SchoolCode',
    'SSS.INT.SchoolEnrollment',
    'SSS.DTT.SurveyStart',
    'SSS.DTT.SurveyEnd',
    'IDN.CHR.Record.ID',
    'IDN.INT.LAS.ID',
    'IDN.CHR.LocallyAssignedSchool.ID',
    'IDN.INT.LAS.ID',
    'SSS.INT.Grade'
  )

  # Original variable names
  if ( lgc_original ) {

    chr_add <- c(
      'SSS.INT.SurveyYear',
      'SSS.INT.District.Code',
      'SSS.INT.School.Code',
      paste0(
        'SSS.INT.',
        '.Grade.Enrollment'
      ),
      'SSS.DTM.SurveyStart',
      'SSS.DTM.SurveyEnd',
      'IDX.CHR.Origin.ID',
      'IDX.INT.Origin.LASID',
      'SSS.INT.Grade'
    )

    # Close 'Original variable names'
  }

  chr_output <- swaap_select.merge( chr_input, chr_add )

  return( chr_output )
}

#### 3.C) swaap_select.contact_info ####
#' Select Contact Information Items
#'
#' Function to select standard contact
#' information columns.
#'
#' @param chr_input An optional character
#'   vector, additional columns to include.
#'
#' @returns A character vector.
#'
#' @export

swaap_select.contact_info <- function(
    chr_input = NULL ) {

  chr_add <- c(
    'SBJ.CHR.Contact.Name',
    'SBJ.CHR.Contact.DateOfBirth',
    'SBJ.CHR.Contact.Cellphone',
    'SBJ.CHR.Contact.Email',
    'SBJ.CHR.Contact.GuardianName',
    'SBJ.CHR.Contact.GuardianRelation',
    'SBJ.CHR.Contact.GuardianPhone'
  )

  chr_output <- swaap_select.merge( chr_input, chr_add )

  return( chr_output )
}

#### 3.D) swaap_select.demographics ####
#' Select Demographic Measures
#'
#' Function to select standard demographic
#' measure columns.
#'
#' @param chr_input An optional character
#'   vector, additional columns to include.
#'
#' @returns A character vector.
#'
#' @export

swaap_select.demographics <- function(
    chr_input = NULL) {

  chr_add <- c(
    "SBJ.INT.AgeInYears",
    "SBJ.CHR.Sex",
    "SBJ.CHR.GenderIdentity",
    "SBJ.CHR.Sexuality",
    "SBJ.CHR.Race",
    "SBJ.CHR.Ethnicity"
  )

  chr_output <- swaap_select.merge( chr_input, chr_add )

  return( chr_output )
}

#### 3.E) swaap_select.experience ####
#' Select School Experience Items
#'
#' Function to select columns for school
#' experience items.
#'
#' @param chr_input An optional character
#'   vector, additional columns to include.
#'
#' @returns A character vector.
#'
#' @export

swaap_select.experience <- function(
    chr_input = NULL) {

  chr_add <- c(
    'SBJ.LGC.Experience.PlaySports',
    'SBJ.LGC.Experience.SuspensionsAny',
    'SBJ.LGC.Experience.SuspensionsDrug',
    'SBJ.CHR.Experience.GradesInSchool',
    'SBJ.INT.Experience.GradesInSchool'
  )

  chr_output <- swaap_select.merge( chr_input, chr_add )

  return( chr_output )
}

#### 3.I) swaap_select.inventories ####
#' Select Inventories
#'
#' Function to select columns for inventories.
#'
#' @param chr_input An optional character
#'   vector, additional columns to include.
#' @param chr_measures A character vector, the
#'   different measures to select, including
#'   \code{'AUDIT'}, \code{'PHQ4'}, \code{'ERS'},
#'   and \code{'ADDI'}.
#' @param lgc_items A logical value; if \code{TRUE}
#'   includes the individual items for each measure,
#'   otherwise includes total scores and subscale
#'   scores.
#'
#' @returns A character vector.
#'
#' @export

swaap_select.inventories <- function(
    chr_input = NULL,
    chr_measures = c(
      'AUDIT', 'PHQ4', 'ERS', 'ADDI'
    ),
    lgc_items = FALSE ) {

  chr_add <- c()

  chr_labels.AUDIT <- c(
    'Frequency',
    'Drinks',
    'Binge',
    'Stopping',
    'Failure',
    'Morning',
    'Guilt',
    'Memory',
    'Injured',
    'Concern'
  )

  chr_labels.ADDI <- c(
    'Class',
    'Disciplined',
    'Grade',
    'Club',
    'Activities',
    'More',
    'Less',
    'English',
    'Police',
    'Store',
    'Insulted',
    'Service',
    'Smart',
    'Afraid',
    'Threatened'
  )

  # Individual items
  if ( lgc_items ) {

    if ( 'PHQ4' %in% chr_measures )
      chr_add <- c(
        chr_add,
        'INV.INT.PHQ4.Q1.Anxious',
        'INV.INT.PHQ4.Q2.Worried',
        'INV.INT.PHQ4.Q3.Depressed',
        'INV.INT.PHQ4.Q4.Anhedonia'
      )

    if ( 'AUDIT' %in% chr_measures )
      chr_add <- c(
        chr_add,
        paste0( 'INV.INT.AUDIT.Q', 1:10, '.', chr_labels.AUDIT )
      )

    if ( 'ADDI' %in% chr_measures )
      chr_add <- c(
        chr_add,
        paste0( 'INV.INT.ADDI.Q', 1:15, '.', chr_labels.ADDI )
      )

    # Close 'Individual items'
  }

  if ( 'PHQ4' %in% chr_measures )
    chr_add <- c(
      chr_add,
      'INV.INT.PHQ4.Anxiety',
      'INV.INT.PHQ4.Depression',
      'INV.INT.PHQ4.Total'
    )

  if ( 'ERS' %in% chr_measures )
    chr_add <- c(
      chr_add,
      'INV.INT.ERS.Total'
    )

  if ( 'AUDIT' %in% chr_measures )
    chr_add <- c(
      chr_add,
      'INV.INT.AUDIT.Total'
    )

  if ( 'ADDI' %in% chr_measures )
    chr_add <- c(
      chr_add,
      'INV.INT.ADDI.Total'
    )

  if ( length(chr_add) == 0 )
    stop( "No columns selected - check 'chr_measures' argument" )

  chr_output <- swaap_select.merge( chr_input, chr_add )

  return( chr_output )
}

#### 3.L) swaap_select.linked ####
#' Select Details on Linkage
#'
#' Function to select columns detailing
#' linkage performance.
#'
#' @param chr_input An optional character
#'   vector, additional columns to include.
#' @param lgc_subset A logical value; if
#'   \code{TRUE} returns a subset of the
#'   columns, the most useful typical details.
#'
#' @returns A character vector.
#'
#' @export

swaap_select.linked <- function(
    chr_input = NULL,
    lgc_subset = TRUE ) {

  chr_add <- c(
    'IDN.CHR.Linked.ID',
    'LNK.LGC.Attempted',
    'LNK.CHR.Method',
    'LNK.CHR.Rows',
    'LNK.LGC.Duplicates',
    'LNK.LGC.NoIssues',
    'LNK.CHR.AttributeWithParameters',
    'LNK.CHR.TimePoints'
  )

  if (lgc_subset)
    chr_add <- c(
      'IDN.CHR.Linked.ID',
      'LNK.CHR.Rows',
      'LNK.LGC.Duplicates',
      'LNK.LGC.NoIssues',
      'LNK.CHR.TimePoints'
    )

  chr_output <- swaap_select.merge( chr_input, chr_add )

  return( chr_output )
}


#### 3.L) swaap_select.linking ####
#' Select Linking Items
#'
#' Function to select standard linking
#' item columns.
#'
#' @param chr_input An optional character
#'   vector, additional columns to include.
#' @param lgc_original A logical value; if
#'   \code{TRUE} uses original variable
#'   names instead of recoded ones.
#'
#' @returns A character vector.
#'
#' @export

swaap_select.linking <- function(
    chr_input = NULL,
    lgc_original = FALSE ) {

  chr_add <- c(
    'SBJ.INT.Link.SchoolCode',
    # Locally assigned school ID
    'SBJ.INT.Link.SchoolID',
    # Linking questions
    #   ranked from least to most discrepant
    'SBJ.INT.Link.KindergartenYearEst',
    'SBJ.CHR.Link.Sex',
    'SBJ.CHR.Link.BirthYearMonth',
    'SBJ.CHR.Link.MiddleInitial',
    'SBJ.CHR.Link.EyeColor',
    'SBJ.CHR.Link.OlderSiblings',
    'SBJ.CHR.Link.Streetname'
  )

  # Original variable names
  if (lgc_original) {

    chr_add <- c(
      'SSS.INT.School.Code',
      # Locally assigned school ID
      'IDX.INT.Origin.LASID',
      # Linking questions
      #   ranked from least to most discrepant
      'SBJ.INT.Link.KindergartenYearEst',
      'SBJ.FCT.Sex',
      'SBJ.DTM.Dob',
      'SBJ.FCT.Link.MiddleInitial',
      'SBJ.FCT.Link.EyeColor',
      'SBJ.FCT.Link.OlderSiblings',
      'SBJ.CHR.Link.Streetname'
    )

    # Close 'Original variable names'
  }

  chr_output <- swaap_select.merge( chr_input, chr_add )

  return( chr_output )
}

#### 3.Q) swaap_select.quality ####
#' Select Variables for Quality Control
#'
#' Function to select variables with
#' quality control details.
#'
#' @param chr_input An optional character
#'   vector, additional columns to include.
#'
#' @returns A character vector.
#'
#' @export

swaap_select.quality <- function(
    chr_input = NULL ) {

  chr_add <- c(
    'QLT.DBL.ProportionCompleted.Total',
    'QLT.LGC.Remove',
    'QLT.CHR.Remove'
  )

  chr_output <- swaap_select.merge( chr_input, chr_add )

  return( chr_output )
}

#### 3.S) swaap_select.SBIRT ####
#' Select Variables Denoting SBIRT Sample
#'
#' Function to select variables tracking
#' SBIRT sample details.
#'
#' @param chr_input An optional character
#'   vector, additional columns to include.
#'
#' @returns A character vector.
#'
#' @export

swaap_select.SBIRT <- function(
    chr_input = NULL ) {

  chr_add <- c(
    'SSS.LGC.SBIRT',
    'SSS.INT.SBIRTTimePoint',
    'SSS.INT.RecruitmentWave'
  )

  chr_output <- swaap_select.merge( chr_input, chr_add )

  return( chr_output )
}

#### 3.S) swaap_select.substance_use ####
#' Select Variables Denoting SBIRT Sample
#'
#' Function to select variables tracking
#' SBIRT sample details.
#'
#' @param chr_input An optional character
#'   vector, additional columns to include.
#'
#' @returns A character vector.
#'
#' @export

swaap_select.substance_use <- function(
    chr_input = NULL,
    lgc_SBIRT = FALSE ) {

  chr_abbr <- c(
    'ALC',
    'CNN',
    'VPS'
  )
  chr_add <- c()

  # Loop over substance abbreviations
  for ( s in seq_along( chr_abbr ) ) {

    chr_add <- c(
      chr_add,
      # Lifetime use
      paste0(
        'SBS.LGC.', chr_abbr, '.Lifetime.Any'
      ),
      # 7-point rating for use [Integer]
      paste0(
        'SBS.INT.', chr_abbr, '.Past31.UseRating'
      ),
      # 7-point rating for use [Character]
      paste0(
        'SBS.CHR.', chr_abbr, '.Past31.UseRating'
      )
    )

    # Close 'Loop over substance abbreviations'
  }

  # Additional variables for SBIRT sample
  if (lgc_SBIRT) {

    chr_add <- c(
      chr_add
    )

    # Close 'Additional variables for SBIRT sample'
  }

  chr_output <- swaap_select.merge( chr_input, chr_add )

  return( chr_output )
}


