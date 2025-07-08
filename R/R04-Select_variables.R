# Functions to select variables
# Written by Kevin Potter
# email: kpotter5@mgh.harvard.edu
# Please email me directly if you
# have any questions or comments
# Last updated 2025-07-08

# Table of contents
# 1) swaap_select
# 2) swaap_select.merge
# 3) Specific subsets
#   3.B) swaap_select.base
#   3.C) swaap_select.contact
#   3.D) swaap_select.demographics
#   3.E) swaap_select.experience
#   3.I) swaap_select.inventories
#   3.L) swaap_select.linked
#   3.L) swaap_select.linking
#   3.Q) swaap_select.quality
#   3.S) swaap_select.SBIRT
#   3.S) swaap_select.substances

#### 1) swaap_select ####
#' Select Columns
#'
#' Function to select specified columns
#' in a data set.
#'
#' @param dtf_data A data frame, assumed to
#'   follow the standardized format for the
#'   school-wide assessment data.
#' @param chr_columns A character vector, the
#'   columns in \code{dtf_data} to select
#'   (columns that do not exist are ignored).
#'
#' @author Kevin Potter
#'
#' @returns A data frame.
#'
#' @export

swaap_select <- function(
    dtf_data,
    chr_columns ) {

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
# @author Kevin Potter
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
#' @author Kevin Potter
#'
#' @returns A character vector.
#'
#' @export

swaap_select.base <- function(
    chr_input = NULL,
    lgc_original = FALSE ) {

  chr_add <- c(
    'SSS.CHR.SourceFileDetails',
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
    'IDN.CHR.LocallyAssignedSchool.ID',
    'IDN.CHR.LAS.ID',
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
      'IDX.INT.Origin.Record',
      'IDX.INT.Origin.Database',
      'IDX.INT.Origin.LASID',
      'SSS.INT.Grade'
    )

    # Close 'Original variable names'
  }

  chr_output <- swaap_select.merge( chr_input, chr_add )

  return( chr_output )
}

#### 3.C) swaap_select.contact ####
#' Select Contact Information Items
#'
#' Function to select standard contact
#' information columns.
#'
#' @param chr_input An optional character
#'   vector, additional columns to include.
#'
#' @author Kevin Potter
#'
#' @returns A character vector.
#'
#' @export

swaap_select.contact <- function(
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
#' @author Kevin Potter
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
#' @author Kevin Potter
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
#'   \code{'APSS'}, and \code{'ADDI'}.
#' @param lgc_items A logical value; if \code{TRUE}
#'   includes the individual items for each measure,
#'   otherwise includes total scores and subscale
#'   scores.
#'
#' @author Kevin Potter
#'
#' @returns A character vector.
#'
#' @export

swaap_select.inventories <- function(
    chr_input = NULL,
    chr_measures = c(
      'AUDIT', 'PHQ4', 'ERS', 'ADDI', 'APSS'
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

  chr_labels.ERS <- c(
    'P.UpsetLongTime', # 1
    'S.HurtEasily', # 2
    'I.FeelIntensely', # 3
    'I.PhysicallyUpset', # 4
    'S.EmotionalEasily', # 5
    'I.EmotionsStrongly', # 6
    'S.OftenAnxious', # 7
    'P.FeelOther', # 8
    'S.LittlestThings', # 9
    'P.DisagreementLong', # 10
    'P.LongerToCalmDown', # 11
    'S.AngryEasily', # 12
    'S.Bothered', # 13
    'S.EasilyAgitated', # 14
    'S.EmotionsInstant', # 15
    'S.ShortFuse', # 16
    'I.EmotionsTooIntense', # 17
    'S.SensitivePerson', # 18
    'I.MoodsPowerful', # 19
    'I.HardToThink', # 20
    'I.Overreacting' # 21
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

    if ( 'ERS' %in% chr_measures )
      chr_add <- c(
        chr_add,
        paste0( 'INV.INT.ERS.Q', 1:21, '.', chr_labels.ERS )
      )

    if ( 'AUDIT' %in% chr_measures )
      chr_add <- c(
        chr_add,
        paste0( 'INV.INT.AUDIT.Q', 1:10, '.', chr_labels.AUDIT )
      )

    if ( 'ADDI' %in% chr_measures )
      chr_add <- c(
        chr_add,
        paste0( 'INV.INT.ADDI.Q', 1:15, '.D.', chr_labels.ADDI ),
        paste0( 'INV.INT.ADDI.Q', 1:15, '.U.', chr_labels.ADDI )
      )

    if ( 'APSS' %in% chr_measures )
      chr_add <- c(
        chr_add,
        "INV.DBL.APSS.Q1.MindReading",
        "INV.DBL.APSS.Q2.TVRadio",
        "INV.DBL.APSS.Q3.Spying",
        "INV.DBL.APSS.Q4.Auditory",
        "INV.DBL.APSS.Q5.Controlled",
        "INV.DBL.APSS.Q6.Visual",
        "INV.DBL.APSS.Q7.Grandiosity"
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
      'INV.INT.ADDI.D.Total',
      'INV.INT.ADDI.U.Total'
    )

  if ( 'APSS' %in% chr_measures )
    chr_add <- c(
      chr_add,
      'INV.DBL.APSS.Total'
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
#' @author Kevin Potter
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
#' @param lgc_fastLink A logical value; if
#'   \code{TRUE} returns column names for
#'   variables specifically formatted to
#'   work with [fastLink::fastLink].
#'
#' @author Kevin Potter
#'
#' @returns A character vector.
#'
#' @export

swaap_select.linking <- function(
    chr_input = NULL,
    lgc_original = FALSE,
    lgc_fastLink = FALSE ) {

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

  # Original variable names
  if (lgc_fastLink) {

    chr_add <- c(
      chr_add[1],
      # Locally assigned school ID
      chr_add[2],
      # Linking questions
      #   ranked from least to most discrepant
      'SBJ.INT.Link.FL.Sex',
      'SBJ.CHR.Link.FL.BirthYearMonth',
      'SBJ.CHR.Link.FL.MiddleInitial',
      'SBJ.INT.Link.FL.EyeColor',
      'SBJ.INT.Link.FL.Siblings',
      'SBJ.CHR.Link.FL.SiblingBirthMonth',
      'SBJ.CHR.Link.FL.StreetName'
    )

    # Close 'Original variable names'
  }

  chr_output <- swaap_select.merge( chr_input, chr_add )

  return( chr_output )
}

#### 3.M) swaap_select.misc ####
#' Select Variables for Miscellaneous Items
#'
#' Function to select variables for
#' miscellaneous items.
#'
#' @param chr_input An optional character
#'   vector, additional columns to include.
#'
#' @author Kevin Potter
#'
#' @returns A character vector.
#'
#' @export
swaap_select.misc <- function(
    chr_input = NULL ) {

  chr_terms <- c(
    'None', # 0
    'ParentCaregiver', # 1
    'OtherFamily', # 2
    'FriendPartner', # 3
    'TeacherCoachAdmin', # 4
    'SchoolCounselor', # 5
    'OutsideCounselor', # 6
    'Pediatrician', # 7
    'ReligiousLeader', # 8
    'Helpline', # 9
    'SocialMediaSupport', # 10
    'EmergencyServices', # 11
    'RehabCenter', # 12
    'NotListed' # 13
  )

  chr_add <- c(
    'SBJ.CHR.PrescribedMedicationHealth',
    paste0(
      'SBJ.LGC.SoughtHelp.',
      chr_terms
    ),
    'SBJ.LGC.CloseConnection.Friend',
    'SBJ.LGC.CloseConnection.Parent',
    'SBJ.LGC.CloseConnection.Teacher'
  )

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
#' @author Kevin Potter
#'
#' @returns A character vector.
#'
#' @export

swaap_select.quality <- function(
    chr_input = NULL ) {

  chr_add <- c(
    'QLT.DBL.ProportionCompleted.Total',
    'QLT.LGC.AttentionChecks.MetAll',
    'QLT.LGC.AttentionChecks.MetAtLeastOne',
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

#### 3.S) swaap_select.substances ####
#' Select Variables Measuring Substance Use
#'
#' Function to select variables with self-reported
#' substance use details.
#'
#' @param chr_input An optional character
#'   vector, additional columns to include.
#'
#' @author Kevin Potter
#'
#' @returns A character vector.
#'
#' @export

swaap_select.substances <- function(
    chr_input = NULL,
    lgc_SBIRT = FALSE ) {

  chr_abbr <- c(
    'ALC',
    'CNN',
    'VPS',
    'CIG',
    'CGR',
    'SMK'
  )
  chr_add <- c()

  # Loop over substance abbreviations
  for ( s in seq_along( chr_abbr ) ) {

    chr_add <- c(
      chr_add,
      # Lifetime use
      paste0(
        'SBS.LGC.', chr_abbr[s], '.Lifetime.Any'
      ),
      # 7-point rating for use [Integer]
      paste0(
        'SBS.INT.', chr_abbr[s], '.Past31.UseRating'
      ),
      # 7-point rating for use [Character]
      paste0(
        'SBS.CHR.', chr_abbr[s], '.Past31.UseRating'
      )
    )

    # Add binge drinking for alcohol
    if ( 'ALC' %in% chr_abbr[s] ) {

      chr_add <- c(
        chr_add,
        'SBS.INT.ALC.Past31.BingeRating',
        'SBS.CHR.ALC.Past31.BingeRating'
      )

      # Close 'Add binge drinking for alcohol'
    }

    # Additional variables for SBIRT sample
    if (lgc_SBIRT) {

      # Alcohol/Cannabis/Vapes
      if ( chr_abbr[s] %in% chr_abbr[1:3] ) {

        chr_add <- c(
          chr_add,
          # Number of days used [Not all surveys]
          paste0(
            'SBS.INT.', chr_abbr[s], '.Past31.DaysUsed'
          ),
          # Past-year use [Not all surveys]
          paste0(
            'SBS.LGC.', chr_abbr[s], '.PastYear.Any'
          ),
          # Which prompts were asked during survey
          paste0(
            'SBS.CHR.', chr_abbr[s], '.PromptsAsked'
          ),
          # Notes on quality
          paste0(
            'SBS.CHR.', chr_abbr[s], '.Notes'
          ),
          # Indicator for subset with no issues
          paste0(
            'SBS.LGC.', chr_abbr[s], '.NoIssues'
          )
        )

        # Close 'Alcohol/Cannabis/Vapes'
      }

      # Close 'Additional variables for SBIRT sample'
    }

    # Close 'Loop over substance abbreviations'
  }

  chr_output <- swaap_select.merge( chr_input, chr_add )

  return( chr_output )
}

#### 3.S) swaap_select.suicidality ####
#' Select Variables on Suicidality
#'
#' Function to select variables on suicidality.
#'
#' @param chr_input An optional character
#'   vector, additional columns to include.
#'
#' @author Kevin Potter
#'
#' @returns A character vector.
#'
#' @export

swaap_select.suicidality <- function(
    chr_input = NULL ) {

  chr_add <- c(
    'INV.INT.SI.Thoughts',
    'INV.INT.SI.How',
    'INV.INT.SI.Attempt',
    'INV.INT.SI.Selfharm',
    'INV.INT.SI.Total'
  )

  chr_output <- swaap_select.merge( chr_input, chr_add )

  return( chr_output )
}

#### 4) %s% ####
#' Operator for Selecting Columns
#'
#' Operator for calling [swaap::swaap_select]
#' and specified column subsets.
#'
#' @param dtf_data A data frame.
#' @param chr_select A character vector, the
#'   specific variants of \code{swaap_select}
#'   to call. Options currently include:
#'   \code{'base'}, \code{'contact'},
#'   \code{'demographics'}, \code{'experience'},
#'   \code{'inventories'}, \code{'linked'},
#'   \code{'linking'}, \code{'quality'},
#'   \code{'SBIRT'}, and \code{'substances'}.
#'
#' @returns A data frame with the subset of
#' specified columns.
#'
#' @export

`%s%` <- function(
    dtf_data,
    chr_select ) {

  chr_columns <- ''

  chr_lgc <- chr_select[
    grepl( 'lgc', chr_select, fixed = TRUE )
  ]
  chr_select <- chr_select[
    !grepl( 'lgc', chr_select, fixed = TRUE )
  ]

  # Loop over calls
  for ( i in seq_along(chr_select) ) {

    lst_args <- list(
      chr_input = chr_columns
    )

    # Extra arguments for swaap_select.substances
    if ( chr_select[i] == 'substances' ) {

      if ( 'lgc_SBIRT' %in% chr_lgc )
        lst_args$lgc_SBIRT <- TRUE

      # Close 'Extra arguments for swaap_select.substances'
    }

    # Extra arguments for swaap_select.linking
    if ( chr_select[i] == 'linking' ) {

      if ( 'lgc_original' %in% chr_lgc )
        lst_args$lgc_original <- TRUE

      if ( 'lgc_fastLink' %in% chr_lgc )
        lst_args$lgc_fastLink <- TRUE

      # Close 'Extra arguments for swaap_select.linking'
    }

    # Extra arguments for swaap_select.linked
    if ( chr_select[i] == 'linked' ) {

      if ( 'lgc_subset' %in% chr_lgc )
        lst_args$lgc_subset <- TRUE

      # Close 'Extra arguments for swaap_select.linked'
    }

    # Extra arguments for swaap_select.base
    if ( chr_select[i] == 'base' ) {

      if ( 'lgc_original' %in% chr_lgc )
        lst_args$lgc_original <- TRUE

      # Close 'Extra arguments for swaap_select.base'
    }

    # Extra arguments for swaap_select.inventories
    if ( chr_select[i] == 'inventories' ) {

      if ( 'lgc_items' %in% chr_lgc )
        lst_args$lgc_items <- TRUE

      # Close 'Extra arguments for swaap_select.inventories'
    }

    chr_columns <- do.call(
      eval(
        parse(
          text = paste0( 'swaap::swaap_select.', chr_select[i] )
        )
      ),
      args = lst_args
    )

    # Close 'Loop over calls'
  }

  return(
    swaap::swaap_select(dtf_data, chr_columns)
  )
}



