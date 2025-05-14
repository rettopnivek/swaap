# Functions to recode variables
# Written by Kevin Potter
# email: kpotter5@mgh.harvard.edu
# Please email me directly if you
# have any questions or comments
# Last updated 2025-05-12

# Table of contents
# B) swaap_recode.base
# C) swaap_recode.contact
# D) swaap_recode.demographics
# E) swaap_recode.experience
# I) swaap_recode.inventories
#   I.1) AUDIT
#   1.2) ADDI
# L) swaap_recode.linking
# Q) swaap_recode.quality

#### B) swaap_recode.base ####
#' Recode Base Information Variables
#'
#' Renames variables with base information
#' (school codes, record identifiers, etc.).
#'
#' @param dtf_data A data frame, assumed to
#'   follow the standardized format for the
#'   school-wide assessment data.
#'
#' @author Kevin Potter
#'
#' @returns A data frame with the additional variables
#' \code{'SSS.INT.DistrictCode'},
#' \code{'SSS.INT.SchoolCode'},
#' \code{'IDN.CHR.Record.ID'},
#' \code{'IDN.CHR.LocallyAssignedSchool.ID'}, and
#' \code{'IDX.INT.Origin.LASID'}.
#'
#' @export

swaap_recode.base <- function(
    dtf_data ) {

  chr_columns <- colnames(dtf_data)

  if ( 'SSS.INT.District.Code' %in% chr_columns )
    dtf_data$SSS.INT.DistrictCode <-
      dtf_data$SSS.INT.District.Code

  if ( 'SSS.INT.School.Code' %in% chr_columns )
    dtf_data$SSS.INT.SchoolCode <-
      dtf_data$SSS.INT.School.Code

  if ( 'IDX.CHR.Origin.ID' %in% chr_columns )
    dtf_data$IDN.CHR.Record.ID <-
      dtf_data$IDX.CHR.Origin.ID

  # Copy multiple versions
  if ( 'IDX.INT.Origin.LASID' %in% chr_columns ) {

    dtf_data$IDN.CHR.LocallyAssignedSchool.ID <-
      dtf_data$IDX.INT.Origin.LASID
    dtf_data$IDN.CHR.LAS.ID <-
      dtf_data$IDX.INT.Origin.LASID

    # Close 'Copy multiple versions'
  }

  if ( 'SSS.DTM.SurveyStart' %in% chr_columns )
    dtf_data$SSS.DTT.SurveyStart <-
      dtf_data$SSS.DTM.SurveyStart

  if ( 'SSS.DTM.SurveyEnd' %in% chr_columns )
    dtf_data$SSS.DTT.SurveyEnd <-
      dtf_data$SSS.DTM.SurveyEnd

  return( dtf_data )
}

#### C) swaap_recode.contact ####
#' Recode Contact Information Items
#'
#' Renames contact information items for easy selection.
#'
#' @param dtf_data A data frame, assumed to
#'   follow the standardized format for the
#'   school-wide assessment data.
#'
#' @author Kevin Potter
#'
#' @returns A data frame with the additional variables
#' \code{'SBJ.CHR.Contact.DateOfBirth'},
#' \code{'SBJ.CHR.Contact.GuardianName'},
#' \code{'SBJ.CHR.Contact.GuardianRelation'}, and
#' \code{'SBJ.CHR.Contact.GuardianPhone'}.
#'
#' @export

swaap_recode.contact <- function(
    dtf_data ) {

  chr_columns <- colnames(dtf_data)

  if ( 'SBJ.DAT.Contact.DOB' %in% chr_columns )
    dtf_data$SBJ.CHR.Contact.DateOfBirth <-
      dtf_data$SBJ.DAT.Contact.DOB

  if ( 'SBJ.CHR.Contact.Guardian.Name' %in% chr_columns )
    dtf_data$SBJ.CHR.Contact.GuardianName <-
      dtf_data$SBJ.CHR.Contact.Guardian.Name

  if ( 'SBJ.CHR.Contact.Guardian.Relation' %in% chr_columns )
    dtf_data$SBJ.CHR.Contact.GuardianRelation <-
      dtf_data$SBJ.CHR.Contact.Guardian.Relation

  if ( 'SBJ.CHR.Contact.Guardian.Phone' %in% chr_columns )
    dtf_data$SBJ.CHR.Contact.GuardianPhone <-
      dtf_data$SBJ.CHR.Contact.Guardian.Phone

  return( dtf_data )
}

#### D) swaap_recode.demographics ####
#' Recode Standard Demographic Variables
#'
#' Renames standard demographic variables
#' (age, biological sex, etc.). Prioritizes
#' revised versions ending with \code{.R}.
#'
#' @param dtf_data A data frame, assumed to
#'   follow the standardized format for the
#'   school-wide assessment data.
#'
#' @author Kevin Potter
#'
#' @returns A data frame with the additional variables
#' \code{'SBJ.INT.AgeInYears'},
#' \code{'SBJ.CHR.Sex'},
#' \code{'SBJ.CHR.GenderIdentity'},
#' \code{'SBJ.CHR.Sexuality'},
#' \code{'SBJ.CHR.Race'}, and
#' \code{'SBJ.CHR.Ethnicity'}.
#'
#' @export

swaap_recode.demographics <- function(
    dtf_data ) {

  # Check for revised age
  if ( 'SBJ.INT.Age.R' %in% colnames(dtf_data) ) {

    dtf_data$SBJ.INT.AgeInYears <-
      dtf_data$SBJ.INT.Age.R

    # Close 'Check for revised age'
  } else {

    if ( 'SBJ.INT.Age' %in% colnames(dtf_data) )
      dtf_data$SBJ.INT.AgeInYears <- dtf_data$SBJ.INT.Age

    # Close else for 'Check for revised age'
  }

  # Additional cleaning
  if ( 'SBJ.INT.AgeInYears' %in% colnames(dtf_data) ) {

    # Set values below 0 to NA
    dtf_data$SBJ.INT.AgeInYears[
      !is.na( dtf_data$SBJ.INT.AgeInYears ) &
      dtf_data$SBJ.INT.AgeInYears < 0
    ] <- NA
    # Set values over 25 to NA
    dtf_data$SBJ.INT.AgeInYears[
      !is.na( dtf_data$SBJ.INT.AgeInYears ) &
        dtf_data$SBJ.INT.AgeInYears > 25
    ] <- NA

    # Close 'Additional cleaning'
  }

  if ( 'SBJ.FCT.Sex' %in% colnames(dtf_data) )
    dtf_data$SBJ.CHR.Sex <-
      dtf_data$SBJ.FCT.Sex

  # Check for revised gender identity
  if ( 'SBJ.FCT.GenderId.R' %in% colnames(dtf_data) ) {

    dtf_data$SBJ.CHR.GenderIdentity <-
      dtf_data$SBJ.FCT.GenderId.R

    # Close 'Check for revised gender identity'
  } else {

    if ( 'SBJ.FCT.GenderId' %in% colnames(dtf_data) )
      dtf_data$SBJ.CHR.GenderIdentity <-
        dtf_data$SBJ.FCT.GenderId

    # Close else for 'Check for revised gender identity'
  }

  # Check for revised sexuality
  if ( 'SBJ.FCT.Sexuality.R' %in% colnames(dtf_data) ) {

    dtf_data$SBJ.CHR.Sexuality <-
      dtf_data$SBJ.FCT.Sexuality.R

    # Close 'Check for revised sexuality'
  } else {

    if ( 'SBJ.FCT.Sexuality' %in% colnames(dtf_data) )
      dtf_data$SBJ.CHR.Sexuality <-
        dtf_data$SBJ.FCT.Sexuality

    # Close else for 'Check for revised sexuality'
  }

  # Check for revised race
  if ( 'SBJ.FCT.Race.R' %in% colnames(dtf_data) ) {

    dtf_data$SBJ.CHR.Race <-
      dtf_data$SBJ.FCT.Race.R

    # Close 'Check for revised race'
  } else {

    if ( 'SBJ.FCT.Race' %in% colnames(dtf_data) )
      dtf_data$SBJ.CHR.Race <-
        dtf_data$SBJ.FCT.Race

    # Close else for 'Check for revised race'
  }

  if ( 'SBJ.FCT.Ethnicity' %in% colnames(dtf_data) )
    dtf_data$SBJ.CHR.Ethnicity <-
      dtf_data$SBJ.FCT.Ethnicity

  return( dtf_data )
}

#### E) swaap_recode.experience ####
#' Recode School Experience Items
#'
#' Recodes school experience items for easy selection.
#'
#' @param dtf_data A data frame, assumed to
#'   follow the standardized format for the
#'   school-wide assessment data.
#'
#' @author Kevin Potter
#'
#' @returns A data frame with the additional variables
#' \code{'SBJ.LGC.Experience.PlaySports'},
#' \code{'SBJ.LGC.Experience.SuspensionsAny'},
#' \code{'SBJ.LGC.Experience.SuspensionsDrug'},
#' \code{'SBJ.CHR.Experience.GradesInSchool'}, and
#' \code{'SBJ.INT.Experience.GradesInSchool'}.
#'
#' @export

swaap_recode.experience <- function(
    dtf_data ) {

  chr_columns <- colnames(dtf_data)

  # Recode item for playing on sports team
  if ( 'INV.INT.SchoolXP.Sports' %in% chr_columns ) {

    dtf_data$SBJ.LGC.Experience.PlaySports <-
      c( FALSE, TRUE )[ dtf_data$INV.INT.SchoolXP.Sports + 1 ]

    # Close 'Recode item for playing on sports team'
  }

  # Recode item for suspensions
  if ( 'INV.INT.SchoolXP.Suspension' %in% chr_columns ) {

    dtf_data$SBJ.LGC.Experience.SuspensionsAny <-
      c( FALSE, TRUE )[ dtf_data$INV.INT.SchoolXP.Suspension + 1 ]

    # Close 'Recode item for suspensions'
  }

  # Recode item for suspensions [Drug]
  if ( 'INV.INT.SchoolXP.Suspension.Drug' %in% chr_columns ) {

    dtf_data$SBJ.LGC.Experience.SuspensionsDrug <-
      c( FALSE, TRUE )[ dtf_data$INV.INT.SchoolXP.Suspension.Drug + 1 ]

    # Close 'Recode item for suspensions [Drug]'
  }

  # Recode item for class performance
  if ( 'INV.INT.SchoolXP.ClassPerformance' %in% chr_columns ) {

    dtf_data$SBJ.CHR.Experience.GradesInSchool <-
      c(
        'Mostly A', # 1
        'Mostly B', # 2
        'Mostly C', # 3
        'Mostly D', # 4
        'Mostly F', # 5
        'Mixed A and B to C and D', # 6
        'None of these grades', # 7
        'Not sure' # 8
        )[ dtf_data$INV.INT.SchoolXP.ClassPerformance ]
    dtf_data$SBJ.INT.Experience.GradesInSchool <-
      dtf_data$INV.INT.SchoolXP.ClassPerformance

    # Close 'Recode item for class performance'
  }

  # Recode item for class performace [Sic]
  if ( 'INV.INT.SchoolXP.ClassPerformace' %in% chr_columns ) {

    dtf_data$SBJ.CHR.Experience.GradesInSchool <-
      c(
        'Mostly A', # 1
        'Mostly B', # 2
        'Mostly C', # 3
        'Mostly D', # 4
        'Mostly F', # 5
        'Mixed A and B to C and D', # 6
        'None of these grades', # 7
        'Not sure' # 8
      )[ dtf_data$INV.INT.SchoolXP.ClassPerformace ]
    dtf_data$SBJ.INT.Experience.GradesInSchool <-
      dtf_data$INV.INT.SchoolXP.ClassPerformace

    # Close 'Recode item for class performance'
  }

  return( dtf_data )
}

#### I) swaap_recode.inventories ####
#' Recode Inventory Variables
#'
#' Renames inventory variables for easy selection.
#'
#' @param dtf_data A data frame, assumed to
#'   follow the standardized format for the
#'   school-wide assessment data.
#'
#' @author Kevin Potter
#'
#' @returns A data frame with the additional variables
#' \code{'INV.INT.AUDIT.Total'},
#' \code{'INV.INT.AUDIT.Q1'} to
#' \code{'INV.INT.AUDIT.Q10'},
#' \code{'INV.INT.ADDI.Total'},
#' \code{'INV.INT.ADDI.Q1'} to
#' \code{'INV.INT.ADDI.Q15'}.
#'
#' @export

swaap_recode.inventories <- function(
    dtf_data ) {

  chr_columns <- colnames(dtf_data)

  #### I.1) AUDIT ####

  if ( 'INV.INT.SUB.Alcohol.AUDIT.Total' %in% chr_columns )
    dtf_data$INV.INT.AUDIT.Total <-
    dtf_data$INV.INT.SUB.Alcohol.AUDIT.Total

  chr_items <- paste0(
    'INV.INT.SUB.Alcohol.AUDIT',
    1:10
  )
  chr_labels <- c(
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

  # Loop over items
  for ( i in seq_along(chr_items) ) {

    if ( chr_items[i] %in% chr_columns )
      dtf_data[[
        paste0( 'INV.INT.AUDIT.Q', i, '.', chr_labels[i] )
      ]] <-
      dtf_data[[ chr_items[i] ]]

    # Close 'Loop over items'
  }

  #### 1.2) ADDI ####

  chr_items <- paste0(
    'INV.LGL.ADDI.Q',
    1:15
  )
  chr_labels <- c(
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

  # Loop over items
  for ( i in seq_along(chr_items) ) {

    if ( chr_items[i] %in% chr_columns )
      dtf_data[[
        paste0( 'INV.INT.ADDI.Q', i, '.', chr_labels[i] )
      ]] <-
        as.numeric( dtf_data[[ chr_items[i] ]] )

    # Close 'Loop over items'
  }

  chr_items_new <-
    paste0( 'INV.INT.ADDI.Q', 1:15, '.', chr_labels )

  if ( all(chr_items_new %in% colnames(dtf_data)) )
    dtf_data$INV.INT.ADDI.Total <- rowSums(
      dtf_data[, chr_items_new],
      na.rm = TRUE
    )

  return( dtf_data )
}

#### L) swaap_recode.linking ####
#' Recode Linking Items
#'
#' Renames linking items for easy selection.
#'
#' @param dtf_data A data frame, assumed to
#'   follow the standardized format for the
#'   school-wide assessment data.
#'
#' @author Kevin Potter
#'
#' @returns A data frame with the additional variables
#' \code{'SBJ.INT.Link.SchoolCode'},
#' \code{'SBJ.INT.Link.SchoolID'},
#' \code{'SBJ.CHR.Link.Sex'},
#' \code{'SBJ.CHR.Link.BirthYearMonth'},
#' \code{'SBJ.CHR.Link.OlderSiblings'},
#' \code{'SBJ.CHR.Link.EyeColor'}, and
#' \code{'SBJ.CHR.Link.MiddleInitial'}.
#'
#' @export

swaap_recode.linking <- function(
    dtf_data ) {

  chr_columns <- colnames(dtf_data)

  if ( 'SSS.INT.School.Code' %in% chr_columns )
    dtf_data$SBJ.INT.Link.SchoolCode <-
    dtf_data$SSS.INT.School.Code

  if ( 'IDX.INT.Origin.LASID' %in% chr_columns )
    dtf_data$SBJ.INT.Link.SchoolID <-
    dtf_data$IDX.INT.Origin.LASID

  if ( 'SBJ.FCT.Sex' %in% chr_columns )
    dtf_data$SBJ.CHR.Link.Sex <-
    dtf_data$SBJ.FCT.Sex

  chr_year <- c(
    'SBJ.INT.BirthYear.R',
    'SBJ.INT.BirthYear'
  )
  chr_month <- c(
    'SBJ.INT.BirthMonth',
    'SBJ.INT.BirthMonth.R'
  )

  # Birth year and month
  if ( any(chr_year %in% chr_columns) &
       any(chr_month %in% chr_columns) ) {

    chr_year <- chr_year[
      chr_year %in% chr_columns
    ][1]
    chr_month <- chr_month[
      chr_month %in% chr_columns
    ][1]

    dtf_data$SBJ.CHR.Link.BirthYearMonth <-
      paste0(
        dtf_data[[ chr_year ]],
        '-',
        dtf_data[[ chr_month ]]
      )

    # Check for inadmissable values
    lgc_NA <-
      !( dtf_data[[ chr_month ]] %in% 1:12 ) |
      !( dtf_data[[ chr_year ]] %in% 2000:2016 )

    dtf_data$SBJ.CHR.Link.BirthYearMonth[lgc_NA] <- NA

    # Close 'Birth year and month'
  } else {

    # Restrict to birth year and month
    if ( 'SBJ.DTM.Dob' %in% chr_columns ) {

      dtf_data$SBJ.CHR.Link.BirthYearMonth <-
        substr(
          as.character( dtf_data$SBJ.DTM.Dob ),
          start = 1,
          stop = 7
        )

      chr_check <- c(
        'SBJ.INT.AgeInYears',
        'SBJ.INT.Age.R',
        'SBJ.INT.Age'
      )

      # If possible remove strange dates
      if ( any( chr_check %in% chr_columns ) ) {

        chr_check <- chr_check[
          chr_check %in% chr_columns
        ][1]

        lgc_NA <- is.na(
          dtf_data[[ chr_check ]]
        )

        dtf_data$SBJ.CHR.Link.BirthYearMonth[lgc_NA] <- NA

        # Close 'If possible remove strange dates'
      }

      # Close 'Restrict to birth year and month'
    }

    # Close else for 'Birth year and month'
  }

  if ( 'SBJ.FCT.Link.OlderSiblings' %in% chr_columns )
    dtf_data$SBJ.CHR.Link.OlderSiblings <-
    dtf_data$SBJ.FCT.Link.OlderSiblings

  if ( 'SBJ.FCT.Link.EyeColor' %in% chr_columns )
    dtf_data$SBJ.CHR.Link.EyeColor <-
    dtf_data$SBJ.FCT.Link.EyeColor

  if ( 'SBJ.FCT.Link.MiddleInitial' %in% chr_columns )
    dtf_data$SBJ.CHR.Link.MiddleInitial <-
    dtf_data$SBJ.FCT.Link.MiddleInitial

  return( dtf_data )
}

#### Q) swaap_recode.quality ####
#' Recode Quality Control Items
#'
#' Renames quality control items for easy selection.
#'
#' @param dtf_data A data frame, assumed to
#'   follow the standardized format for the
#'   school-wide assessment data.
#'
#' @author Kevin Potter
#'
#' @returns A data frame with the additional variables
#' \code{'QLT.DBL.ProportionCompleted.Total'},
#' \code{'QLT.LGC.AttentionChecks.MetAll'}. and
#' \code{'QLT.LGC.AttentionChecks.MetAtLeastOne'}.
#'
#' @export

swaap_recode.quality <- function(
    dtf_data ) {

  chr_columns <- colnames(dtf_data)

  if ( 'QCC.DBL.Completion.CrossSection' %in% chr_columns )
    dtf_data$QLT.DBL.ProportionCompleted.Total <-
      dtf_data$QCC.DBL.Completion.CrossSection

  if ( 'QCC.LGL.AttnChecks.OnlyCorrectResponses' %in% chr_columns )
    dtf_data$QLT.LGC.AttentionChecks.MetAll <-
      dtf_data$QCC.LGL.AttnChecks.OnlyCorrectResponses

  if ( 'QCC.LGL.AttnChecks.AtleastOneCorrect' %in% chr_columns )
    dtf_data$QLT.LGC.AttentionChecks.MetAtLeastOne <-
      dtf_data$QCC.LGL.AttnChecks.AtleastOneCorrect

  return( dtf_data )
}

