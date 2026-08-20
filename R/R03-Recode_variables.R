# Functions to recode variables
# Written by...
#   Kevin Potter
# Maintained by...
#   Kevin Potter
# Email:
#   kpotter5@mgh.harvard.edu
# Please email me directly if you
# have any questions or comments
# Last updated: 2026-08-03

# Table of contents
# B) swaap_recode.base
# C) swaap_recode.character
# C) swaap_recode.contact
# D) swaap_recode.demographics
# D) swaap_recode.discrimination
# E) swaap_recode.experience
# I) swaap_recode.intermittent
#   I.1) Close connections [2024]
#   I.2) Language [2023-2024]
#   I.3) Sleep [2024]
#   I.4) Climate change [2024]
#   I.5) Social media [2024]
# I) swaap_recode.inventories
#   I.1) ADDI
#   I.2) APSS
#   I.3) AUDIT
#   I.4) ERS
#   I.5) PHQ-4
# L) swaap_recode.linking
# M) swaap_recode.misc
#   M.1) Prescribed medication [2022+]
#   M.2) Help-seeking [2020+]
#   M.3) Connect with school services [2024+]
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

  lgc_found <- all(
    c( 'IDX.INT.Origin.Record', 'IDX.INT.Origin.Database' ) %in% chr_columns
  )

  if ( lgc_found )
    dtf_data$IDN.CHR.Record.ID <- paste0(
      dtf_data$IDX.INT.Origin.Database,
      '-',
      dtf_data$IDX.INT.Origin.Record
    )

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

#### C) swaap_recode.character ####
#' Recode Character Value Columns to Integer
#'
#' Converts columns of character strings to
#' integers for compatibility with other
#' statistical software programs. Also
#' converts logical variables (\code{TRUE/FALSE})
#' to (\code{1/0}) instead.
#'
#' @param dtf_data A data frame, assumed to
#'   follow the standardized format for the
#'   school-wide assessment data.
#' @param lgc_mapping A logical value; if
#'   \code{TRUE} instead returns a data
#'   frame with the mapping between
#'   character and numeric values for
#'   each changed variable.
#'
#' @author Kevin Potter
#'
#' @returns A data frame with variables with
#' the data type \code{CHR} (character strings)
#' renamed to \code{INT} (integer values), or
#' a data frame reporting on the mapping from
#' original character strings to numeric values.
#'
#' @export

swaap_recode.character <- function(
    dtf_data,
    lgc_mapping = FALSE ) {

  # Initialize variables
  chr_columns <- colnames(dtf_data)
  chr_remove <- c()
  dtf_mapping <- c()

  fun_unique <- function(x) {

    vec_unq <- unique(x)
    vec_unq <- vec_unq[ !is.na(vec_unq) ]

    return( vec_unq[1] )
  }

  # Loop over columns
  for ( k in seq_along(chr_columns) ) {

    lgc_chr <- substr(chr_columns[k], 5, 7 ) %in% 'CHR'

    # Check if character string
    if (lgc_chr) {

      # Check if integer version of variable already exists
      chr_int <- strsplit(
        chr_columns[k], split = '.CHR.', fixed = TRUE
      ) |> unlist() |> paste( collapse = '.INT.' )

      # Update columns to remove
      chr_remove <- c( chr_remove, chr_columns[k] )

      # Create integer version
      if ( !chr_int %in% chr_columns ) {

        # Copy column
        chr_values <- dtf_data[[ chr_columns[k] ]]

        dtf_data[[ chr_int ]] <- as.numeric(
          as.factor( chr_values )
        )

        # Close 'Create integer version'
      }

      # If few enough distinct values
      if ( dplyr::n_distinct(dtf_data[[ chr_columns[k] ]]) <= 70 &
           lgc_mapping ) {

        dtf_mapping_cur <- dtf_data |>
          dplyr::group_by_at(
            chr_columns[k]
          ) |>
          dplyr::summarise_at(
            chr_int, fun_unique
          ) |>
          data.frame()
        colnames(dtf_mapping_cur) <- c(
          'Labels', 'Values'
        )
        dtf_mapping_cur$Variable <- chr_int

        dtf_mapping <- rbind(
          dtf_mapping,
          dtf_mapping_cur
        )

        # Close 'If few enough distinct values'
      }

      # Close 'Check if character string'
    }

    lgc_lgc <- substr(chr_columns[k], 5, 7 ) %in% 'LGC'

    # Logical variable
    if ( lgc_lgc ) {

      dtf_data[[ chr_columns[k] ]] <- as.numeric(
        dtf_data[[ chr_columns[k] ]]
      )

      # Close 'Logical variable'
    }

    # Close 'Loop over columns'
  }

  # If any columns to remove
  if ( length(chr_remove) > 0 ) {

    dtf_data <- dtf_data[, !colnames(dtf_data) %in% chr_remove]

    # Close 'If any columns to remove'
  }

  if (lgc_mapping) return( dtf_mapping )

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

#### D) swaap_recode.discrimination ####
#' Recode Discrimination Items
#'
#' Recodes discrimination items.
#'
#' @param dtf_data A data frame, assumed to
#'   follow the standardized format for the
#'   school-wide assessment data.
#'
#' @author Kevin Potter
#'
#' @returns A data frame with the additional variables
#' \code{'INV.INT.DISC.Q1.Gender'},
#' \code{'INV.INT.DISC.Q2.Sexuality'},
#' \code{'INV.INT.DISC.Q3.Religion'},
#' \code{'INV.INT.DISC.Q4.Disability'},
#' \code{'INV.INT.DISC.Q5.Money'}, and
#' \code{'INV.INT.DISC.Q6.Other'}.
#'
#' @export

swaap_recode.discrimination <- function(
    dtf_data ) {

  chr_columns <- colnames(dtf_data)

  chr_questions <- c(
    'Gender',
    'Sexuality',
    'Religion',
    'Disability',
    'Money',
    'Other'
  )

  # Loop over questions
  for ( i in seq_along(chr_questions) ) {

    chr_current <-
      paste0( 'INV.INT.DISC.', chr_questions[i], '.Q', i )

    # Question found
    if ( chr_current %in% chr_columns ) {

      dtf_data[[
        paste0(
          'INV.INT.DISC.',
          'Q', i, '.',
          chr_questions[i]
        )
      ]] <- dtf_data[[ chr_current ]]

      # Close 'Question found'
    }

    # Close 'Loop over questions'
  }

  return(dtf_data)
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
#' \code{'SBJ.LGC.Experience.UsedDrugsAtSchool'},
#' \code{'SBJ.CHR.Experience.GradesInSchool'},
#' \code{'SBJ.INT.Experience.GradesInSchool'}, and
#' \code{'SBJ.CHR.Experience.IEP'}.
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

  # Recode item for using substances on school grounds
  if ( 'INV.INT.SchoolXP.Substance' %in% chr_columns ) {

    dtf_data$SBJ.LGC.Experience.UsedDrugsAtSchool <-
      dtf_data$INV.INT.SchoolXP.Substance == 1

    # Close 'Recode item for using substances on school grounds'
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

  # Recode item for IEP [Individualized education program]
  if ( 'INV.INT.IEP.Member' %in% chr_columns ) {

    dtf_data$SBJ.CHR.Experience.IEP <- c(
      'No',
      'Yes',
      'Not sure'
    )[dtf_data$INV.INT.IEP.Member+1]

    # Close 'Recode item for IEP [Individualized education program]'
  }

  return( dtf_data )
}

#### I) swaap_recode.intermittent ####
#' Recode Variables at Intermittent Time Points
#'
#' Renames variables that occurred at
#' intermittent time points.
#'
#' @param dtf_data A data frame, assumed to
#'   follow the standardized format for the
#'   school-wide assessment data.
#'
#' @author Kevin Potter
#'
#' @returns A data frame with the additional variables
#' \code{'SBJ.LGC.CloseConnection.<Type>'},
#' \code{'SBJ.INT.CloseConnection.Happiness'},
#' \code{'SBJ.LGC.Language.EnglishWasFirst'} and
#' \code{'SBJ.LGC.Language.EnglishAtHome'}.
#'
#' @export

swaap_recode.intermittent <- function(
    dtf_data ) {

  chr_columns <- colnames(dtf_data)

  #### I.1) Close connections [2024] ####

  chr_terms <- c(
    Friend = 'Friend',
    Adult.Parent = 'Parent',
    Adult.Teacher = 'Teacher'
  )

  # Loop over terms
  for ( i in seq_along(chr_terms) ) {

    chr_old <- paste0( 'INV.INT.CloseConnection.', names(chr_terms)[i] )
    chr_new <- paste0( 'SBJ.LGC.CloseConnection.', chr_terms[i] )

    if ( chr_old %in% chr_columns )
      dtf_data[[ chr_new ]] <- as.logical( dtf_data[[ chr_old ]] )

    # Close 'Loop over terms'
  }

  # Add variable on happiness with support
  if ( 'INV.FCT.Support.Happiness' %in% chr_columns ) {

    dtf_data$SBJ.INT.CloseConnection.Happiness <- as.numeric(
      factor(
        dtf_data$INV.FCT.Support.Happiness,
        levels = c(
          "Very unhappy",
          "Unhappy",
          "Neutral (Not unhappy or happy)",
          "Happy",
          "Very happy"
        )
      )
    )
    dtf_data$SBJ.CHR.CloseConnection.Happiness <- as.character(
      dtf_data$INV.FCT.Support.Happiness
    )

    # Close 'Add variable on happiness with support'
  }

  #### I.2) Language [2023-2024] ####

  if ( 'SBJ.LGL.FirstLanguage.English' %in% chr_columns )
    dtf_data$SBJ.LGC.Language.EnglishWasFirst <-
      dtf_data$SBJ.LGL.FirstLanguage.English

  if ( 'SBJ.LGL.HomeLanguage.English' %in% chr_columns )
    dtf_data$SBJ.LGC.Language.EnglishAtHome <-
      dtf_data$SBJ.LGL.HomeLanguage.English

  #### I.3) Sleep [2024] ####

  if ( 'INV.FCT.SchoolDay.WakeupTime' %in% chr_columns )
    dtf_data$SBJ.CHR.Sleep.TimeWakeUp <-
      dtf_data$INV.FCT.SchoolDay.WakeupTime

  if ( 'INV.FCT.SchoolDay.SleepTime' %in% chr_columns )
    dtf_data$SBJ.CHR.Sleep.TimeGoToBed <-
      dtf_data$INV.FCT.SchoolDay.SleepTime

  if ( 'INV.INT.SchoolDay.Sleepiness' %in% chr_columns )
    dtf_data$SBJ.INT.Sleep.TirednessDuringDay <-
      dtf_data$INV.INT.SchoolDay.Sleepiness

  #### I.4) Climate change [2024] ####

  if ( 'INV.INT.ClimateChange.Worries' %in% chr_columns )
    dtf_data$SBJ.LGC.ClimateChange.Worried <- c(
      FALSE, TRUE
    )[ dtf_data$INV.INT.ClimateChange.Worries + 1 ]

  # Recode impact on daily life
  if ( 'INV.FCT.ClimateChange.Worries.DailyLife' %in% chr_columns ) {

    dtf_data$SBJ.CHR.ClimateChange.ImpactOnDailyLife <-
      dtf_data$INV.FCT.ClimateChange.Worries.DailyLife

    if ( 'INV.INT.ClimateChange.Worries' %in% chr_columns )
      dtf_data$SBJ.CHR.ClimateChange.ImpactOnDailyLife[
        dtf_data$SBJ.LGC.ClimateChange.Worried %in% FALSE
      ] <- 'Not worried in first place'

    # Close 'Recode impact on daily life'
  }

  # Recode coping strategies
  if ( 'INV.FCT.ClimateChange.Worries.HelpfulWays' %in% chr_columns ) {

    dtf_data$SBJ.CHR.ClimateChange.CopingStrategies <-
      dtf_data$INV.FCT.ClimateChange.Worries.HelpfulWays

    if ( 'INV.INT.ClimateChange.Worries' %in% chr_columns )
      dtf_data$SBJ.CHR.ClimateChange.CopingStrategies[
        dtf_data$SBJ.LGC.ClimateChange.Worried %in% FALSE
      ] <- 'Not worried in first place'

    # Close 'Recode coping strategies'
  }

  #### I.5) Social media [2024] ####

  if ( 'INV.FCT.SocialMedia.Use.Frequency' %in% chr_columns )
    dtf_data$SBJ.CHR.SocialMediaUseFrequency <-
      dtf_data$INV.FCT.SocialMedia.Use.Frequency

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
#' \code{'INV.INT.AUDIT.Q1.<Label>'} to
#' \code{'INV.INT.AUDIT.Q10.<Label>'},
#' \code{'INV.CHR.AUDIT.CutOffs'},
#' \code{'INV.INT.ADDI.D.Total'},
#' \code{'INV.INT.ADDI.U.Total'},
#' \code{'INV.INT.ADDI.Q1.<Label>'} to
#' \code{'INV.INT.ADDI.Q15.<Label>'},
#' \code{'INV.INT.ERS.Persistence'},
#' \code{'INV.INT.ERS.Sensitivity'},
#' \code{'INV.INT.ERS.Intensity'},
#' \code{'INV.INT.ERS.Q01.<Subscale>.<Label>'} to
#' \code{'INV.INT.ERS.Q21.<Subscale>.<Label>'},
#' \code{'INV.CHR.APSS.CutOffs'},
#' \code{'INV.LGC.APSS.AtRisk'}.
#'
#' @export

swaap_recode.inventories <- function(
    dtf_data ) {

  chr_columns <- colnames(dtf_data)

  #### I.1) ADDI ####

  chr_disc <- paste0(
    'INV.LGL.ADDI.Q',
    1:15
  )
  chr_upset <- paste0(
    'INV.INT.ADDI.Scale',
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
  for ( i in seq_along(chr_disc) ) {

    if ( chr_disc[i] %in% chr_columns )
      dtf_data[[
        paste0( 'INV.INT.ADDI.Q', i, '.D.', chr_labels[i] )
      ]] <-
        as.numeric( dtf_data[[ chr_disc[i] ]] )

    if ( chr_upset[i] %in% chr_columns )
      dtf_data[[
        paste0( 'INV.INT.ADDI.Q', i, '.U.', chr_labels[i] )
      ]] <-
        as.numeric( dtf_data[[ chr_upset[i] ]] )

    # Close 'Loop over items'
  }

  chr_items_new <-
    paste0( 'INV.INT.ADDI.Q', 1:15, '.D.', chr_labels )

  if ( all(chr_items_new %in% colnames(dtf_data)) )
    dtf_data$INV.INT.ADDI.D.Total <- rowSums(
      dtf_data[, chr_items_new], na.rm = TRUE
    )

  chr_items_new <-
    paste0( 'INV.INT.ADDI.Q', 1:15, '.U.', chr_labels )

  if ( all(chr_items_new %in% colnames(dtf_data)) )
    dtf_data$INV.INT.ADDI.U.Total <- rowSums(
      dtf_data[, chr_items_new], na.rm = TRUE
    )

  #### I.2) APSS ####

  dtf_data$INV.CHR.APSS.CutOffs <- NA
  dtf_data$INV.CHR.APSS.CutOffs[
    dtf_data$INV.DBL.APSS.Total %in% c(
      0, .5, 1, 1.5
    )
  ] <- '0-1.5 = Not at risk'
  dtf_data$INV.CHR.APSS.CutOffs[
    dtf_data$INV.DBL.APSS.Total %in% c(
      2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7
    )
  ] <- '2-7 = At risk'
  dtf_data$INV.LGC.APSS.AtRisk <-
    dtf_data$INV.CHR.APSS.CutOffs == '2-7 = At risk'

  #### I.3) AUDIT ####

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

  dtf_data$INV.CHR.AUDIT.CutOffs <- NA
  dtf_data$INV.CHR.AUDIT.CutOffs[
    dtf_data$INV.INT.AUDIT.Total %in% 0
  ] <- '0 = Abstainer'
  dtf_data$INV.CHR.AUDIT.CutOffs[
    dtf_data$INV.INT.AUDIT.Total %in% 1:7
  ] <- '1-7 = Low risk'
  dtf_data$INV.CHR.AUDIT.CutOffs[
    dtf_data$INV.INT.AUDIT.Total %in% 8:15
  ] <- '8-15 = Hazardous'
  dtf_data$INV.CHR.AUDIT.CutOffs[
    dtf_data$INV.INT.AUDIT.Total %in% 16:19
  ] <- '16-19 = Harmful'
  dtf_data$INV.CHR.AUDIT.CutOffs[
    dtf_data$INV.INT.AUDIT.Total %in% 20:40
  ] <- '20-40 = High risk'

  #### I.4) ERS ####

  chr_items <- c(
    "INV.INT.ERS.Q01.Persistence1",
    "INV.INT.ERS.Q02.Sensitivity1",
    "INV.INT.ERS.Q03.IntensityArousal1",
    "INV.INT.ERS.Q04.IntensityArousal2",
    "INV.INT.ERS.Q05.Sensitivity2",
    "INV.INT.ERS.Q06.IntensityArousal3",
    "INV.INT.ERS.Q07.Sensitivity3",
    "INV.INT.ERS.Q08.Persistence2",
    "INV.INT.ERS.Q09.Sensitivity4",
    "INV.INT.ERS.Q10.Persistence3",
    "INV.INT.ERS.Q11.Persistence4",
    "INV.INT.ERS.Q12.Sensitivity5",
    "INV.INT.ERS.Q13.Sensitivity6",
    "INV.INT.ERS.Q14.Sensitivity7",
    "INV.INT.ERS.Q15.Sensitivity8",
    "INV.INT.ERS.Q16.Sensitivity9",
    "INV.INT.ERS.Q17.IntensityArousal4",
    "INV.INT.ERS.Q18.Sensitivity10",
    "INV.INT.ERS.Q19.IntensityArousal5",
    "INV.INT.ERS.Q20.IntensityArousal6",
    "INV.INT.ERS.Q21.IntensityArousal7"
  )
  chr_labels <- c(
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

  # Loop over items
  for ( i in seq_along(chr_items) ) {

    if ( chr_items[i] %in% chr_columns )
      dtf_data[[
        paste0( 'INV.INT.ERS.Q', i, '.', chr_labels[i] )
      ]] <-
        as.numeric( dtf_data[[ chr_items[i] ]] )

    # Close 'Loop over items'
  }

  chr_items_new <-
    paste0( 'INV.INT.ERS.Q', 1:21, '.', chr_labels )

  int_subscale <- which( grepl( 'P.', chr_labels, fixed = TRUE ) )
  if ( all(chr_items_new[int_subscale] %in% colnames(dtf_data)) )
    dtf_data$INV.INT.ERS.Persistence <- rowSums(
      dtf_data[, chr_items_new[int_subscale]]
    )

  int_subscale <- which( grepl( 'S.', chr_labels, fixed = TRUE ) )
  if ( all(chr_items_new[int_subscale] %in% colnames(dtf_data)) )
    dtf_data$INV.INT.ERS.Sensitivity <- rowSums(
      dtf_data[, chr_items_new[int_subscale]]
    )

  int_subscale <- which( grepl( 'I.', chr_labels, fixed = TRUE ) )
  if ( all(chr_items_new[int_subscale] %in% colnames(dtf_data)) )
    dtf_data$INV.INT.ERS.Intensity <- rowSums(
      dtf_data[, chr_items_new[int_subscale]]
    )

  #### I.5) PHQ-4 ####

  dtf_data$INV.CHR.PHQ4.CutOffs <- NA
  dtf_data$INV.CHR.PHQ4.CutOffs[
    dtf_data$INV.INT.PHQ4.Total %in% 0:2
  ] <- 'Normal'
  dtf_data$INV.CHR.PHQ4.CutOffs[
    dtf_data$INV.INT.PHQ4.Total %in% 3:5
  ] <- 'Mild distress'
  dtf_data$INV.CHR.PHQ4.CutOffs[
    dtf_data$INV.INT.PHQ4.Total %in% 6:8
  ] <- 'Moderate distress'
  dtf_data$INV.CHR.PHQ4.CutOffs[
    dtf_data$INV.INT.PHQ4.Total %in% 9:12
  ] <- 'High distress'
  dtf_data$INV.LGC.PHQ4.Distress <-
    dtf_data$INV.INT.PHQ4.Anxiety >= 3 |
    dtf_data$INV.INT.PHQ4.Depression >= 3
  dtf_data$INV.LGC.PHQ4.Anxiety <-
    dtf_data$INV.INT.PHQ4.Anxiety >= 3
  dtf_data$INV.LGC.PHQ4.Depression <-
    dtf_data$INV.INT.PHQ4.Depression >= 3

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
#' \code{'SBJ.INT.Link.DistrictCode'},
#' \code{'SBJ.INT.Link.SchoolCode'},
#' \code{'SBJ.INT.Link.SchoolID'},
#' \code{'SBJ.CHR.Link.Sex'},
#' \code{'SBJ.CHR.Link.BirthYearMonth'},
#' \code{'SBJ.CHR.Link.OlderSiblings'},
#' \code{'SBJ.CHR.Link.EyeColor'}, and
#' \code{'SBJ.CHR.Link.MiddleInitial'} for
#' standard linking, and
#' \code{SBJ.INT.Link.FL.Sex},
#' \code{SBJ.CHR.Link.FL.MiddleInitial},
#' \code{SBJ.CHR.Link.FL.StreetName},
#' \code{SBJ.INT.Link.FL.EyeColor},
#' \code{SBJ.INT.Link.FL.Siblings},
#' \code{SBJ.CHR.Link.FL.SiblingBirthMonth}, and
#' \code{SBJ.CHR.Link.FL.BirthYearMonth} for
#' linking with [fastLink::fastLink].
#'
#' @export

swaap_recode.linking <- function(
    dtf_data ) {

  chr_columns <- colnames(dtf_data)

  if ( 'SSS.INT.District.Code' %in% chr_columns )
    dtf_data$SBJ.INT.Link.DistrictCode <-
      dtf_data$SSS.INT.District.Code

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

  # Create recoded variables specific for using 'fastLink'

  # Recode biological sex
  if ( 'SBJ.CHR.Link.Sex' %in% colnames(dtf_data) ) {

    dtf_data$SBJ.INT.Link.FL.Sex <- NA
    dtf_data$SBJ.INT.Link.FL.Sex[
      dtf_data$SBJ.CHR.Link.Sex %in% 'Male'
    ] <- 1
    dtf_data$SBJ.INT.Link.FL.Sex[
      dtf_data$SBJ.CHR.Link.Sex %in% 'Female'
    ] <- 2

    # Close 'Recode biological sex'
  }

  # Recode middle initial
  if ( 'SBJ.CHR.Link.MiddleInitial' %in% colnames(dtf_data) ) {

    chr_initial <- dtf_data$SBJ.CHR.Link.MiddleInitial
    lgc_NMI <- chr_initial %in% 'no middle name'
    chr_initial[lgc_NMI] <- '0'
    dtf_data$SBJ.CHR.Link.FL.MiddleInitial <- chr_initial

    # Close 'Recode middle initial'
  }

  # Recode street name
  if ( 'SBJ.CHR.Link.Streetname' %in% colnames(dtf_data) ) {

    chr_street <- dtf_data$SBJ.CHR.Link.Streetname
    lgc_2 <- nchar( chr_street ) %in% 2
    chr_street[lgc_2] <- paste0(
      chr_street[lgc_2], '1'
    )
    lgc_1 <- nchar( chr_street ) %in% 1
    chr_street[lgc_1] <- paste0(
      chr_street[lgc_1], '11'
    )
    dtf_data$SBJ.CHR.Link.FL.StreetName <- chr_street

    # Close 'Recode middle initial'
  }

  # Recode eye color
  if ( 'SBJ.CHR.Link.EyeColor' %in% colnames(dtf_data) ) {

    dtf_data$SBJ.INT.Link.FL.EyeColor <- as.numeric(
      as.factor( dtf_data$SBJ.CHR.Link.EyeColor)
    ) + 1

    # Close 'Recode eye color'
  }

  # Recode older siblings
  if ( 'SBJ.CHR.Link.OlderSiblings' %in% colnames(dtf_data) ) {

    dtf_data$SBJ.INT.Link.FL.Siblings <-
      substr( dtf_data$SBJ.CHR.Link.OlderSiblings, 1, 1 )
    dtf_data$SBJ.INT.Link.FL.Siblings[
      dtf_data$SBJ.INT.Link.FL.Siblings %in% 'n'
    ] <- '0'
    dtf_data$SBJ.INT.Link.FL.Siblings <- as.numeric(
      dtf_data$SBJ.INT.Link.FL.Siblings
    )

    dtf_data$SBJ.CHR.Link.FL.SiblingBirthMonth <- NA
    lgc_NOS <- dtf_data$SBJ.CHR.Link.OlderSiblings %in%
      'no older siblings'
    dtf_data$SBJ.CHR.Link.FL.SiblingBirthMonth[lgc_NOS] <-
      'Not'

    # Loop over months
    for ( m in seq_along(month.name) ) {

      lgc_month <- grepl(
        month.name[m],
        dtf_data$SBJ.CHR.Link.OlderSiblings
      )
      dtf_data$SBJ.CHR.Link.FL.SiblingBirthMonth[
        lgc_month %in% TRUE
      ] <- month.abb[m]

      # Close 'Loop over months'
    }

    # Close 'Recode older siblings'
  }

  # Recode birth year and month
  if ( 'SBJ.CHR.Link.BirthYearMonth' %in% colnames(dtf_data) ) {

    dtf_data$SBJ.CHR.Link.FL.BirthYearMonth <- NA

    chr_year_abbr <- substr( dtf_data$SBJ.CHR.Link.BirthYearMonth, 3, 4 )
    chr_month <- substr( dtf_data$SBJ.CHR.Link.BirthYearMonth, 6, 7 )
    chr_month[ chr_month %in% as.character(1:9) ] <- paste0(
      '0', chr_month[ chr_month %in% as.character(1:9) ]
    )

    dtf_data$SBJ.CHR.Link.FL.BirthYearMonth <- paste0(
      chr_year_abbr, chr_month
    )
    dtf_data$SBJ.CHR.Link.FL.BirthYearMonth[
      grepl( 'NA', dtf_data$SBJ.CHR.Link.FL.BirthYearMonth )
    ] <- NA

    # Close 'Recode birth year and month'
  }

  return( dtf_data )
}

#### M) swaap_recode.misc ####
#' Recode Miscellaneous Items
#'
#' Renames miscellaneous items.
#'
#' @param dtf_data A data frame, assumed to
#'   follow the standardized format for the
#'   school-wide assessment data.
#'
#' @author Kevin Potter
#'
#' @returns A data frame with the additional variables
#' \code{'SBJ.CHR.PrescribedMedicationHealth'},
#' \code{'SBJ.LGC.SoughtHelp.<Type>'}, and
#' \code{'SBJ.LGC.ConnectWithSchoolServices'}.
#'
#' @export

swaap_recode.misc <- function(
    dtf_data ) {

  chr_columns <- colnames(dtf_data)

  #### M.1) Prescribed medication [2022+] ####

  # Ongoing: Added 2022

  if ( 'INV.INT.Health.Medication' %in% chr_columns )
    dtf_data$SBJ.CHR.PrescribedMedicationHealth <- c(
      'No',
      'Yes',
      'Not sure'
    )[ dtf_data$INV.INT.Health.Medication + 1 ]

  #### M.2) Help-seeking [2020+] ####

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
    'NotListed', # 13
    'Nurse', # 14
    'YouthWellnessCoach' # 15
  )

  # Loop over terms
  for ( i in seq_along(chr_terms) ) {

    chr_old <- paste0( 'INV.LGL.HelpSeeking', (0:15)[i] )
    chr_new <- paste0( 'SBJ.LGC.SoughtHelp.', chr_terms[i] )

    if ( chr_old %in% chr_columns )
      dtf_data[[ chr_new ]] <-
        dtf_data[[ chr_old ]]

    # Close 'Loop over terms'
  }

  # Identify which variables exist
  lgc_present <- chr_terms %in% colnames(dtf_data)

  # Ensure consistency
  if ( lgc_present[1] & any(lgc_present[-1]) ) {

    chr_present <- chr_terms[lgc_present][-1]

    lgc_none <- dtf_data$SBJ.LGC.SoughtHelp.None %in% TRUE

    lgc_any <- dtf_data[lgc_none, chr_present] |> apply(
      1, function(x) any( x %in% TRUE )
    )

    dtf_data$SBJ.LGC.SoughtHelp.None[lgc_none] <-
      !lgc_any

    # Close 'Ensure consistency'
  }

  if ( 'INV.CHR.HelpSeeking.Other' %in% chr_columns )
    dtf_data$SBJ.CHR.SoughtHelp.Other <-
      dtf_data$INV.CHR.HelpSeeking.Other

  if ( 'INV.FCT.HelpSeeking.Past6months.Frequency' %in% chr_columns )
    dtf_data$SBJ.CHR.SoughtHelp.FrequencyPast6Months <-
      dtf_data$INV.FCT.HelpSeeking.Past6months.Frequency

  if ( 'INV.FCT.HelpSeeking.Past6months.Helpfulness' %in% chr_columns )
    dtf_data$SBJ.CHR.SoughtHelp.HelpfulnessPast6Months <-
      dtf_data$INV.FCT.HelpSeeking.Past6months.Helpfulness

  #### M.3) Connect with school services [2024+] ####

  if ( 'INV.INT.Connection.Services' %in% chr_columns )
    dtf_data$SBJ.LGC.ConnectWithSchoolServices <-
      c( FALSE, TRUE )[ dtf_data$INV.INT.Connection.Services + 1 ]

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
#' \code{'QLT.LGC.AttentionChecks.MetAll'}, and
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

