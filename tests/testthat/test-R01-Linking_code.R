# Testing: School-wide assessment linking code
# Written by...
#   Michael Pascale
#   Kevin Potter
# Maintained by...
#   Kevin Potter
# Email:
#   kpotter5@mgh.harvard.edu
# Please email me directly if you
# have any questions or comments
# Last updated: 2026-04-22

#### 2) swaap_link ####

#### 2.1) Exact matching ####

test_that("function works", {

  dtf_long <- swaap::swaap_simulate( 'link', 'demo' )

  expect_equal({
    dtf_linked <- swaap::swaap_link( dtf_long, chr_progress = '' )
    as.numeric( as.factor( dtf_linked$IDN.CHR.Linked ) )
  }, {
    int_ID <- dtf_long$LNK.INT.True.ID
    int_ID[ int_ID == 0 ] <-
      1:sum( int_ID == 0 ) + max( int_ID[ int_ID != 0 ] )
    int_ID
  } )

})

test_that("function works using school codes", {

  dtf_long <- swaap::swaap_simulate( 'link', 'debug' ) |>
    swaap::swaap_recode.linking()

  expect_equal({
  dtf_linked <- swaap::swaap_link( dtf_long, chr_progress = '' )
    lst_report <- swaap::swaap_link.report( dtf_linked )
    lst_report$true[, c('Hits', 'Rejects')]
  }, {
    dtf_check <- data.frame(
      Hits = c(
        8, # Standard linking
        0, # Standard no link
        6, # Dissimilarity = 1 [Base]
        6, # Dissimilarity = 1 [Add]
        1, # Duplicate records [Base]
        1, # Duplicate records [Add]
        0, # Subset dissimiliarity = 0
        0, # Dissimiliarity off by 1
        1, # Duplicate records w/ NA [Base]
        1, # Duplicate records w/ NA [Add]
        1, # Test of priority [School ID over questions]
        1, # Link using different items by time point
        4 # Special cases for duplicates
      ),
      Rejects = c(
        0, # Standard linking
        24, # Standard no link
        6, # Dissimilarity = 1 [Base]
        6, # Dissimilarity = 1 [Add]
        0, # Duplicate records [Base]
        0, # Duplicate records [Add]
        12, # Subset dissimiliarity = 0
        0, # Dissimiliarity off by 1
        0, # Duplicate records w/ NA [Base]
        0, # Duplicate records w/ NA [Add]
        0, # Test of priority [School ID over questions]
        0, # Link using different items by time point
        1 # Special cases for duplicates
      )
    )
  } )

})

test_that("function takes custom inputs", {

  dtf_long <- swaap::swaap_simulate( 'link', 'debug' ) |>
    swaap::swaap_recode.linking()
  lst_sets <- swaap::swaap_link.input.sets(
    dtf_long
  )
  lst_items <- swaap::swaap_link.input.items(
    dtf_long, lst_sets,
    obj_items = swaap::swaap_select.linking( lgc_district = TRUE )
  )
  lst_combos <- swaap::swaap_link.input.combos(
    dtf_long, lst_items
  )
  lst_missing <- swaap::swaap_link.input.missing(
    dtf_long, lst_items, lst_combos
  )

  expect_equal({
    dtf_linked <- swaap::swaap_link(
      dtf_long,
      lst_sets = lst_sets,
      obj_items = lst_items,
      lst_combos = lst_combos,
      lst_missing = lst_missing,
      chr_progress = ''
    )
    lst_report <- swaap::swaap_link.report( dtf_linked )
    lst_report$true[, c('Hits', 'Rejects')]
  }, {
    dtf_check <- data.frame(
      Hits = c(
        8, # Standard linking
        0, # Standard no link
        6, # Dissimilarity = 1 [Base]
        6, # Dissimilarity = 1 [Add]
        1, # Duplicate records [Base]
        1, # Duplicate records [Add]
        0, # Subset dissimiliarity = 0
        0, # Dissimiliarity off by 1
        1, # Duplicate records w/ NA [Base]
        1, # Duplicate records w/ NA [Add]
        1, # Test of priority [School ID over questions]
        1, # Link using different items by time point
        4 # Special cases for duplicates
      ),
      Rejects = c(
        0, # Standard linking
        24, # Standard no link
        6, # Dissimilarity = 1 [Base]
        6, # Dissimilarity = 1 [Add]
        0, # Duplicate records [Base]
        0, # Duplicate records [Add]
        12, # Subset dissimiliarity = 0
        0, # Dissimiliarity off by 1
        0, # Duplicate records w/ NA [Base]
        0, # Duplicate records w/ NA [Add]
        0, # Test of priority [School ID over questions]
        0, # Link using different items by time point
        1 # Special cases for duplicates
      )
    )
  } )

})

#### 2.2) fastLink ####

test_that("function works", {

  dtf_long <- swaap::swaap_simulate( 'link', 'debug-fastLink' ) |>
    swaap::swaap_recode.linking()

  expect_equal({
    dtf_linked <- swaap::swaap_link(
      dtf_long, chr_progress = '', chr_method = 'fastLink'
    )
    lst_report <- swaap::swaap_link.report( dtf_linked )
    lst_report$true[, c('Hits', 'Rejects')]
  }, {
    dtf_check <- data.frame(
      Hits = c(
        7, # Standard linking
           # Doesn't link pair of records that have only
           # school code & ID
        0, # Standard no link
        0 # False positive school code
      ),
      Rejects = c(
        0, # Standard linking
        24, # Standard no link
        6 # False positive school code
      )
    )
  } )

})

#### 3) Input functions ####

#### 3.1) swaap_link.input.sets ####

test_that("function works", {

  dtf_long <- swaap::swaap_simulate( 'link', 'debug' )

  expect_equal({
    swaap::swaap_link.input.sets( dtf_long )
  }, {
    lst_sets <- list(
      W1T0tW1T1 = list(
        Base = dtf_long$SSS.INT.TimePoint %in% 0,
        Add = dtf_long$SSS.INT.TimePoint %in% 1
      ),
      W1T0tW1T2 = list(
        Base = dtf_long$SSS.INT.TimePoint %in% 0,
        Add = dtf_long$SSS.INT.TimePoint %in% 2
      ),
      W1T1tW1T2 = list(
        Base = dtf_long$SSS.INT.TimePoint %in% 1,
        Add = dtf_long$SSS.INT.TimePoint %in% 2
      )
    )
    lst_sets
  } )
})

test_that("default for SBIRT pairs works", {

  dtf_long <- data.frame(
    SSS.LGC.SBIRT = TRUE,
    SSS.INT.RecruitmentWave = c(
      rep( 1, 6 ),
      rep( 1, 6 ),
      rep( 2, 2 )
    ),
    SSS.INT.SBIRTTimePoint = c(
      rep( 0:2, 2 ),
      rep( 0:2, 2 ),
      0, 0
    ),
    SSS.INT.Grade = c(
      rep( c( 9, 9, 10 ), 2 ),
      rep( c( 10, 10, 11 ), 2 ),
      c( 9, 10 )
    )
  )
  dtf_long$SSS.INT.TimePoint <- dtf_long$SSS.INT.SBIRTTimePoint

  expect_equal({
    swaap::swaap_link.input.sets( dtf_long )
  }, {
    lgc_9th <- dtf_long$SSS.INT.Grade %in% 9
    lgc_10th <- dtf_long$SSS.INT.Grade %in% 10
    lgc_11th <- dtf_long$SSS.INT.Grade %in% 11
    lgc_W1 <- dtf_long$SSS.INT.RecruitmentWave %in% 1

    lst_sets <- list(
      W1T0G9tW1T1G9 = list(
        Base = dtf_long$SSS.INT.TimePoint %in% 0 & lgc_9th & lgc_W1,
        Add = dtf_long$SSS.INT.TimePoint %in% 1 & lgc_9th & lgc_W1
      ),
      W1T0G10tW1T1G10 = list(
        Base = dtf_long$SSS.INT.TimePoint %in% 0 & lgc_10th & lgc_W1,
        Add = dtf_long$SSS.INT.TimePoint %in% 1 & lgc_10th & lgc_W1
      ),
      W1T0G9tW1T2G10 = list(
        Base = dtf_long$SSS.INT.TimePoint %in% 0 & lgc_9th & lgc_W1,
        Add = dtf_long$SSS.INT.TimePoint %in% 2 & lgc_10th & lgc_W1
      ),
      W1T0G10tW1T2G11 = list(
        Base = dtf_long$SSS.INT.TimePoint %in% 0 & lgc_10th & lgc_W1,
        Add = dtf_long$SSS.INT.TimePoint %in% 2 & lgc_11th & lgc_W1
      ),
      W1T1G9tW1T2G10 = list(
        Base = dtf_long$SSS.INT.TimePoint %in% 1 & lgc_9th & lgc_W1,
        Add = dtf_long$SSS.INT.TimePoint %in% 2 & lgc_10th & lgc_W1
      ),
      W1T1G10tW1T2G11 = list(
        Base = dtf_long$SSS.INT.TimePoint %in% 1 & lgc_10th & lgc_W1,
        Add = dtf_long$SSS.INT.TimePoint %in% 2 & lgc_11th & lgc_W1
      )
    )
    lst_sets
  } )
})

test_that("default for SBIRT duplicates works", {

  dtf_long <- data.frame(
    SSS.LGC.SBIRT = TRUE,
    SSS.INT.RecruitmentWave = c(
      rep( 1, 6 ),
      rep( 1, 6 ),
      rep( 2, 2 )
    ),
    SSS.INT.SBIRTTimePoint = c(
      rep( 0:2, 2 ),
      rep( 0:2, 2 ),
      0, 0
    ),
    SSS.INT.Grade = c(
      rep( c( 9, 9, 10 ), 2 ),
      rep( c( 10, 10, 11 ), 2 ),
      c( 9, 10 )
    )
  )
  dtf_long$SSS.INT.TimePoint <- dtf_long$SSS.INT.SBIRTTimePoint

  expect_equal({
    swaap::swaap_link.input.sets(
      dtf_long, lgc_duplicates = TRUE
    )
  }, {
    lgc_9th <- dtf_long$SSS.INT.Grade %in% 9
    lgc_10th <- dtf_long$SSS.INT.Grade %in% 10
    lgc_11th <- dtf_long$SSS.INT.Grade %in% 11
    lgc_W1 <- dtf_long$SSS.INT.RecruitmentWave %in% 1

    lst_sets <- list(
      W1T0G9 = list(
        Base = dtf_long$SSS.INT.TimePoint %in% 0 & lgc_9th & lgc_W1,
        Add = dtf_long$SSS.INT.TimePoint %in% 0 & lgc_9th & lgc_W1
      ),
      W1T0G10 = list(
        Base = dtf_long$SSS.INT.TimePoint %in% 0 & lgc_10th & lgc_W1,
        Add = dtf_long$SSS.INT.TimePoint %in% 0 & lgc_10th & lgc_W1
      ),
      W1T1G9 = list(
        Base = dtf_long$SSS.INT.TimePoint %in% 1 & lgc_9th & lgc_W1,
        Add = dtf_long$SSS.INT.TimePoint %in% 1 & lgc_9th & lgc_W1
      ),
      W1T1G10 = list(
        Base = dtf_long$SSS.INT.TimePoint %in% 1 & lgc_10th & lgc_W1,
        Add = dtf_long$SSS.INT.TimePoint %in% 1 & lgc_10th & lgc_W1
      ),
      W1T2G10 = list(
        Base = dtf_long$SSS.INT.TimePoint %in% 2 & lgc_10th & lgc_W1,
        Add = dtf_long$SSS.INT.TimePoint %in% 2 & lgc_10th & lgc_W1
      ),
      W1T2G11 = list(
        Base = dtf_long$SSS.INT.TimePoint %in% 2 & lgc_11th & lgc_W1,
        Add = dtf_long$SSS.INT.TimePoint %in% 2 & lgc_11th & lgc_W1
      ),
      W2T0G9 = list(
        Base = dtf_long$SSS.INT.TimePoint %in% 0 & lgc_9th & !lgc_W1,
        Add = dtf_long$SSS.INT.TimePoint %in% 0 & lgc_9th & !lgc_W1
      ),
      W2T0G10 = list(
        Base = dtf_long$SSS.INT.TimePoint %in% 0 & lgc_10th & !lgc_W1,
        Add = dtf_long$SSS.INT.TimePoint %in% 0 & lgc_10th & !lgc_W1
      )
    )
    lst_sets
  } )
})

#### 3.2) swaap_link.input.items ####

test_that("function works", {

  dtf_long <- swaap::swaap_simulate( 'link', 'debug' )
  lst_sets <- dtf_long |> swaap::swaap_link.input.sets()

  expect_equal({
    swaap::swaap_link.input.items( dtf_long, lst_sets )
  }, {

    lst_items <- lapply(
      seq_along(lst_sets), function(s) {
        c(
          "SSS.INT.School.Code",
          "IDX.INT.Origin.LASID",
          "SBJ.FCT.Sex",
          "SBJ.DTM.Dob",
          "SBJ.FCT.Link.MiddleInitial",
          "SBJ.FCT.Link.EyeColor",
          "SBJ.FCT.Link.OlderSiblings",
          "SBJ.CHR.Link.Streetname"
        )
      }
    )
    names(lst_items) <- names(lst_sets)

    lst_items
  } )
})

test_that("function detects recoded items", {

  dtf_long <- swaap::swaap_simulate( 'link', 'debug' ) |>
    swaap::swaap_recode.linking()
  lst_sets <- swaap::swaap_link.input.sets( dtf_long )

  expect_equal({
    swaap::swaap_link.input.items( dtf_long, lst_sets )
  }, {

    lst_items <- lapply(
      seq_along(lst_sets), function(s) {
        c(
          "SBJ.INT.Link.SchoolCode",
          "SBJ.INT.Link.SchoolID",
          "SBJ.CHR.Link.Sex",
          "SBJ.CHR.Link.BirthYearMonth",
          "SBJ.CHR.Link.MiddleInitial",
          "SBJ.CHR.Link.EyeColor",
          "SBJ.CHR.Link.OlderSiblings",
          "SBJ.CHR.Link.Streetname"
        )
      }
    )
    names(lst_items) <- names(lst_sets)

    lst_items

  } )
})

test_that("function outputs for fastLink", {

  dtf_long <- swaap::swaap_simulate( 'link', 'debug' ) |>
    swaap::swaap_recode.linking()
  lst_sets <- swaap::swaap_link.input.sets( dtf_long )

  expect_equal({
    swaap::swaap_link.input.items(
      dtf_long, lst_sets, lgc_fastLink = TRUE
    ) |> suppressWarnings()
  }, {
    lst_sets <- list(
      W1T0tW1T1 = swaap::swaap_select.linking(lgc_fastLink = TRUE),
      W1T0tW1T2 = swaap::swaap_select.linking(lgc_fastLink = TRUE),
      W1T1tW1T2 = swaap::swaap_select.linking(lgc_fastLink = TRUE)
    )
  } )
})

#### 3.3) Combos ####

#### 3.3.1) Exact match ####

test_that("function works", {

  dtf_long <- swaap::swaap_simulate( 'link', 'debug' )
  lst_sets <- swaap::swaap_link.input.sets( dtf_long )
  lst_items <- swaap::swaap_link.input.items(
    dtf_long, lst_sets = lst_sets
  )

  expect_equal({
    swaap::swaap_link.input.combos(
      dtf_long, lst_items = lst_items
    )
  }, {
    lst_combos <- list(
      C1 = 1:2,
      C2 = c( 1, 3:8 ),
      C3 = c( 1, (3:8)[-1] ),
      C4 = c( 1, (3:8)[-2] ),
      C5 = c( 1, (3:8)[-3] ),
      C6 = c( 1, (3:8)[-4] ),
      C7 = c( 1, (3:8)[-5] ),
      C8 = c( 1, (3:8)[-6] )
    )
    lst_combos <- lapply(
      seq_along(lst_sets), function(s) {
        return(lst_combos)
      }
    )
    names(lst_combos) <- names(lst_sets)

    lst_combos
  })
})

test_that("function works with recoded items", {

  dtf_long <- swaap::swaap_simulate( 'link', 'debug' ) |>
    swaap::swaap_recode.linking()
  lst_sets <- swaap::swaap_link.input.sets( dtf_long )
  lst_items <- swaap::swaap_link.input.items(
    dtf_long, lst_sets = lst_sets
  )

  expect_equal({
    swaap::swaap_link.input.combos(
      dtf_long, lst_items = lst_items
    )
  }, {
    lst_combos <- list(
      C1 = 1:2,
      C2 = c( 1, 3:8 ),
      C3 = c( 1, (3:8)[-1] ),
      C4 = c( 1, (3:8)[-2] ),
      C5 = c( 1, (3:8)[-3] ),
      C6 = c( 1, (3:8)[-4] ),
      C7 = c( 1, (3:8)[-5] ),
      C8 = c( 1, (3:8)[-6] )
    )
    lst_combos <- lapply(
      seq_along(lst_sets), function(s) {
        return(lst_combos)
      }
    )
    names(lst_combos) <- names(lst_sets)

    lst_combos
  })
})


test_that("function works with district codes", {

  dtf_long <- swaap::swaap_simulate( 'link', 'debug' ) |>
    swaap::swaap_recode.linking()
  lst_sets <- swaap::swaap_link.input.sets( dtf_long )
  lst_items <- swaap::swaap_link.input.items(
    dtf_long, lst_sets = lst_sets, lgc_district = TRUE
  )

  expect_equal({
    swaap::swaap_link.input.combos(
      dtf_long, lst_items = lst_items
    )
  }, {
    lst_combos <- list(
      C1 = 1:2,
      C2 = c( 1, 3:8 ),
      C3 = c( 1, (3:8)[-1] ),
      C4 = c( 1, (3:8)[-2] ),
      C5 = c( 1, (3:8)[-3] ),
      C6 = c( 1, (3:8)[-4] ),
      C7 = c( 1, (3:8)[-5] ),
      C8 = c( 1, (3:8)[-6] )
    )
    lst_combos <- lapply(
      seq_along(lst_sets), function(s) {
        return(lst_combos)
      }
    )
    names(lst_combos) <- names(lst_sets)

    lst_combos
  })
})

test_that("function works with non-standard items", {

  dtf_long <- swaap::swaap_simulate( 'link', 'debug' ) |>
    swaap::swaap_recode.linking()
  lst_sets <- swaap::swaap_link.input.sets( dtf_long )
  lst_items <- swaap::swaap_link.input.items(
    dtf_long, lst_sets = lst_sets,
    obj_items = swaap::swaap_select.linking( lgc_fastLink = TRUE )
  )

  expect_equal({
    swaap::swaap_link.input.combos(
      dtf_long, lst_items = lst_items
    )
  }, {
    lst_combos <- list(
      C1 = 1:2,
      C2 = c( 3:9 )
    )
    lst_combos <- lapply(
      seq_along(lst_sets), function(s) {
        return(lst_combos)
      }
    )
    names(lst_combos) <- names(lst_sets)

    lst_combos
  })
})

test_that("function works without school codes and IDs", {

  dtf_long <- swaap::swaap_simulate( 'link', 'debug' ) |>
    swaap::swaap_recode.linking()
  lst_sets <- swaap::swaap_link.input.sets( dtf_long )
  lst_items <- swaap::swaap_link.input.items(
    dtf_long, lst_sets = lst_sets,
    obj_items = swaap::swaap_select.linking()[-(1:2)]
  )

  expect_equal({
    swaap::swaap_link.input.combos(
      dtf_long, lst_items = lst_items
    )
  }, {
    lst_combos <- list(
      C1 = 1:6,
      C2 = (1:6)[-1],
      C3 = (1:6)[-2],
      C4 = (1:6)[-3],
      C5 = (1:6)[-4],
      C6 = (1:6)[-5],
      C7 = (1:6)[-6]
    )
    lst_combos <- lapply(
      seq_along(lst_sets), function(s) {
        return(lst_combos)
      }
    )
    names(lst_combos) <- names(lst_sets)

    lst_combos
  })
})

#### 3.3.2) fastLink ####

test_that("function works", {

  dtf_long <-
    swaap::swaap_simulate( 'link', 'debug' ) |>
    swaap::swaap_recode.linking()
  lst_sets <- swaap::swaap_link.input.sets( dtf_long )
  lst_items <- swaap::swaap_link.input.items(
    dtf_long, lst_sets = lst_sets, lgc_fastLink = TRUE
  )

  expect_equal({
    swaap::swaap_link.input.combos(
      dtf_long, lst_items = lst_items, lgc_fastLink = TRUE
    )
  }, {
    lst_combos <- list(
      stringdist = c( 4, 5, 8, 9 ),
      numeric = c( 1, 2, 3, 6, 7 )
    )
    lst_combos <- lapply(
      seq_along(lst_sets), function(s) {
        return(lst_combos)
      }
    )
    names(lst_combos) <- names(lst_sets)

    lst_combos
  })
})

test_that("function works with contact info", {

  dtf_long <-
    swaap::swaap_simulate( 'link', 'debug' ) |>
    swaap::swaap_recode.linking() |>
    dplyr::mutate(
      SBJ.CHR.Contact.Name = 'AAA',
      SBJ.CHR.Contact.DateOfBirth = '01-01-2025',
      SBJ.CHR.Contact.Cellphone = '???-???-????',
      SBJ.CHR.Contact.Email = 'fake@fake.com'
    )
  lst_sets <- swaap::swaap_link.input.sets( dtf_long )
  lst_items <- swaap::swaap_link.input.items(
    dtf_long, lst_sets = lst_sets,
    obj_items = swaap_select.contact()[1:4]
  )

  expect_equal({
    swaap::swaap_link.input.combos(
      dtf_long, lst_items = lst_items, lgc_fastLink = TRUE
    )
  }, {
    lst_combos <- list(
      stringdist = c( 1, 2, 4 ),
      numeric = 3,
      partial = 1
    )
    lst_combos <- lapply(
      seq_along(lst_sets), function(s) {
        return(lst_combos)
      }
    )
    names(lst_combos) <- names(lst_sets)

    lst_combos
  })
})

#### 3.4) Missingness ####

#### 3.4.1) Function creates missing treatment ####

test_that("function creates missing treatment", {

  dtf_long <-
    swaap::swaap_simulate( 'link', 'debug' ) |>
    swaap::swaap_recode.linking()
  lst_sets <- swaap::swaap_link.input.sets( dtf_long )
  lst_items <- swaap::swaap_link.input.items(
    dtf_long, lst_sets = lst_sets,
  )
  lst_combos <- swaap::swaap_link.input.combos(
    dtf_long, lst_items = lst_items
  )

  expect_equal({
    swaap::swaap_link.input.missing(
      dtf_long, lst_items = lst_items, lst_combos = lst_combos
    )
  }, {
    lst_combos <- list(
      C1 = 1:2,
      C2 = c( 1, 3:8 ),
      C3 = c( 1, 3:8 ),
      C4 = c( 1, 3:8 ),
      C5 = c( 1, 3:8 ),
      C6 = c( 1, 3:8 ),
      C7 = c( 1, 3:8 ),
      C8 = c( 1, 3:8 )
    )
    lst_combos <- lapply(
      seq_along(lst_sets), function(s) {
        return(lst_combos)
      }
    )
    names(lst_combos) <- names(lst_sets)

    lst_combos
  })
})

