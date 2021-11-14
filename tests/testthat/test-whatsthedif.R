# library(testthat)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


# span_or_eng -------------------------------------------------------------

test_that("Test span_or_eng", {
  # Should return NA
  expect_equal(span_or_eng(NA, NA, NA), NA)
  expect_equal(span_or_eng(2, NA, NA), NA)

  # Expect warnings
  expect_warning(span_or_eng(NA, 1, NA))
  expect_warning(span_or_eng(NA, 1, 1))
  expect_warning(span_or_eng(NA, 1, 2))
  expect_warning(span_or_eng(NA, NA, 2))
  expect_warning(span_or_eng(NA, 1, NA))

  # Expect a warning - Language is English, but only Spanish data
  expect_warning(span_or_eng(1, NA, 6))

  # Expect a warning - Different data in English and Spanish
  expect_warning(span_or_eng(1, 5, 6))

  # If the function is working
  expect_equal(span_or_eng(1, "Some English data", NA),
                    "Some English data")
  expect_equal(span_or_eng(2, NA, "Some Spanish data"),
                    "Some Spanish data")
})

test_that("raw_score_mp", {

  mp_test1 <- c(1, 2, 3, 4, 5, 5, 5)
  mp_test2 <- c(1, 2, 3, 4, 5, 5, NA)
  mp_test3 <- c(1, 2, 3, 4, NA, NA, NA)
  mp_test4 <- c(1, 2, 3, 4, 9, 9, 9)

  mp_test_df <- rbind(mp_test1,
                      mp_test2,
                      mp_test3,
                      mp_test4)

  expect_equal(get_raw_score_mp(mp_test1), 25)
  expect_equal(get_raw_score_mp(mp_test2), (20 / 6) * 7)
  expect_equal(get_raw_score_mp(mp_test3), NA)
  expect_equal(get_raw_score_mp(mp_test4), NA)

  rm(mp_test1,
     mp_test2,
     mp_test3,
     mp_test4,
     mp_test_df)

})
