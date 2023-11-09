# test-alcohol.R

library(testthat)

# Test for low_drink_score_fun
test_that("low_drink_score_fun returns correct scores", {
  expect_equal(low_drink_score_fun(CLC_SEX = 1, ALCDWKY = 3), 1) # Low risk for male
  expect_equal(low_drink_score_fun(CLC_SEX = 2, ALCDWKY = 3), 1) # Low risk for female
  expect_equal(low_drink_score_fun(CLC_SEX = 1, ALCDWKY = 12), 1) # Low risk for male
  expect_equal(low_drink_score_fun(CLC_SEX = 2, ALCDWKY = 12), 2) # Marginal risk for female
  expect_equal(low_drink_score_fun(CLC_SEX = 1, ALCDWKY = 18), 2) # Marginal risk for male
  expect_equal(low_drink_score_fun(CLC_SEX = 2, ALCDWKY = 18), 3) # Medium risk for female
  expect_equal(low_drink_score_fun(CLC_SEX = 1, ALCDWKY = 25), 3) # Medium risk for male
  expect_equal(low_drink_score_fun(CLC_SEX = 2, ALCDWKY = 25), 3) # Medium risk for female
  expect_true(is.na(low_drink_score_fun(CLC_SEX = 1, ALCDWKY = 996))) # Check for NA
})


# Test for low_drink_score_fun1
test_that("low_drink_score_fun1 returns correct scores", {
  expect_equal(low_drink_score_fun1(CLC_SEX = 1, ALCDWKY = 3, ALC_17 = 1, ALC_11 = 1), 2, info = "Low risk for male who drank in the past year")
  expect_equal(low_drink_score_fun1(CLC_SEX = 2, ALCDWKY = 3, ALC_17 = 1, ALC_11 = 1), 2, info = "Low risk for female who drank in the past year")
  expect_equal(low_drink_score_fun1(CLC_SEX = 1, ALCDWKY = 12, ALC_17 = 1, ALC_11 = 1), 2, info = "Low risk for male who drank in the past year")
  expect_equal(low_drink_score_fun1(CLC_SEX = 2, ALCDWKY = 12, ALC_17 = 1, ALC_11 = 1), 3, info = "Marginal risk for female who drank in the past year")
  expect_equal(low_drink_score_fun1(CLC_SEX = 1, ALCDWKY = 18, ALC_17 = 1, ALC_11 = 1), 3, info = "Marginal risk for male who drank in the past year")
  expect_equal(low_drink_score_fun1(CLC_SEX = 2, ALCDWKY = 18, ALC_17 = 1, ALC_11 = 1), 4, info = "Medium risk for female who drank in the past year")
  expect_equal(low_drink_score_fun1(CLC_SEX = 1, ALCDWKY = 25, ALC_17 = 1, ALC_11 = 1), 4, info = "Medium risk for male who drank in the past year")
  expect_equal(low_drink_score_fun1(CLC_SEX = 2, ALCDWKY = 25, ALC_17 = 1, ALC_11 = 1), 5, info = "High risk for female who drank in the past year")
  expect_true(is.na(low_drink_score_fun1(CLC_SEX = 1, ALCDWKY = 996, ALC_17 = 1, ALC_11 = 1)), info = "Invalid input should return NA")
  expect_equal(low_drink_score_fun1(CLC_SEX = 1, ALCDWKY = 5, ALC_17 = 2, ALC_11 = 2), 1, info = "Low risk - never drank")
  expect_equal(low_drink_score_fun1(CLC_SEX = 1, ALCDWKY = 5, ALC_17 = 1, ALC_11 = 2), 2, info = "Low risk - former drinker")
})