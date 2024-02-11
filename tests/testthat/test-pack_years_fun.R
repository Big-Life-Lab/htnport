# test-pack_years_dev.R

library(testthat)
library(logger)

test_that("Correct calculation for SMKDSTY 1", {
  expect_equal(
    pack_years_dev(SMKDSTY = 1, CLC_AGE = 30, SMK_52 = 20, SMK_31 = 20),
    10
  ) 
})

test_that("pack_years calculation is correct for valid inputs", {

  # SMKDSTY = 1 Daily smoker: one pack/day), 20 years smoking (from age 20 to age 30)
  expect_equal(pack_years_dev(SMKDSTY = 1, CLC_AGE = 30, SMK_52 = 20, SMK_31 = 20), 10)
  
  # SMKDSTY = 2 Occasional smoker (former daily): 5 cigarettes a day, 5 days per month, started smoking age 15 and stopped age 40 years
  expect_equal(pack_years_dev(SMKDSTY = 5, CLC_AGE = 50, SMKDSTP = 20, SMK_52 = 10, SMK_41 = 5, SMK_23 = 4, SMK_21 = 25, SMK_11 = 1), 0.007)
  
})

# # Test Boundary Conditions
# test_that("pack_years calculation handles boundary conditions", {
#   # Assuming minimum smoking age is 18 and maximum packs per day is, say, 2
#   expect_equal(pack_years_dev(SMKDSTY = 6, CLC_AGE = 18, SMK_52 = 0, SMK_31 = 0), 0)
#   expect_equal(pack_years_dev(SMKDSTY = 6, CLC_AGE = 60, SMK_52 = 42, SMK_31 = 0), 84) # If 6 corresponds to 2 packs/day
#   # ... additional boundary test cases ...
# })
# 
# # Test Special Cases
# test_that("pack_years calculation handles special cases", {
#   # Assuming non-smokers have SMKDSTY as 1 but with 0 packs a day
#   expect_equal(pack_years_dev(SMKDSTY = 1, CLC_AGE = 30, SMK_52 = 0, SMK_31 = 0), 0)
#   # ... additional special test cases ...
# })
# 
# # Assuming the `pack_years_dev` function is properly handling invalid inputs, we could test for those as well.
# # Test that invalid inputs are handled properly would be written once the function is designed to handle such cases.
# 
# # Test Edge Cases
# test_that("pack_years calculation handles edge cases", {
#   # If the legal smoking age is 18 and someone just started smoking
#   expect_equal(pack_years_dev(SMKDSTY = 1, CLC_AGE = 18, SMK_52 = 0, SMK_31 = 0), 0)
#   # If someone smoked exactly 19 cigarettes a day for a year (not a full pack)
#   expect_equal(pack_years_dev(SMKDSTY = 1, CLC_AGE = 19, SMK_52 = 1, SMK_31 = 0), 0.95) # Assuming 20 cigarettes per pack
#   # ... additional edge test cases ...
# })
