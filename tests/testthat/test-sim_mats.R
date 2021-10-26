test_that("SeasonalAbsDistance works when around and direct dist are needed", {
  px <- 1
  py <- 6
  pz <- 3
  np <- 7

  #Test of around dist
  expect_equal(SeasonalAbsDistance(px,py,np), 2)

  #Test of direct dist
  expect_equal(SeasonalAbsDistance(px,pz,np), 2)
})
