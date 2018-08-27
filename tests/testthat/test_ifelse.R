context("Ifelse works correct")

test_that("Implicit vectorisation",{
  p = c(-0.5, 1, 1.5)
  result = c(-0.5,0,0)
  test = p>0
  yes = 0
  no = p
  expect_equal(ifelseC(p>0,0,p), result)
}
          )