context("fuzzy_join")

test_that("Can join multiple times to the same column", {
  ret <- fuzzy_inner_join(mtcars, mtcars,
                          by = c("gear" = "cyl", "carb" = "cyl"),
                          match_fun = list(`==`, `==`))

  expect_gt(nrow(ret), 0)
  expect_equal(ret$gear.x, ret$cyl.y)
  expect_equal(ret$carb.x, ret$cyl.y)
})
