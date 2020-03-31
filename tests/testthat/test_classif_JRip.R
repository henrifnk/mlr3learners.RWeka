context("classif.JRip")

skip_on_os("windows")
skip_on_cran()

requirePackagesOrSkip("RWeka", default.method = "load")

test_that("autotest", {
  learner = LearnerClassifJRip$new()
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
