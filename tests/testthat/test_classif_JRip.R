context("classif.JRip")

test_that("autotest", {
  learner = LearnerClassifJRip$new()
  learner$param_set$values <- list(subset = 1:50)
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
