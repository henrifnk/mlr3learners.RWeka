library(mlr3learners.rweka)

test_that("classif.OneR", {
  learner = lrn("classif.OneR")
  fun = RWeka::OneR
  exclude = c(
    "formula", # handled via mlr3
    "data", # handled via mlr3
    "control" # handled to RWeka::Weka_Control
  )

  ParamTest = run_paramtest(learner, fun, exclude)
  expect_true(ParamTest, info = paste0("\nMissing parameters:\n",
                                       paste0("- '", ParamTest$missing, "'", collapse = "\n")))
})

test_that("Weka_control OneR", {
  learner = lrn("classif.OneR")
  fun = RWeka::Weka_control
  exclude = c(
    character(0L)
  )

  ParamTest = run_paramtest(learner, fun, exclude)
  expect_true(ParamTest, info = paste0("\nMissing parameters:\n",
                                       paste0("- '", ParamTest$missing, "'", collapse = "\n")))
})
