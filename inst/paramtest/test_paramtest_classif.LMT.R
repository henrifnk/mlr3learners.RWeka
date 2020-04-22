library(mlr3learners.rweka)

test_that("classif.LMT", {
  learner = lrn("classif.LMT")
  fun = RWeka::LMT
  exclude = c(
    "formula", # handled via mlr3
    "data", # handled via mlr3
    "control" # handled to RWeka::Weka_Control
  )

  ParamTest = run_paramtest(learner, fun, exclude)
  expect_true(ParamTest, info = paste0("\nMissing parameters:\n",
                                       paste0("- '", ParamTest$missing, "'", collapse = "\n")))
})

test_that("Weka_control LMT", {
  learner = lrn("classif.LMT")
  fun = RWeka::Weka_control
  exclude = c(
    character(0L)
  )

  ParamTest = run_paramtest(learner, fun, exclude)
  expect_true(ParamTest, info = paste0("\nMissing parameters:\n",
                                       paste0("- '", ParamTest$missing, "'", collapse = "\n")))
})
