library(mlr3learners.rweka)

test_that("classif.J48", {
  learner = lrn("classif.J48")
  fun = RWeka::J48
    exclude = c(
      "formula", # handled via mlr3
      "data", # handled via mlr3
      "control" # handled to RWeka::Weka_Control
    )
  ParamTest = run_paramtest(learner, fun, exclude)
  expect_true(ParamTest, info = paste0("\nMissing parameters:\n",
                                       paste0("- '", ParamTest$missing, "'", collapse = "\n")))
})

test_that("classif.Weka_control", {
  learner = lrn("classif.J48")
  fun = RWeka::Weka_control
  exclude = NULL
    ParamTest = run_paramtest(learner, fun, exclude)
  expect_true(ParamTest, info = paste0(
    "\nMissing parameters:\n",
    paste0("- '", ParamTest$missing, "'", collapse = "\n")))
})
