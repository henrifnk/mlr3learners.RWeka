library(mlr3learners.rweka)

test_that("regr.M5Rules", {
  learner = lrn("regr.M5Rules")
  fun = RWeka::JRip
    exclude = c(
      "formula", # handled via mlr3
      "data", # handled via mlr3
      "control" # handled to RWeka::Weka_Control
    )
    
    ParamTest = run_paramtest(learner, fun, exclude)
    expect_true(ParamTest, info = paste0(
      "
Missing parameters:
",
      paste0("- '", ParamTest$missing, "'", collapse = "
")))
})

test_that("Weka_control M5 Rules", {
  learner = lrn("regr.M5Rules")
  fun = RWeka::Weka_control
  exclude = c(
    character(0L)
  )
  
  ParamTest = run_paramtest(learner, fun, exclude)
  expect_true(ParamTest, info = paste0(
    "
Missing parameters:
",
    paste0("- '", ParamTest$missing, "'", collapse = "
")))
})