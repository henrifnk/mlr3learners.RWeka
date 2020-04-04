library(mlr3learners.rweka)

test_that("classif.J48", {
  learner = lrn("classif.J48")
  fun = RWeka::J48
    exclude = c(
      # Examples how to exclude certain parameters. Always comment why a parameter
      # was excluded!
      "formula", # handled via mlr3
      "data", # handled via mlr3
      "control" # handled to RWeka::Weka_Control

    )
  
  ParamTest = run_paramtest(learner, fun, exclude)
  expect_true(ParamTest, info = paste0("\nMissing parameters:\n",
                                       paste0("- '", ParamTest$missing,"'", collapse = "\n")))
})

# example for checking a "control" function of a learner
test_that("classif.Weka_control", {
  learner = lrn("classif.J48")
  fun = RWeka::Weka_control # replace!
  exclude = c(
   
   )
  
  ParamTest = run_paramtest(learner, fun, exclude)
  expect_true(ParamTest, info = paste0("\nMissing parameters:\n",
                                       paste0("- '", ParamTest$missing,"'", collapse = "\n")))
})