#' @import data.table
#' @import paradox
#' @import mlr3misc
#' @importFrom R6 R6Class
#' @importFrom mlr3 mlr_learners LearnerClassif LearnerRegr
"_PACKAGE"

# nocov start
register_mlr3 = function(libname, pkgname) {

  # get mlr_learners dictionary from the mlr3 namespace

  x = utils::getFromNamespace("mlr_learners", ns = "mlr3")

  # add the learner to the dictionary
  x$add("classif.AdaBoostM1", LearnerClassifAdaBoostM1)
  x$add("classif.IBk", LearnerClassifIBk)
  x$add("regr.IBk", LearnerRegrIBk)
  x$add("classif.JRip", LearnerClassifJRip)
  x$add("classif.J48", LearnerClassifJ48)
  x$add("classif.LMT", LearnerClassifLMT)
  x$add("regr.M5Rules", LearnerRegrM5Rules)
  x$add("classif.OneR", LearnerClassifOneR)
  x$add("classif.PART", LearnerClassifPART)
}

.onLoad = function(libname, pkgname) { # nolint
  register_mlr3()
  setHook(packageEvent("mlr3", "onLoad"), function(...) register_mlr3(),
    action = "append")
}

.onUnload = function(libpath) { # nolint
  event = packageEvent("mlr3", "onLoad")
  hooks = getHook(event)
  pkgname = vapply(hooks, function(x) environment(x)$pkgname, NA_character_)
  setHook(event, hooks[pkgname != "mlr3learners.rweka"],
    action = "replace")
}
# nocov end
