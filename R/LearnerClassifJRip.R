#' @title Classification JRip Learner
#'
#' @name mlr_learners_classif.JRip
#'
#' @description
#' A [mlr3::LearnerClassif] implementing classification JRip from package \CRANpkg{RWeka}.
#' Calls [RWeka::JRip()].
#'
#' @templateVar id classif.Jrip
#' @template section_dictionary_learner
#'
#' @references
#' Cohen W (1995). 
#' Fast effective rule induction
#' In: Proceedings of the 12th International Conference on Machine Learning, pages 115–123.
#' \url{http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.50.8204}
#'
#' @export
LearnerClassifJRip <- R6Class("LearnerClassifJRip",
  inherit = LearnerClassif,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps <- ParamSet$new(
        params = list(
          ParamUty$new(id = "subset", tags = c("train", "pars")),
          ParamUty$new(id = "na.action", tags = c("train", "pars")),
          ParamInt$new(id = "F", default = 3L, lower = 2L, tags = c("train", "Weka_control")),
          ParamDbl$new(id = "N", default = 2, lower = 0, tags = c("train", "Weka_control")),
          ParamInt$new(id = "O", default = 2L, lower = 1L, tags = c("train", "Weka_control")),
          ParamLgl$new(id = "D", default = FALSE, tags = c("train", "Weka_control")),
          ParamInt$new(id = "S", default = 1, tags = c("train", "Weka_control")),
          ParamLgl$new(id = "E", default = FALSE, tags = c("train", "Weka_control")),
          ParamLgl$new(id = "P", default = FALSE, tags = c("train", "Weka_control")),
          ParamUty$new(id = "options", default = NULL, tags = c("train", "pars"))
        )
      )

      super$initialize(
        id = "classif.JRip",
        packages = "RWeka",
        feature_types = c("numeric", "factor", "ordered"),
        predict_types = c("response", "prob"),
        param_set = ps,
        properties = c("twoclass", "multiclass", "missings"),
        man = "mlr3learners.RWeka::mlr_learners_classif.JRip"
      )
    }
  ),

  private = list(
    .train = function(task) {
      ctrl <- do.call(
        RWeka::Weka_control,
        self$param_set$get_values(tags = "Weka_control")
      )

      pars <- self$param_set$get_values(tags = "pars")
      f <- task$formula()
      data <- task$data()
      invoke(RWeka::JRip, formula = f, data = data, control = ctrl, .args = pars)
    },

    .predict = function(task) {
      response <- NULL
      prob <- NULL
      newdata <- task$data(cols = task$feature_names)

      if (self$predict_type == "response") {
        response <- invoke(predict, self$model,
          newdata = newdata, type = "class"
        )
      } else {
        prob <- invoke(predict, self$model,
          newdata = newdata, type = "prob"
        )
      }
      PredictionClassif$new(task = task, response = response, prob = prob)
    }
  )
)
