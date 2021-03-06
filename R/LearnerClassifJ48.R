#' @title Classification J48 Learner
#'
#' @name mlr_learners_classif.J48
#'
#' @description
#' A [mlr3::LearnerClassif] implementing classification J48 from package \CRANpkg{RWeka}.
#' Calls [RWeka::J48()].
#'
#' @section Custom mlr3 defaults:
#' - `output_debug_info`:
#'   - original id: output-debug-info
#'
#' - `do_not_check_capabilities`:
#'   - original id: do-not-check-capabilities
#'
#' - `num_decimal_places`:
#'   - original id: num-decimal-places
#'
#' - `batch_size`:
#'   - original id: batch-size
#'
#' - Reason for change: This learner contains changed ids of the following control arguments
#' since their ids contain irregular pattern
#'
#' @templateVar id classif.J48
#' @template section_dictionary_learner
#'
#' @references
#' Quinlan R (1993).
#' C4.5: Programs for Machine Learning
#' \url{http://www.rulequest.com/see5-unix.html}
#'
#' @export
LearnerClassifJ48 = R6Class("LearnerClassifJ48",
  inherit = LearnerClassif,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          ParamUty$new(id = "subset", tags = c("train", "pars")),
          ParamUty$new(id = "na.action", tags = c("train", "pars")),
          ParamLgl$new(id = "U", default = FALSE, tags = c("train", "control")),
          ParamLgl$new(id = "O", default = FALSE, tags = c("train", "control")),
          ParamDbl$new(
            id = "C", default = 0.25, lower = .Machine$double.eps,
            upper = 1 - .Machine$double.eps, tags = c("train", "control")),
          ParamInt$new(id = "M", default = 2L, lower = 1L, tags = c("train", "control")),
          ParamLgl$new(id = "R", default = FALSE, tags = c("train", "control")),
          ParamInt$new(id = "N", default = 3L, lower = 2L, tags = c("train", "control")),
          ParamLgl$new(id = "B", default = FALSE, tags = c("train", "control")),
          ParamLgl$new(id = "S", default = FALSE, tags = c("train", "control")),
          ParamLgl$new(id = "L", default = FALSE, tags = c("train", "control")),
          ParamLgl$new(id = "A", default = FALSE, tags = c("train", "control")),
          ParamLgl$new(id = "J", default = FALSE, tags = c("train", "control")),
          ParamInt$new(id = "Q", default = 1L, lower = 1L, tags = c("train", "control")),
          ParamLgl$new(
            id = "doNotMakeSplitPointActualValue", default = FALSE,
            tags = c("train", "control")),
          ParamLgl$new(id = "output_debug_info", default = FALSE, tags = c("train", "control")),
          ParamLgl$new(
            id = "do_not_check_capabilities", default = FALSE,
            tags = c("train", "control")),
          ParamInt$new(
            id = "num_decimal_places", default = 2L, lower = 1L,
            tags = c("train", "control")),
          ParamInt$new(id = "batch_size", default = 100L, lower = 1L, tags = c("train", "control")),
          ParamUty$new(id = "options", default = NULL, tags = c("train", "pars"))
        )
      )
      ps$add_dep("C", "U", CondEqual$new(FALSE))
      ps$add_dep("C", "R", CondEqual$new(FALSE))
      ps$add_dep("R", "U", CondEqual$new(FALSE))
      ps$add_dep("N", "U", CondEqual$new(FALSE))
      ps$add_dep("N", "R", CondEqual$new(TRUE))

      super$initialize(
        id = "classif.J48",
        packages = "RWeka",
        feature_types = c("numeric", "factor", "ordered"),
        predict_types = c("response", "prob"),
        param_set = ps,
        properties = c("twoclass", "multiclass", "missings"),
        man = "mlr3learners.rweka::mlr_learners_classif.J48"
      )
    }
  ),

  private = list(
    .train = function(task) {
      ctrl = self$param_set$get_values(tags = "control")
      if (length(ctrl) > 0L) {
        names(ctrl) = gsub("_", replacement = "-", x = names(ctrl))
        ctrl = mlr3misc::invoke(RWeka::Weka_control, ctrl)
      }

      pars = self$param_set$get_values(tags = "pars")
      f = task$formula()
      data = task$data()
      mlr3misc::invoke(RWeka::J48, formula = f, data = data, control = ctrl, .args = pars)
    },

    .predict = function(task) {
      response = NULL
      prob = NULL
      newdata = task$data(cols = task$feature_names)

      if (self$predict_type == "response") {
        response = mlr3misc::invoke(predict, self$model, newdata = newdata, type = "class")
      } else {
        prob = mlr3misc::invoke(predict, self$model, newdata = newdata, type = "prob")
      }
      PredictionClassif$new(task = task, response = response, prob = prob)
    }
  )
)
