#' @title Classification IBk Learner
#'
#' @name mlr_learners_classif.IBk
#'
#' @description
#' A [mlr3::LearnerClassif] implementing classification IBk from package \CRANpkg{RWeka}.
#' Calls [RWeka::IBk()].
#'
#' @section Custom mlr3 defaults:
#' - `output_debug_info`:
#'   - original id: output-debug-info
#'
#' - `do_not_check_capabilities`:
#'   - actual id: do-not-check-capabilities
#'
#' - `num_decimal_places`:
#'   - actual id: num-decimal-places
#' - `batch_size`
#'   - actual id: batch-size
#'
#' - Reason for change: This learner contains changed ids of the following control agruments
#' since their ids contain irregular pattern
#'
#' @templateVar id classif.IBk
#' @template section_dictionary_learner
#'
#' @references
#' Aha D, Kibbler D, Alber M (1991).
#' Instance-based learning algorithms
#' \url{https://link.springer.com/content/pdf/10.1007/BF00153759.pdf}
#'
#' @export
LearnerClassifIBk = R6Class("LearnerClassifIBk",
  inherit = LearnerClassif,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          ParamUty$new(id = "subset", tags = c("train", "pars")),
          ParamUty$new(id = "na.action", tags = c("train", "pars")),
          ParamLgl$new(id = "I", default = FALSE, tags = c("train", "control")),
          ParamLgl$new(id = "F", default = FALSE, tags = c("train", "control")),
          ParamInt$new(id = "K", default = 1L, lower = 1L, tags = c("train", "control")),
          ParamLgl$new(id = "E", default = FALSE, tags = c("train", "control")),
          ParamInt$new(id = "W", default = 0L, lower = 0L, tags = c("train", "control")),
          ParamLgl$new(id = "X", default = FALSE, tags = c("train", "control")),
          ParamUty$new(id = "A", default = "weka.core.neighboursearch.LinearNNSearch",
            tags = c("train", "control")),
          ParamLgl$new(id = "output_debug_info", default = FALSE, tags = c("train", "control")),
          ParamLgl$new(id = "do_not_check_capabilities", default = FALSE,
            tags = c("train", "control")),
          ParamInt$new(id = "num_decimal_places", default = 2L, lower = 1L,
            tags = c("train", "control")),
          ParamInt$new(id = "batch_size", default = 100L, lower = 1L, tags = c("train", "control")),
          ParamUty$new(id = "options", default = NULL, tags = c("train", "pars"))
        )
      )

      super$initialize(
        id = "classif.IBk",
        packages = "RWeka",
        feature_types = c("numeric", "factor", "ordered"),
        predict_types = c("response", "prob"),
        param_set = ps,
        properties = c("twoclass", "multiclass"),
        man = "mlr3learners.rweka::mlr_learners_classif.IBk"
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
      mlr3misc::invoke(RWeka::IBk, formula = f, data = data, control = ctrl, .args = pars)
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
