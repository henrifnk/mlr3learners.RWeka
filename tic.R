# installs dependencies, runs R CMD check, runs covr::codecov()
do_package_checks()

do_drat("mlr3learners/mlr3learners.drat")

if (ci_is_env("CODECOV", "true")) {
  get_stage("after_success") %>%
    add_code_step(RWeka::WPM("refresh-cache")) %>%
#    add_code_step(RWeka::WPM("install-package", "XMeans")) %>%
    add_code_step(covr::codecov())
}
