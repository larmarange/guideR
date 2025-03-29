# guideR 0.3.0

**New features**

* `plot_proportions()` now accepts several conditions (#18)
* `stratified_by()` helper for `plot_proportions()` (#19)
* new function `cut_quartiles()` (#13)
* new argument `convert_continuous` for `plot_proportions()` (#14)
* new argument `.drop_by_na` for `proportion()` (#12)
* new argument `drop_by_na` for `plot_proportions()` (#16)

**Bug fixes**

* fix in `proportion()` when `.conf.int = TRUE` and some rows have no
  observation (N = 0) (#15)

# guideR 0.2.0

**New features**

* `proportion()` could be applied to atomic vectors (#4)
* new function `periods_to_long()` (#7)
* new function `plot_proportions()` (#9)

# guideR 0.1.0

* initial CRAN submission.
