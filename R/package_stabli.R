#' @eval c(
#'   codedoc::codedoc_R_package_description("stabli"),
#'   codedoc::codedoc_news_for_R_package()
#' )
#' @keywords internal
"_PACKAGE"

# @codedoc_comment_block R_package_description(stabli)
# <!-- badges: start -->
# [![R-CMD-check](https://github.com/FinnishCancerRegistry/stabli/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/FinnishCancerRegistry/stabli/actions/workflows/R-CMD-check.yaml)
# <!-- badges: end -->
#
# ## Description
#
# ${paste0(read.dcf("DESCRIPTION", fields = "Description"), collapse = "\n")}
#
# ## Installation
#
# ```r
# devtools::install_github("FinnishCancerRegistry/stabli@release")
# ```
#
# ## Example
#
# ```r
# @codedoc_insert_comment_block R_package_example(stabli)
# ```
#
# ## Releases
#
# @codedoc_insert_comment_block R_package_releases(stabli)
# @codedoc_comment_block R_package_description(stabli)

# @codedoc_comment_block news("stabli", "2025-07-04", "0.2.0")
# First release.
# @codedoc_comment_block news("stabli", "2025-07-04", "0.2.0")
