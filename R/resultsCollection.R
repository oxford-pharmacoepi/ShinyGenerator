# Copyright 2023 OXINFER
#
# This file is part of ShinyGenerator
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Create a results_collection object
#'
#' @param resultsList List of results.
#' @param types Specification of the result class.
#'
#' @return A results_collection object
#'
#' @export
#'
newResultsCollection <- function(resultsList, types) {
  class(resultsList) <- "results_collection"
  attr(resultsList, "specifications") <- types
  return(resultsList)
}

#' Print a results_collection object
#'
#' @param x A results_collection object
#' @param ... Included for compatibility with generic. Not used.
#'
#' @return Invisibly returns the input
#' @export
print.results_collection <- function(x, ...) {
  len <- length(x)
  if (len == 0) {
    cli::cat_line(glue::glue("# Empty collection of results."))
  } else {
    cli::cat_line(glue::glue("# Collection of {len} results:"))
    cli::cat_line("")
    cli::cat_bullet(messageFromType(attr(x, "specifications")))
  }
  invisible(x)
}

messageFromType <- function(type) {
  x <- sort(unique(x$result_type))
  message <- NULL
  for (k in x) {
    message <- c(
      message,
      type %>%
        dplyr::filter(.data$result_type == .env$k) %>%
        dplyr::pull("result_name") %>%
        sort() %>%
        paste0(collapse = ", ")
    )
  }
  return(message)
}
