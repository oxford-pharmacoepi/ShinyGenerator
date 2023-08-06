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

#' Read OMOP CDM files from a path
#'
#' @param resultsPath Path that points to a folder with the result files.
#'
#' @return All elements that could be identified from the csv files.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ShinyGenerator)
#' readFiles(here::here("Results"))
#' }
readFiles <- function(resultsPath) {
  if (!dir.exists(resultsPath)) {
    cli::cli_abort(glue::glue("No directory found in: {resultsPath}"))
  }
  files <- list.files(resultsPath, full.names = TRUE)
  if (length(files) == 0) {
    cli::cli_abort(glue::glue("No files found in: {resultsPath}"))
  }
  files <- files[tools::file_ext(basename(files)) == "csv"]
  if (length(files) == 0) {
    cli::cli_abort(glue::glue("No csv files found in: {resultsPath}"))
  }
  results <- list()
  specifications <- dplyr::tibble(
    result_name = character(), result_type = character()
  )
  for (k in seq_along(files)) {
    nam <- tools::file_path_sans_ext(basename(files[k]))
    content <- readr::read_csv(
      files[k], col_types = readr::cols(.default = "c")
    )
    type <- getFileType(content)
    if (is.na(type)) {
      cli::cli_warn(glue::glue("No type idenfified in file: {files[k]}"))
    } else {
      results[[nam]] <- content
      specifications <- specifications %>%
        dplyr::union_all(dplyr::tibble(result_name = nam, result_type = type))
    }
  }
  results <- newResultsCollection(results, specifications)
  return(results)
}

getFileType <- function(content) {
  if (!("result_type" %in% colnames(content))) {
    x <- as.character(NA)
  } else {
    x <- unique(content$result_type)
    if (length(x) > 1) {
      x <- as.character(NA)
    }
  }
  return(x)
}
