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

#' Create a ShinyApp with the specified results.
#'
#' @param resultsPath Path that points to a folder with the result files.
#' @param shinyPath Path where the shiny will be saved.
#' @param zipFile Name of the shiny app and the zip file.
#'
#' @export
#'
shinyFromResults <- function(resultsPath,
                             shinyPath = here::here(),
                             zipFile = "shiny") {
  if (dir.exists(shinyPath)) {
    cli::cli_abort(glue::glue("Path for the shiny don't exist: {shinyPath}"))
  }
  results <- readFiles(resultsPath)
  type <- attr(results, "specifications") %>%
    dplyr::pull("result_type") %>%
    unique()
  modules <- getModules(type)
  shiny <- writeShiny(modules)
  files <- exportFiles(results, modules)
  proj <- createProject(zipFile)
  files <- c(shiny, files, proj)
  zip::zipr(
    zipfile = file.path(paste0(shinyPath, "/", zipFile, ".zip")),
    files = files
  )
  file.remove(files)
  return(invisible(shinyPath))
}

getModules <- function(type) {
  type
}
createProject <- function(zipFile) {
  x <- "Version: 1.0\n\nRestoreWorkspace: Default\nSaveWorkspace: Default\n
  AlwaysSaveHistory: Default\n\nEnableCodeIndexing: Yes\nUseSpacesForTab: Yes\n
  NumSpacesForTab: 2\nEncoding: UTF-8\n\nRnwWeave: Sweave\nLaTeX: pdfLaTeX"
  file <- tempfile(zipFile, fileext = ".Rproj")
  writeLines(x, con = file)
  return(file)
}
writeShiny <- function(modules) {
  modules <- listModules(modules)
  shiny <- shinyTemplate
  shiny <- gsub("#PACKAGES#", combinePackages(modules), shiny)
  shiny <- gsub("#LOAD#", combineLoad(modules), shiny)
  shiny <- gsub("#MENU#", combineMenu(modules), shiny)
  shiny <- gsub("#BODY#", combineBody(modules), shiny)
  shiny <- gsub("#SERVER#", combineServer(modules), shiny)
  return(shiny)
}
listModules <- function(modules) {
  result <- list()
  for (module in modules) {
    result[[module]] <- eval(parse(text = paste0(
      "module", stringr::str_to_title(module)
    )))
  }
  return(result)
}
combinePackages <- function(modules) {
  lapply(modules, function(x) {
    paste0('library("', x[["packages"]], '")')
  }) %>%
    unlist() %>%
    unique() %>%
    paste0(collapse = "\n")
}
combineLoad <- function(modules) {
  ""
}
combineMenu <- function(modules) {
  ""
}
combineBody <- function(modules) {
  ""
}
combineServer <- function(modules) {
  ""
}
exportFiles <- function(results, modules) {

}
