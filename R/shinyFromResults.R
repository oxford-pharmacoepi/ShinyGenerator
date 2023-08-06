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
  # initial checks
  if (!dir.exists(shinyPath)) {
    cli::cli_abort(glue::glue("Path for the shiny don't exist: {shinyPath}"))
  }

  # read files
  results <- readFiles(resultsPath)

  # create shiny
  shiny <- writeShiny(results)

  # create project
  proj <- createEmptyProject()

  # export zip file
  createZipFile(shinyPath, zipFile, shiny, proj, results)

  # Final message
  cli::cat_line(glue::glue(
    "App succesfully created in: {shinyPath}/{zipFile}.zip"
  ))

  return(invisible(shinyPath))
}

writeShiny <- function(results) {
  moduleName <- getModuleName(results)
  modules <- listModules(moduleName)
  shiny <- shinyTemplate
  shiny <- gsub("#PACKAGES#", combinePackages(modules), shiny)
  shiny <- gsub("#READ#", combineRead(modules), shiny)
  shiny <- gsub("#MENU#", combineMenu(modules), shiny)
  shiny <- gsub("#BODY#", combineBody(modules), shiny)
  shiny <- gsub("#SERVER#", combineServer(modules), shiny)
  shiny <- styler::style_text(shiny)
  return(shiny)
}
getModuleName <- function(results) {
  type <- attr(results, "specifications") %>%
    dplyr::pull("result_type") %>%
    unique()
  moduleName <- relation %>%
    dplyr::filter(.data$result_type %in% .env$type) %>%
    dplyr::pull("module_name")
  return(moduleName)
}
listModules <- function(moduleName) {
  result <- list()
  result$moduleBackground <- moduleBackground
  for (module in moduleName) {
    result[[module]] <- eval(parse(text = module))
  }
  return(result)
}
combinePackages <- function(modules) {
  packages <- lapply(modules, function(x) {
    x[["packages"]]
  }) %>%
    unlist() %>%
    unique()
  paste0("library(\"", packages[packages != ""],"\")", collapse = "\n")
}
combineRead <- function(modules) {
  elements <- lapply(modules, function(x) {
    x[["read"]]
  }) %>%
    unlist() %>%
    unique()
  if (length(elements) > 0) {
    elements <- paste0(
      CDMUtilities::toCamelCase(elements), " <- read_csv(here(\"data\", \"",
      elements, ".csv\"))", collapse = "\n"
    )
  } else {
    elements <- ""
  }
  return(elements)
}
combineMenu <- function(modules) {
  menuItems <- lapply(modules, function(x) {
    x[["menu"]]
  }) %>%
    dplyr::bind_rows()
  lapply(unique(menuItems$item), function(item) {
    subItems <- menuItems %>%
      dplyr::filter(.data$item == .env$item) %>%
      dplyr::pull("sub_item")
    if (any(subItems != "")) {
      subItems <- paste0(
        "menuSubItem(\n", writeTabName(subItems),"\n)"
      ) %>%
        paste0(collapse = ",\n")
      subItems <- paste0(",\n", subItems)
    }
    paste0("menuItem(\n", writeTabName(item), subItems, "\n)")
  }) %>%
    paste0(collapse = ",\n")
}
writeTabName <- function(name) {
  paste0(
    "text = \"", name, "\",\ntabName = \"", CDMUtilities::toSnakeCase(name),
    "\""
  )
}
combineBody <- function(modules) {
  body <- lapply(modules, function(x) {
    body <- x[["body"]]
    lapply(names(body), function(x) {
      name <- CDMUtilities::toSnakeCase(x)
      paste0(
        "# ", name, " ----\ntabItem(\ntabName = \"", name, "\",\n", body[[x]],
        "\n)"
      )
    }) %>%
      unlist() %>%
      paste0(collapse = ",\n")
  }) %>%
    unlist()
  return(paste0(body[body != ""], collapse = ",\n"))
}
combineServer <- function(modules) {
  ""
}
createEmptyProject <- function() {
  c(
    "Version: 1.0", "", "RestoreWorkspace: Default", "SaveWorkspace: Default",
    "AlwaysSaveHistory: Default", "", "EnableCodeIndexing: Yes",
    "UseSpacesForTab: Yes", "NumSpacesForTab: 2","Encoding: UTF-8", "",
    "RnwWeave: Sweave", "LaTeX: pdfLaTeX"
  ) %>%
    paste0(collapse = "\n")
}
createZipFile <- function(shinyPath, zipFile, shiny, proj, results) {
  # temporal directory
  tempDir <- tempdir()

  # create temp data folder
  dataPath <- file.path(tempDir, "data")
  if (!dir.exists(dataPath)) {
    dir.create(dataPath, recursive = TRUE)
  }

  # export bound results
  types <- attr(results, "specifications") %>%
    dplyr::pull("result_type") %>%
    unique()
  for (type in types) {
    nam <- attr(results, "specifications") %>%
      dplyr::filter(.data$result_type == .env$type) %>%
      dplyr::pull("result_name")
    file <- file.path(tempDir, "data",  paste0(type, ".csv"))
    readr::write_csv(
      dplyr::bind_rows(results[nam]), file = file, progress = FALSE
    )
    files <- c(files, file)
  }

  # export Rproject
  fileRproj <- file.path(tempDir, paste0(zipFile, ".Rproj"))
  writeLines(proj, fileRproj)

  # export shiny
  fileApp <- file.path(tempDir, "app.R")
  writeLines(shiny, fileApp)

  # create zip file
  zip::zipr(
    zipfile = file.path(paste0(shinyPath, "/", zipFile, ".zip")),
    files = c(dataPath, fileApp, fileRproj)
  )

  # eliminate temp files
  unlink(tempDir)

  return(invisible(TRUE))
}
