library(shiny)
library(shinydashboard)
library(dplyr)

dat <- dplyr::tibble(
  cdm_name = c("A", "A", "B", "B"),
  variable = c("x", "y", "x", "y"),
  estimate = 1:4
)

uiSnapshot <- function(data,
                       id = "snapshot",
                       picker = c("cdm_name", "variable"),
                       raw = TRUE,
                       gt = TRUE) {
  checkmate::checkCharacter(id, min.chars = 1, any.missing = FALSE, len = 1)
  checkmate::checkChoice(filter, c("cdm_name", "variable"), null.ok = TRUE)
  x <- list()
  for (col in picker) {
    x <- c(x, list(selector(prefix = id, data = data, column = col)))
  }
  x <- c(x, list(DT::DTOutput(outputId = paste0(id, "_raw"))))
  return(x)
}
serverSnapshot <- function(output, input, data, id = "snapshot") {
  output[[paste0(id, "_raw")]] <- DT::renderDataTable({
    x <- filterData(data = data, id = id, cols = c("cdm_name", "variable"), input = input)
    DT::datatable(data = x)
  })
}

ui <- dashboardPage(
  dashboardHeader(title = "shiny generator"),
  # menu ----
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Databases snapshot",
        tabName = "snap"
      )
    )
  ),
  # snapshot ----
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "snap",
        uiSnapshot(data = dat)
      )
    )
  )
)

server <- function(input, output, session) {
  # snapshot
  output <- serverSnapshot(output = output, input = input, data = dat)
}

filterData <- function(data, id, cols, input) {
  x <- data
  for (col in cols) {
    nm <- paste0(id, "_", col)
    if (nm %in% names(input)) {
      x <- x %>% dplyr::filter(.data[[col]] %in% input[[nm]])
    }
  }
  return(x)
}
fancy <- function(x) {
  stringr::str_to_sentence(gsub("_", " ", x))
}
selector <- function(prefix, data, column) {
  shinyWidgets::pickerInput(
    inputId = paste0(prefix, "_", column),
    label = fancy(column),
    choices = unique(data[[column]]),
    selected = unique(data[[column]]),
    multiple = TRUE,
    options = list(`actions-box` = TRUE),
    inline = TRUE
  )
}



# Run the application
shinyApp(ui = ui, server = server)
