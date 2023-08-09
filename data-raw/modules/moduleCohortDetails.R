# This file contains the information for the incidence module
style <- "\"display: inline-block;vertical-align:top; width: 150px;\""
option <- "list(`actions-box` = TRUE, size = 10, `selected-text-format` = \"count > 3\")"
selector <- function(id, lab, multiple) {
  paste0(
    "div(
      style = \"display: inline-block;vertical-align:top; width: 150px;\",
      pickerInput(
        inputId = \"cohort_count_", id, "\",
        label = \"", lab, "\",
        choices = unique(cohortDetails$", id, "),
        selected = unique(cohortDetails$", id, "),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = \"count > 3\"),
        multiple = ", multiple, "
      )
    )"
  )
}
bodyCohortCount <- c(
  "h3(\"Cohort counts\")",
  "p(\"Cohort counts for each cohort of the present study\")",
  selector("cohort_table_name", "Cohort table name", "TRUE"),
  "div(
      style = \"display: inline-block;vertical-align:top; width: 150px;\",
      pickerInput(
        inputId = \"cohort_count_count\",
        label = \"Count type\",
        choices = c(\"Number records\", \"Number subjects\"),
        selected = \"Number records\",
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = \"count > 3\"),
        multiple = FALSE
      )
    )",
  selector("cdm_name", "CDM name", "TRUE"),
  "downloadButton(\"cohort_count_download_table\", \"Download current estimates\")",
  "DTOutput(\"cohort_count_table\") %>% withSpinner()"
)
bodyCohortCount <- paste0(bodyCohortCount, collapse = ",\n")
serverCohortCount <- c(
  "### get counts ----",
  "getCohortCount <- reactive({
    cohortDetails %>%
      mutate(reason_id = as.numeric(reason_id)) %>%
      filter(cdm_name %in% input$cohort_count_cdm_name) %>%
      filter(cohort_table_name %in% input$cohort_count_cohort_table_name) %>%
      group_by(cohort_name, cohort_table_name) %>%
      filter(reason_id == max(reason_id)) %>%
      ungroup() %>%
      select(cohort_table_name, cohort_name, cdm_name, value = !!toSnakeCase(input$cohort_count_count)) %>%
      mutate(value = suppressWarnings(as.numeric(value))) %>%
      pivot_wider(names_from = cdm_name, values_from = value)
  })",
  "### download table ----",
  "output$cohort_count_download_table <- downloadHandler(
    filename = function() {
      \"cohortCountTable.csv\"
    },
    content = function(file) {
      write_csv(getCohortCount(), file)
    }
  )",
  "### table count ----",
  "output$cohort_count_table <- renderDataTable({
    table <- getCohortCount()
    validate(need(nrow(table) > 0, \"No results for selected inputs\"))
    datatable(
      table,
      rownames= FALSE,
      extensions = 'Buttons',
      options = list(scrollX = TRUE, scrollCollapse = TRUE)
    )
  })"
) %>%
  paste0(collapse = "\n")

moduleCohortDetails <- list(
  packages = c(
    "readr", "here", "shinyWidgets", "DT", "tidyr", "CDMUtilities"
  ),
  read = c("Cohort details"),
  menu = dplyr::tibble(
    item = c("Cohort details"), sub_item = c("Cohort count")
  ),
  body = list("Cohort count" = bodyCohortCount),
  server = list("Cohort count" = serverCohortCount)
)
