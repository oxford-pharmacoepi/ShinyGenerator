# This file contains the information for the incidence module
bodyCdmSnapshot <- c(
  "h3(\"Database details\")",
  "p(\"See details of CDM snapshot for each database:\")",
  "DTOutput(\"cdm_snapshot_table\")"
) %>%
  paste0(collapse = ",\n")
serverCdmSnapshot <-
  "output$cdm_snapshot_table <- renderDataTable({
    datatable(
      cdmSnapshot %>% select(-result_type),
      rownames= FALSE,
      extensions = 'Buttons',
      options = list(scrollX = TRUE, scrollCollapse = TRUE)
    )
  })"

moduleSnapshot <- list(
  packages = c(
    "readr", "here", "DT", "CDMUtilities"
  ),
  read = c("CDM snapshot"),
  menu = dplyr::tibble(
    item = c("Database details"),
    sub_item = c("CDM snapshot")
  ),
  body = list("CDM snapshot" = bodyCdmSnapshot),
  server = list("CDM snapshot" = serverCdmSnapshot)
)
