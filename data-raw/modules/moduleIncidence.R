# This file contains the information for the incidence module
style <- "\"display: inline-block;vertical-align:top; width: 150px;\""
option <- "list(`actions-box` = TRUE, size = 10, `selected-text-format` = \"count > 3\")"
selector <- function(id, lab) {
  paste0(
    "div(
      style = \"display: inline-block;vertical-align:top; width: 150px;\",
      pickerInput(
        inputId = \"incidence_estimates_", id,"\",
        label = \"", lab,"\",
        choices = unique(incidenceEstimates$", id,"),
        selected = unique(incidenceEstimates$", id,"),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = \"count > 3\"),
        multiple = TRUE
      )
    )"
  )
}
variables <- c(
  "cdm_name", "outcome_cohort_name", "denominator_strata_cohort_name",
  "denominator_age_group", "denominator_sex",
  "denominator_days_prior_history", "denominator_start_date",
  "denominator_end_date", "analysis_outcome_washout",
  "analysis_repeated_events", "analysis_complete_database_intervals",
  "analysis_min_cell_count", "analysis_interval", "incidence_start_date"
)
{
bodyIncidence <- c(
  "h3(\"Incidence estimates\")",
  "p(\"Incidence estimates are shown below, please select configuration to filter them:\")",
  "p(\"Database and study outcome\")",
  selector("cdm_name", "CDM name"),
  selector("outcome_cohort_name", "Outcome name"),
  "p(\"Denominator population settings\")",
  selector("denominator_strata_cohort_name", "Strata"),
  selector("denominator_age_group", "Age group"),
  selector("denominator_sex", "Sex"),
  selector("denominator_days_prior_history", "Days prior observation"),
  selector("denominator_start_date", "Start date"),
  selector("denominator_end_date", "End date"),
  "p(\"Analysis settings\")",
  selector("analysis_outcome_washout", "Outcome washout"),
  selector("analysis_repeated_events", "Repeated events"),
  selector("analysis_complete_database_intervals", "Complete period"),
  selector("analysis_min_cell_count", "Minimum counts"),
  "p(\"Dates\")",
  selector("analysis_interval", "Intervals"),
  selector("incidence_start_date", "Incidence start date"),
  paste0(
    "tabsetPanel(
      type = \"tabs\",
      tabPanel(
        \"Table of estimates\",
        downloadButton(\"incidence_estimates_download_table\", \"Download current estimates\"),
        DTOutput(\"incidence_estimates_table\") %>% withSpinner()
      ),
      tabPanel(
        \"Plot of estimates\",
        p(\"Plotting options\"),
        div(
          style = #STYLE#,
          pickerInput(
            inputId = \"incidence_estimates_plot_x\",
            label = \"x axis\",
            choices = c(\"", paste0(variables, collapse = "\", \""),"\"),
            selected = \"incidence_start_date\",
            #OPTION#,
            multiple = FALSE
          )
        ),
        div(
          style = #STYLE#,
          pickerInput(
            inputId = \"incidence_estimates_plot_facet\",
            label = \"Facet by\",
            choices = c(\"", paste0(variables, collapse = "\", \""),"\"),
            selected = c(\"outcome_cohort_name\", \"cdm_name\"),
            #OPTION#,
            multiple = TRUE
          )
        ),
        div(
          style = #STYLE#,
          pickerInput(
            inputId = \"incidence_estimates_plot_colour\",
            label = \"Colour by\",
            choices = c(\"", paste0(variables, collapse = "\", \""),"\"),
            #OPTION#,
            multiple = TRUE
          )
        ),
        downloadButton(\"incidence_estimates_download_plot\", \"Download plot\"),
        plotlyOutput(
          \"incidence_estimates_plot\",
          height = \"800px\"
        ) %>%
          withSpinner()
      )
    )"
  )
)
}
bodyIncidence <- gsub("#STYLE#", style, bodyIncidence)
bodyIncidence <- gsub("#OPTION#", option, bodyIncidence)
bodyIncidence <- paste0(bodyIncidence, collapse = ",\n")
{
  filterIncidenceEstimates <- paste0(
    "filter(", variables, " %in% input$incidence_estimates_", variables, ")",
    collapse = " %>%\n"
  )
serverIncidence <- c(
  "### get estimates ----",
  paste0(
    "getIncidenceEstimates <- reactive({
      incidenceEstimates %>%\n",
    filterIncidenceEstimates,
    "%>%
    mutate(
      person_years = round(suppressWarnings(as.numeric(person_years))),
      person_days = round(suppressWarnings(as.numeric(person_days))),
      n_events = round(suppressWarnings(as.numeric(n_events))),
      incidence_100000_pys = round(suppressWarnings(as.numeric(incidence_100000_pys))),
      incidence_100000_pys_95CI_lower = round(suppressWarnings(as.numeric(incidence_100000_pys_95CI_lower))),
      incidence_100000_pys_95CI_upper = round(suppressWarnings(as.numeric(incidence_100000_pys_95CI_upper)))
    )\n})"
  ),
  "### download table ----",
  "output$incidence_estimates_download_table <- downloadHandler(
    filename = function() {
      \"incidenceEstimatesTable.csv\"
    },
    content = function(file) {
      write_csv(getIncidenceEstimates(), file)
    }
  )",
  "### table estimates ----",
  paste0(
    "output$incidence_estimates_table <- renderDataTable({
      table <- getIncidenceEstimates()
      validate(need(nrow(table) > 0, \"No results for selected inputs\"))
      table <- table %>%
        mutate(incidence_100000_pys = paste0(
          incidence_100000_pys, \" (\", incidence_100000_pys_95CI_lower,\" to \",
          incidence_100000_pys_95CI_upper, \" )\"
        )) %>%
        select(", paste0(variables, collapse = ", "), ", n_events, n_persons, person_years, incidence_100000_pys)
      datatable(
        table,
        rownames= FALSE,
        extensions = 'Buttons',
        options = list(scrollX = TRUE, scrollCollapse = TRUE)
      )
    })"
  ),
  "### make plot ----",
  "plotIncidenceEstimates <- reactive({
    table <- getIncidenceEstimates()
    validate(need(nrow(table) > 0, \"No results for selected inputs\"))
    class(table) <- c(\"IncidenceResult\", \"IncidencePrevalenceResult\", class(table))
    plotIncidence(
      table,
      x = input$incidence_estimates_plot_x,
      ylim = c(0, NA),
      ribbon = TRUE,
      facet = input$incidence_estimates_plot_facet,
      colour = input$incidence_estimates_plot_colour,
      colour_name = paste0(input$incidence_estimates_plot_colour, collapse = \"; \")
    )
  })",
  "### download plot ----",
  "output$incidence_estimates_download_table <- downloadHandler(
    filename = function() {
      \"incidenceEstimatesPlot.png\"
    },
    content = function(file) {
      ggsave(file, plotIncidenceEstimates())
    }
  )",
  "### plot ----",
  "output$incidence_estimates_plot <- renderPlotly({
    plotIncidenceEstimates()
  })"
) %>%
  paste0(collapse = "\n")
}
moduleIncidence <- list(
  packages = c(
    "readr", "here", "shinyWidgets", "DT", "plotly", "shinycssloaders",
    "IncidencePrevalence", "ggplot2"
  ),
  read = c("Incidence estimates"),
  menu = dplyr::tibble(
    item = c("Incidence"), sub_item = c("Incidence estimates")
  ),
  body = list("Incidence estimates" = bodyIncidence),
  server = list("Incidence estimates" = serverIncidence)
)
