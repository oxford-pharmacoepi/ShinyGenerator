# This file contains the information for the prevalence module
style <- "\"display: inline-block;vertical-align:top; width: 150px;\""
option <- "list(`actions-box` = TRUE, size = 10, `selected-text-format` = \"count > 3\")"
selector <- function(id, lab) {
  paste0(
    "div(
      style = \"display: inline-block;vertical-align:top; width: 150px;\",
      pickerInput(
        inputId = \"prevalence_estimates_", id,"\",
        label = \"", lab,"\",
        choices = unique(prevalenceEstimates$", id,"),
        selected = unique(prevalenceEstimates$", id,"),
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
  "denominator_end_date", "analysis_type", "analysis_outcome_lookback_days",
  "analysis_time_point", "analysis_complete_database_intervals",
  "analysis_full_contribution", "analysis_min_cell_count", "analysis_interval",
  "prevalence_start_date"
)
{
bodyPrevalence <- c(
  "h3(\"Prevalence estimates\")",
  "p(\"Prevalence estimates are shown below, please select configuration to filter them:\")",
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
  selector("analysis_type", "Prevalence type"),
  selector("analysis_outcome_lookback_days", "Lookback days"),
  selector("analysis_time_point", "Time point"),
  selector("analysis_complete_database_intervals", "Complete period"),
  selector("analysis_full_contribution", "Full contribution"),
  selector("analysis_min_cell_count", "Minimum counts"),
  "p(\"Dates\")",
  selector("analysis_interval", "Interval"),
  selector("prevalence_start_date", "Prevalence start date"),
  paste0(
    "tabsetPanel(
      type = \"tabs\",
      tabPanel(
        \"Table of estimates\",
        downloadButton(\"prevalence_estimates_download_table\", \"Download current estimates\"),
        DTOutput(\"prevalence_estimates_table\") %>% withSpinner()
      ),
      tabPanel(
        \"Plot of estimates\",
        p(\"Plotting options\"),
        div(
          style = #STYLE#,
          pickerInput(
            inputId = \"prevalence_estimates_plot_x\",
            label = \"x axis\",
            choices = c(\"", paste0(variables, collapse = "\", \""),"\"),
            selected = \"prevalence_start_date\",
            #OPTION#,
            multiple = FALSE
          )
        ),
        div(
          style = #STYLE#,
          pickerInput(
            inputId = \"prevalence_estimates_plot_facet\",
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
            inputId = \"prevalence_estimates_plot_colour\",
            label = \"Colour by\",
            choices = c(\"", paste0(variables, collapse = "\", \""),"\"),
            #OPTION#,
            multiple = TRUE
          )
        ),
        plotlyOutput(
          \"prevalence_estimates_plot\",
          height = \"800px\"
        ) %>%
          withSpinner(),
                h4(\"Download figure\"),
        div(\"height:\", style = \"display: inline-block; font-weight: bold; margin-right: 5px;\"),
        div(
          style = \"display: inline-block;\",
          textInput(\"prevalence_estimates_download_height\", \"\", 10, width = \"50px\")
        ),
        div(\"cm\", style = \"display: inline-block; margin-right: 25px;\"),
        div(\"width:\", style = \"display: inline-block; font-weight: bold; margin-right: 5px;\"),
        div(
          style = \"display: inline-block;\",
          textInput(\"prevalence_estimates_download_width\", \"\", 20, width = \"50px\")
        ),
        div(\"cm\", style = \"display: inline-block; margin-right: 25px;\"),
        div(\"dpi:\", style = \"display: inline-block; font-weight: bold; margin-right: 5px;\"),
        div(
          style = \"display: inline-block; margin-right:\",
          textInput(\"prevalence_estimates_download_dpi\", \"\", 300, width = \"50px\")
        ),
        downloadButton(\"prevalence_estimates_download_plot\", \"Download plot\")
      )
    )"
  )
)
}
bodyPrevalence <- gsub("#STYLE#", style, bodyPrevalence)
bodyPrevalence <- gsub("#OPTION#", option, bodyPrevalence)
bodyPrevalence <- paste0(bodyPrevalence, collapse = ",\n")
{
  filterPrevalenceEstimates <- paste0(
    "filter(", variables, " %in% input$prevalence_estimates_", variables, ")",
    collapse = " %>%\n"
  )
serverPrevalence <- c(
  "### get estimates ----",
  paste0(
    "getPrevalenceEstimates <- reactive({
      prevalenceEstimates %>%\n",
    filterPrevalenceEstimates,
    "%>%
    mutate(
      n_cases = round(suppressWarnings(as.numeric(n_cases))),
      n_population = round(suppressWarnings(as.numeric(n_population))),
      prevalence = round(suppressWarnings(as.numeric(prevalence)), 4),
      prevalence_95CI_lower = round(suppressWarnings(as.numeric(prevalence_95CI_lower)), 4),
      prevalence_95CI_upper = round(suppressWarnings(as.numeric(prevalence_95CI_upper)), 4)
    )\n})"
  ),
  "### download table ----",
  "output$prevalence_estimates_download_table <- downloadHandler(
    filename = function() {
      \"prevalenceEstimatesTable.csv\"
    },
    content = function(file) {
      write_csv(getPrevalenceEstimates(), file)
    }
  )",
  "### table estimates ----",
  paste0(
    "output$prevalence_estimates_table <- renderDataTable({
      table <- getPrevalenceEstimates()
      validate(need(nrow(table) > 0, \"No results for selected inputs\"))
      table <- table %>%
        mutate(`prevalence (%)` = paste0(
          100*prevalence, \" (\", 100*prevalence_95CI_lower,\" to \",
          100*prevalence_95CI_upper, \" )\"
        )) %>%
        select(", paste0(variables, collapse = ", "), ", n_cases, n_population, \"prevalence (%)\")
      datatable(
        table,
        rownames= FALSE,
        extensions = 'Buttons',
        options = list(scrollX = TRUE, scrollCollapse = TRUE)
      )
    })"
  ),
  "### make plot ----",
  "plotPrevalenceEstimates <- reactive({
    table <- getPrevalenceEstimates()
    validate(need(nrow(table) > 0, \"No results for selected inputs\"))
    class(table) <- c(\"PrevalenceResult\", \"IncidencePrevalenceResult\", class(table))
    plotPrevalence(
      table,
      x = input$prevalence_estimates_plot_x,
      ylim = c(0, NA),
      ribbon = TRUE,
      facet = input$prevalence_estimates_plot_facet,
      colour = input$prevalence_estimates_plot_colour,
      colour_name = paste0(input$prevalence_estimates_plot_colour, collapse = \"; \")
    )
  })",
  "### download plot ----",
  "output$prevalence_estimates_download_plot <- downloadHandler(
    filename = function() {
      \"prevalenceEstimatesPlot.png\"
    },
    content = function(file) {
      ggsave(
        file,
        plotPrevalenceEstimates(),
        width = as.numeric(input$prevalence_estimates_download_width),
        height = as.numeric(input$prevalence_estimates_download_height),
        dpi = as.numeric(input$prevalence_estimates_download_dpi),
        units = \"cm\"
      )
    }
  )",
  "### plot ----",
  "output$prevalence_estimates_plot <- renderPlotly({
    plotPrevalenceEstimates()
  })"
) %>%
  paste0(collapse = "\n")
}
modulePrevalence <- list(
  packages = c(
    "readr", "here", "shinyWidgets", "DT", "plotly", "shinycssloaders",
    "IncidencePrevalence", "ggplot2"
  ),
  read = c("Prevalence estimates"),
  menu = dplyr::tibble(
    item = c("Prevalence"), sub_item = c("Prevalence estimates")
  ),
  body = list("Prevalence estimates" = bodyPrevalence),
  server = list("Prevalence estimates" = serverPrevalence)
)
