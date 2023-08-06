# This file contains the information for the incidence module
style <- "\"display: inline-block;vertical-align:top; width: 150px;\""
option <- "options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = \"count > 3\")"
{
bodyIncidence <- c(
  "h3(\"Incidence estimates\")",
  "h5(\"Incidence estimates are shown below....\")",
  "hr()",
  "h5(\"Database and study outcome\")",
  "div(
    style = #STYLE#,
    pickerInput(
      inputId = \"incidence_estimates_cdm_name\",
      label = \"CDM name\",
      choices = unique(incidenceEstimates$cdm_name),
      selected = unique(incidenceEstimates$cdm_name),
      #OPTION#,
      multiple = TRUE
    )
  )",
  "div(
    style = #STYLE#,
    pickerInput(
      inputId = \"incidence_estimates_outcome_name\",
      label = \"Outcome name\",
      choices = sort(unique(incidenceEstimates$outcome_cohort_name)),
      selected = sort(unique(incidenceEstimates$outcome_cohort_name))[1],
      #OPTION#,
      multiple = TRUE
    )
  )",
  "hr()",
  "h5(\"Population settings\")",
  "div(
    style = #STYLE#,
    pickerInput(
      inputId = \"incidence_estimates_denominator_age_group\",
      label = \"Age group\",
      choices = unique(incidenceEstimates$denominator_age_group),
      selected = unique(incidenceEstimates$denominator_age_group),
      #OPTION#,
      multiple = TRUE
    )
  )",
  "div(
    style = #STYLE#,
    pickerInput(
      inputId = \"incidence_estimates_denominator_sex\",
      label = \"Sex\",
      choices = unique(incidenceEstimates$denominator_sex),
      selected = \"Both\",
      #OPTION#,
      multiple = TRUE
    )
  )",
  "div(
    style = #STYLE#,
    pickerInput(
      inputId = \"incidence_estimates_denominator_days_prior_history\",
      label = \"Days prior observation\",
      choices = unique(incidenceEstimates$denominator_days_prior_history),
      selected = unique(incidenceEstimates$denominator_days_prior_history)[1],
      #OPTION#,
      multiple = TRUE
    )
  )",
  "hr()",
  "h5(\"Analysis settings\")",
  "div(
    style = #STYLE#,
    pickerInput(
      inputId = \"incidence_estimates_start_date\",
      label = \"Incidence start date\",
      choices = unique(incidenceEstimates$incidence_start_date),
      selected = unique(incidenceEstimates$incidence_start_date),
      #OPTION#,
      multiple = TRUE
    )
  )",
  "tabsetPanel(
    type = \"tabs\",
    tabPanel(
      \"Table of estimates\",
      DTOutput(\"tbl_incidence_estimates\") %>% withSpinner()
    ),
    tabPanel(
      \"Plot of estimates\",
      hr(),
      h5(\"Plotting options\"),
      div(
        style = #STYLE#,
        pickerInput(
          inputId = \"incidence_x_axis\",
          label = \"x axis\",
          choices = c(
            \"denominator_age_group\",
            \"denominator_sex\",
            \"denominator_days_prior_history\",
            \"outcome_cohort_name\",
            \"cdm_name\",
            \"incidence_start_date\"
          ),
          selected = \"incidence_start_date\",
          #OPTION#,
          multiple = FALSE
        )
      ),
      div(
        style = #STYLE#,
        pickerInput(
          inputId = \"incidence_plot_facet\",
          label = \"Facet by\",
          choices = c(
            \"denominator_age_group\",
            \"denominator_sex\",
            \"denominator_days_prior_history\",
            \"outcome_cohort_name\",
            \"cdm_name\",
            \"incidence_start_date\"
          ),
          selected = c(
            \"outcome_cohort_name\",
            \"cdm_name\"
          ),
          #OPTION#,
          multiple = TRUE
        )
      ),
      div(
        style = #STYLE#,
        pickerInput(
          inputId = \"incidence_plot_group\",
          label = \"Colour by\",
          choices = c(
            \"denominator_age_group\",
            \"denominator_sex\",
            \"denominator_days_prior_history\",
            \"outcome_cohort_name\",
            \"cdm_name\",
            \"incidence_start_date\"
          ),
          #OPTION#,
          multiple = TRUE
        )
      ),
      plotlyOutput(
        \"plot_incidence_estimates\",
        height = \"800px\"
      ) %>%
        withSpinner()
    )
  )"
)
}
bodyIncidence <- gsub("#STYLE#", style, bodyIncidence)
bodyIncidence <- gsub("#OPTION#", option, bodyIncidence)
bodyIncidence <- paste0(bodyIncidence, collapse = ",\n")
moduleIncidence <- list(
  packages = c("readr", "here", "shinyWidgets", "DT", "plotly", "shinycssloaders"),
  read = c("Incidence estimates"),
  menu = dplyr::tibble(
    item = c("Incidence"), sub_item = c("Incidence estimates")
  ),
  body = list("Incidence estimates" = bodyIncidence)
)
