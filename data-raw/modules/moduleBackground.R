# This file contains the information for the background module
bodyBackground <- c(
  'h3("Please edit this part in the file app.R")',
  'h3("STUDY TITLE")',
  'h4("Identifier: PX-CX-XXX")',
  'p("The main purposo of this study is to identify ...")',
  'p("This shinyApp was generated automatically with ShinyGenerator")'
) %>%
  paste0(collapse = ",\n")

moduleBackground <- list(
  packages = c("shiny", "shinydashboard", "dplyr"),
  menu = dplyr::tibble(item = "Background", sub_item = ""),
  body = list("Background" = bodyBackground)
)
