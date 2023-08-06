shinyTemplate <- '
#PACKAGES#

#LOAD#

# ui shiny ----
ui <- dashboardPage(
  dashboardHeader(title = "Menu"),
  ## menu ----
  dashboardSidebar(
    sidebarMenu(
      #MENU
    )
  ),
  ## body ----
  dashboardBody(
    #MENU#
  )
)

# server shiny ----
server <- function(input, output, session) {
#SERVER#
}

# run shiny ----
shinyApp(ui, server)
'
