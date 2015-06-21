shinyUI(pageWithSidebar(
  headerPanel("Walter's Illuminated Manuscript Database"),
  sidebarPanel(
    sliderInput('dateRange', 'Date Range',value = c(1000,1500), min = 500, max = 2000, step = 25),
    selectizeInput(
      'keywords', 'Key Words', choices = NULL, multiple = TRUE
    )
  ),
  mainPanel(
    DT::dataTableOutput(outputId="view")
  )
))