shinyUI(pageWithSidebar(
  headerPanel("Walters Illuminated Manuscript Database"),
  sidebarPanel(
    sliderInput('dateRange', 'Date Range',value = c(1000,1500), min = 500, max = 2000, step = 25),
#    selectizeInput(
#      'keywords', 'Key Words', choices = NULL, multiple = TRUE
#    ),
    hr(),
    h1("Instructions"),
    p("To restrict the results in the window on the right, drag the sliders above. Only manuscripts with a start range that intersects with the slider dates should appear in the data table."),
    p("The data table itself provides a search text box that dynamically matches any text in the table. It also supports paging via the buttons at the bottom and sorting via the widget next to the column name.")
  ),
  mainPanel(
    DT::dataTableOutput(outputId="view")
  )
))