
#### UI Component


shinyUI(navbarPage("Interactive Dashboard",
                   tabPanel("Scatter Plot",
                                plotOutput("plot")
                   ),
                   tabPanel("Summary Report",
                            verbatimTextOutput("summary")
                   ),
                   tabPanel("Data",
                            dataTableOutput("table")
                   )
))