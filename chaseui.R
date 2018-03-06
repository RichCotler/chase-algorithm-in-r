##
## The Chase Algorithm (in R)
## Rich Cotler
## February 2018
##

## Shiny ui section
##

ui <- fluidPage(
  shinyjs::useShinyjs(),
  theme = shinytheme("paper"),
  titlePanel("Chase Algorithm in R"),
  sidebarPanel(
    fluidRow(style = "padding-bottom:20px",
             sliderInput("setcolcount", "Number of columns:",
                         min = 2, max = 10, value = 5),
             sliderInput("setrowcount", "Number of rows (Join Dependencies):",
                         min = 2, max = 10, value = 2),
             actionButton("savejds", "save JD(s)")
    ),
    fluidRow(style = "padding-bottom:20px",
             verbatimTextOutput("jdlist")
    ),
    fluidRow(style = "padding-bottom:20px",
             verbatimTextOutput("fdlist")
    ),
    fluidRow(style = "padding-bottom:30px",
             actionButton("runchase", "Start the Chase!"),
             verbatimTextOutput("sbmessage")
    ),
    fluidRow(style = "padding-bottom:30px",
      tags$style(HTML('table.dataTable tr, table.dataTable td {font-family:"Courier","courier new"; font-size: 16px;}')),
      tags$style(HTML('table.dataTable th {text-align: center;}')),
      dataTableOutput("validationerrors")
    ),
    fluidRow(
      column(4,
            actionButton("aboutdiag", "About", icon = icon("info-sign", lib = "glyphicon"))
      )
    )
  ),
  mainPanel(
    fluidRow(style = "padding-bottom:30px",
             verbatimTextOutput("cainfotitle"),
             tags$style(HTML('table.dataTable th {text-align: center;}')),
             tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: LightGreen !important;}')),
             dataTableOutput("catable")
    ),
    fluidRow(
      textOutput("fdinfotitle"),
      dataTableOutput("determinant")
    ),
    fluidRow(style = "padding-left:16px",
             dataTableOutput("dependent")
    ),
    fluidRow(style = "padding-top:30px",
      column(2,
             actionButton("saveFD", "save FD")
      ),
      column(2, offset = 1,
             actionButton("deletelastFD", "delete last FD", icon = icon("remove-sign", lib = "glyphicon"))
      ),
      column(2, offset = 2,
             actionButton("resetFDs", "reset all FDs", icon = icon("undo"))
      )
    ),
    fluidRow(style = "padding-bottom:20px",
             textOutput("reviewmessage")
    ),
    fluidRow(
             column(2,
                    actionButton("reviewfirst", "First", icon = icon("fast-backward", lib = "glyphicon"))
             ),
             column(2, offset = 1,
                    actionButton("reviewback", "Previous", icon = icon("step-backward", lib = "glyphicon"))
             ),
             column(2, offset = 1,
                     actionButton("reviewnext", "Next", icon = icon("step-forward", lib = "glyphicon"))
             ),
             column(2, offset = 1,
                    actionButton("reviewlast", "Last", icon = icon("fast-forward", lib = "glyphicon"))
             )
    )
  )
)
