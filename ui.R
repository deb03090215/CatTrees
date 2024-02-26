pacman::p_load(shiny)
pacman::p_load(shinydashboard)
pacman::p_load(plotly)
pacman::p_load(highcharter)
pacman::p_load(shinyjqui)
pacman::p_load(grDevices)
pacman::p_load(shinyBS)

sidebar <- dashboardSidebar(
sidebarMenu(
    menuItem("Dashboard", 
             tabName = "tab_normal",
             icon = icon("dashboard")),
    div(
      div(
        style="width:80%; display:inline-block; vertical-align: middle;",
        fileInput('file1', 'Phylogenetic trees',
                  accept=c('text/csv', 'text/comma-separated-values,text/plain',
                           '.nwk', '.phy', '.jplace', '.jtree', '.tree', '.tre'),
                  multiple = T),
      ),
      div(
        style="display:inline-block; vertical-align: middle;",
        bsButton("q1", label = "", icon = icon("info"),
                 style = "info", size = "extra-small"),
        bsPopover(id = "q1", title = "Help",
                  content = paste0("Please upload two or more tree files with format `.nwk`.",
                                   br(),
                                   "Recommended sequence number: below 1000"),
                  placement = "right", 
                  trigger = "click", 
                  options = list(container = "body")
        )
      )
    ),
    div(
      div(
        style="width:80%; display:inline-block; vertical-align: middle;",
        fileInput('file3', 'Reassortment strains',
                  accept=c('text/csv', 'text/comma-separated-values,text/plain',
                           '.csv')),
      ),
      div(
        style="display:inline-block; vertical-align: middle;",
        bsButton("q2", label = "", icon = icon("info"),
                 style = "info", size = "extra-small"),
        bsPopover(id = "q2", title = "Help",
                  content = paste0("Please upload a csv file containg a list of reassortment strains.",
                                   br(),
                                   "Titile of the list must be `strain`."),
                  placement = "right", 
                  trigger = "click", 
                  options = list(container = "body")
        )
      )
    ),
    div(
      div(
        style="width:80%; display:inline-block; vertical-align: middle;",
        fileInput('file4', 'Clade',
                  accept=c('text/csv', 'text/comma-separated-values,text/plain',
                           '.csv')),
        ),
      div(
        style="display:inline-block; vertical-align: middle;",
        bsButton("q3", label = "", icon = icon("info"),
                 style = "info", size = "extra-small"),
        bsPopover(id = "q3", title = "Help",
                  content = paste0("Please upload a csv file containing strains and their corresponding clades.", 
                                   br(),
                                   "Titile of two columns must be `Taxa` and `Clade`."),
                  placement = "right", 
                  trigger = "click",
                  options = list(container = "body")
        )
      )
    ),
    menuItem("Settings", 
             icon = icon("tools"),
             sliderInput("distance", "Distance between trees",
                         min = 0.000001, max = 1, value = c(0.000001)
                         ),
             sliderInput("z", "Size of reassortant line",
                         min = 0.05, max = 1,value = c(0.3)
                         ),
             sliderInput("n", "Size of gray line",
                         min = 0.005, max = 1,
                         value = c(0.005)
                         ),
             sliderInput("p", "Point size",
                         min = 0.1, max = 2,
                         value = c(0.8)
                         )
             ),
    menuItem("Picture size",
             icon = icon("photo"),
             numericInput("x", "Width", 3000),
             numericInput("y", "Hight", 1000),
             numericInput("r", "Resolution(dpi)", 300)
    )
))

tab_normal <- fluidRow(
  box(plotOutput("treeplot_normal"), width = "100%", height = "100%",dpi=3000)
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName  = "tab_normal", tab_normal)
    ),
    downloadButton("downloadPlot", "Download", class = "butt3")
)

header <- dashboardHeader(title = h4(HTML("CatTrees"),
                          style = "font-size: 32px")
                          )

dashboardPage(skin = "blue",
              dashboardHeader(title = header),
              sidebar,
              body
              )
