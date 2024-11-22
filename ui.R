# Load required packages
pacman::p_load(shiny, shinydashboard, plotly, highcharter, shinyjqui, grDevices, shinyBS)

# Define the dashboard sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    # Dashboard menu item
    menuItem("Dashboard", 
             tabName = "tab_normal",
             icon = icon("dashboard")),
    
    # File input for phylogenetic trees with help popover
    div(
      div(
        style = "width:80%; display:inline-block; vertical-align: middle;",
        fileInput('file1', 'Phylogenetic trees',
                  accept = c('text/csv', 'text/plain', '.nwk', '.phy', '.jplace', '.jtree', '.tree', '.tre'),
                  multiple = TRUE)
      ),
      div(
        style = "display:inline-block; vertical-align: middle;",
        bsButton("q1", label = "", icon = icon("info"), style = "info", size = "extra-small"),
        bsPopover(id = "q1", title = "Help",
                  content = paste0("Please upload two or more tree files in `.nwk` format.", 
                                   br(),
                                   "Recommended: fewer than 1000 sequences per tree."),
                  placement = "right",
                  trigger = "click",
                  options = list(container = "body"))
      )
    ),
    
    # File input for reassortment strains with help popover
    div(
      div(
        style = "width:80%; display:inline-block; vertical-align: middle;",
        fileInput('file3', 'Reassortment strains',
                  accept = c('text/csv', 'text/plain', '.csv'))
      ),
      div(
        style = "display:inline-block; vertical-align: middle;",
        bsButton("q2", label = "", icon = icon("info"), style = "info", size = "extra-small"),
        bsPopover(id = "q2", title = "Help",
                  content = paste0("Please upload a CSV file containing a list of reassortment strains.", 
                                   br(),
                                   "The column title must be `strain`."),
                  placement = "right",
                  trigger = "click",
                  options = list(container = "body"))
      )
    ),
    
    # File input for clade information with help popover
    div(
      div(
        style = "width:80%; display:inline-block; vertical-align: middle;",
        fileInput('file4', 'Clade',
                  accept = c('text/csv', 'text/plain', '.csv'))
      ),
      div(
        style = "display:inline-block; vertical-align: middle;",
        bsButton("q3", label = "", icon = icon("info"), style = "info", size = "extra-small"),
        bsPopover(id = "q3", title = "Help",
                  content = paste0("Please upload a CSV file with clade information.", 
                                   br(),
                                   "Columns must be titled `Taxa` and `Clade`."),
                  placement = "right",
                  trigger = "click",
                  options = list(container = "body"))
      )
    ),
    
    # Settings menu for tree display options
    menuItem("Settings", 
             icon = icon("tools"),
             sliderInput("distance", "Distance between trees",
                         min = 0.000001, max = 1, value = 0.000001),
             sliderInput("z", "Size of reassortant line",
                         min = 0.05, max = 1, value = 0.3),
             sliderInput("n", "Size of gray line",
                         min = 0.005, max = 1, value = 0.005),
             sliderInput("p", "Point size",
                         min = 0.1, max = 2, value = 0.8)
    ),
    
    # Picture size settings
    menuItem("Picture size",
             icon = icon("photo"),
             numericInput("x", "Width", 3000),
             numericInput("y", "Height", 1000),
             numericInput("r", "Resolution (dpi)", 300)
    )
  )
)

# Define the dashboard tab content
tab_normal <- fluidRow(
  box(plotOutput("treeplot_normal", height = "600px"), width = 12)
)

# Define the dashboard body
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "tab_normal", tab_normal)  # Main tab for tree plots
  ),
  downloadButton("downloadPlot", "Download", class = "butt3")  # Download button
)

# Define the dashboard header
header <- dashboardHeader(
  title = h4(HTML("CatTrees"), style = "font-size: 32px")
)

# Assemble the dashboard page
dashboardPage(
  skin = "blue",
  header,
  sidebar,
  body
)