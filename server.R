# Load required packages
# Install Bioconductor packages if not present
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(c("ggtree", "treeio"), ask = FALSE)

# Load packages
library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggtree)
library(RColorBrewer)
library(magrittr)
library(tidytree)
library(plotly)
library(phytools)
library(ape)
library(treeio)
library(phangorn)
library(grid)
library(gridExtra)
library(dplyr)
library(ggrepel)
library(scales)
library(dendextend)
library(ggnewscale)
library(shadowtext)
library(highcharter)
library(shinyjqui)
library(tidyverse)
library(Cairo)
library(rsconnect)

# Allow a maximum upload size of 512 MB per request
options(shiny.maxRequestSize = 512 * 1024^2)

# Define Shiny server logic
shinyServer(function(input, output) {
  
  # Load and process input tree files
  tree <- reactive({
    validate(need(input$file1, "Please upload at least one valid tree file."))
    inFile <- req(input$file1)  # Require the input file
    tree_list <- lapply(inFile$datapath, function(path) {
      tryCatch({
        read.tree(path)
      }, error = function(e) {
        NULL  # Return NULL if a tree file cannot be read
      })
    })
    
    # Check for any invalid files
    if (any(sapply(tree_list, is.null))) {
      showNotification("Some files could not be read. Please check the format.", type = "error")
    }
    
    return(tree_list)
  })
  
  # Load reassortment strains file
  rst <- reactive({
    req(input$file3)  # Require the input file
    df <- read.table(input$file3$datapath, sep = "\t",header = TRUE)
    return(df)
  })
  
  # Load clade information
  clade <- reactive({
    req(input$file4)  # Require the input file
    df <- read.csv(input$file4$datapath,sep = "\t", header = TRUE)
    return(df)
  })
  
  # Generate the plot with progress bar
  plotInput <- reactive({
    validate(need(tree(), "Please upload tree files to generate the plot."))
    validate(need(clade(), "Clade information is required for plotting."))
    
    withProgress(message = "Generating Tree...", {
      infor <- list()
      df <- list()
      
      # Initialize the combined tree
      x <- tree()[[1]]
      cl <- clade()
      combined_tree <- ggtree(x) %<+% cl + 
        geom_tippoint(aes(color = Clade), size = input$p, shape = 15)

      # Highlight clades with specific color
      # cols <- c("6B.1" = "#F7A072", "6B.1A.5a" = "#DFBEE0", "6B.1A.5a.1" = "#55BB6D", "re6B.1A.5a.1" = "#94DF5D", "6B.1A.5a.2" = "#2F6EBA", "6B.1A.5a.2a" = "#6FAEE9", "6B.1A.5a.2a.1" = "#8841F6",  "6B.1A.5b" = "#F06C9B")
      # p <- ggtree(x) %<+% cl + geom_tippoint(aes(color=Clade), size=input$p, shape=15) + scale_colour_manual(values = cols)
      
      infor[[1]] <- combined_tree
      df[[1]] <- infor[[1]]$data
      label_pos <- max(df[[1]]$x) + input$distance
      label_name <- input$file1$name[1]
      
      # Append additional trees
      for (i in 2:length(tree())) {
        x <- tree()[[i]]
        p <- ggtree(x) %<+% cl + 
          geom_tippoint(aes(color = Clade), size = input$p, shape = 15)
        
        df[[i]] <- p$data
        df[[i]]$x <- df[[i]]$x + max(df[[i - 1]]$x) + input$distance
        
        combined_tree <- combined_tree +
          geom_tree(data = df[[i]]) +
          geom_tippoint(data = df[[i]], aes(color = Clade), size = input$p, shape = 15)
        
        label_pos <- append(label_pos, max(df[[i]]$x) + input$distance)
        label_name <- append(label_name, input$file1$name[i])
        
        # Update progress
        incProgress(1 / length(tree()), detail = paste("Processing tree", i, "of", length(tree())))
      }
      
      # Annotate tree with file names
      combined_tree <- combined_tree + 
        annotate("text", x = label_pos, y = 70, label = label_name)
      
      # Highlight reassortment strains
      combined_df <- bind_rows(df) %>% filter(isTip == TRUE)
      r <- rst()
      reassort <- subset(combined_df, label %in% r$strain)
      non_reassort <- subset(combined_df, !(label %in% r$strain))
      
      combined_tree <- combined_tree +
        geom_line(data = reassort, aes(x, y, group = label), size = input$z, color = "#ff0000") +
        geom_line(data = non_reassort, aes(x, y, group = label), size = input$n, color = "#646464")
      
      return(combined_tree)
    })
  })
  
  # Render the tree plot in the Shiny app
  output$treeplot_normal <- renderPlot({
    print(plotInput())
  })
  
  # Allow users to download the plot
  output$downloadPlot <- downloadHandler(
    filename = "tree_plot.png",
    content = function(file) {
      png(file, height = input$y, width = input$x, res = input$r)
      print(plotInput())
      dev.off()
    }
  )
})
