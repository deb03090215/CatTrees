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
library(tools)

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

      ### use default color code            
      n_clades <- length(unique(cl$Clade))
      colors <- rainbow(n_clades)  # 或使用其他色盤生成器
      combined_tree <- ggtree(x) %<+% cl + 
        geom_tippoint(aes(color = Clade), size = input$p, shape = 15)+
        scale_color_manual(values = colors)
      ###
      
      # ###---
      # ### Highlight clades with specific color v2
      # cl$Clade <- as.character(cl$Clade)
      # #highlight_clades <- c("C.3", "C.5.1")
      # #highlight_colors <- c("C.3" = "#e74c3c", "C.5.1"="#28b463")
      # highlight_clades <- c("C.3", "C.5.7", "C.5.1")
      # highlight_colors <- c("C.3" = "#e74c3c", "C.5.7"="#FFC300", "C.5.1"="#28b463")
      # #highlight_clades <- c("C.3", "C.4")
      # #highlight_colors <- c("C.3" = "#e74c3c", "C.4" = "#2e86c1")
      # 
      # ### get clades
      # tree_tips <- tree()[[1]]$tip.label
      # cl_tips <- cl[cl$label %in% tree_tips, ]
      # all_clades <- unique(cl_tips$Clade)
      # 
      # ### gray for other clades
      # other_clades <- setdiff(all_clades, highlight_clades)
      # gray_colors <- setNames(rep("#D3D3D3", length(other_clades)), other_clades)
      # 
      # ### combine
      # cols <- c(highlight_colors, gray_colors)
      # 
      # ### plot
      # combined_tree <- ggtree(tree()[[1]]) %<+% cl +
      #   geom_tippoint(aes(color = Clade), size = input$p, shape = 15) +
      #   scale_color_manual(values = cols, breaks = highlight_clades)
      # ###
      # ###---
      
      infor[[1]] <- combined_tree
      df[[1]] <- infor[[1]]$data
      
      # label_pos <- max(df[[1]]$x) + input$distance
      label_pos <- mean(range(df[[1]]$x, na.rm = TRUE))
      
      # label_name <- input$file1$name[1]
      # use ?_? the second element for naming
      # label_name <- map(strsplit(input$file1$name[1], split="_"),2)
      label_name <- tools::file_path_sans_ext(strsplit(input$file1$name[1], "_")[[1]][2])
      
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

        # label_pos <- append(label_pos, max(df[[i]]$x) + input$distance)
        label_pos <- append(label_pos, mean(range(df[[i]]$x, na.rm = TRUE)))
        # just use file name
        # label_name <- append(label_name, input$file1$name[i])
        # use ?_? the second element for naming
        label_name <- append(label_name, tools::file_path_sans_ext(strsplit(input$file1$name[i], "_")[[1]][2]))
        
        # label_name <- append(label_name, map(strsplit(input$file1$name[i], split="_"),2))
        
        # Update progress
        incProgress(1 / length(tree()), detail = paste("Processing tree", i, "of", length(tree())))
      }
      
      # Annotate tree with file names
      label_y <- sapply(df, function(x) max(x$y, na.rm = TRUE)) + 20
            
      combined_tree <- combined_tree + 
        annotate("text", x = label_pos, y = label_y, label = label_name)+ coord_cartesian(clip = "off")+ theme(
          legend.position = "right",
          legend.justification = "center",
          plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
        )
      
#      combined_tree <- combined_tree + 
#        annotate("text", x = label_pos-0.006, y = 2200, label = label_name)
        
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
