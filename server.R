pacman::p_load(shiny)
pacman::p_load(shinydashboard)
pacman::p_load(ggplot2)
pacman::p_load(ggtree)
pacman::p_load(showtext)
pacman::p_load(RColorBrewer)
pacman::p_load(magrittr)
pacman::p_load(tidytree)
pacman::p_load(plotly)
pacman::p_load(phytools)
pacman::p_load(ape)
pacman::p_load(treeio)
pacman::p_load(phangorn)
pacman::p_load(grid)
pacman::p_load(gridExtra)
pacman::p_load(dplyr)
pacman::p_load(ggrepel)
pacman::p_load(scales)
pacman::p_load(dendextend)
pacman::p_load(ggnewscale)
pacman::p_load(shadowtext)
pacman::p_load(TDbook)
pacman::p_load(highcharter)
pacman::p_load(shinyjqui)
pacman::p_load(tidyverse)
pacman::p_load(fivethirtyeight)
pacman::p_load(grDevices)
pacman::p_load(Cairo)
pacman::p_load(vegan)

shinyServer(function(input, output){
  tree <- reactive({
    inFile <- req(input$file1)
    if (is.null(inFile)){
      return(NULL)
    }else {
      
      numfiles = nrow(inFile)
      tree_list = list()
      
      for (i in 1:numfiles){
        tree_list[[i]] = read.tree(input$file1$datapath[i])
      }
      return(tree_list)
    }
  })

    ## reassortment strains file
    rst <- reactive({
      req(input$file3)
      df <- read.table(input$file3$datapath, sep = "\t",header = TRUE)
      return(df)
    })
    
    ## clade file
    clade <- reactive({
        req(input$file4)
        df <- read.csv(input$file4$datapath,sep = "\t", header = TRUE)
        return(df)
    })
    
    ## 
    plotInput <- reactive({
        ## make 2 empty list for "for loop"
        infor <- list()
        df <- list()
        
        ## build reference tree at first
        ## import reference & clade information
        x <- tree()[[1]]
        cl <- clade()
        
        #highlight clade
        #cl_list <- list(Clade = cl[!duplicated(cl$clade),])
        #cl_list <- factor(cl$clade, levels = cl_list$Clade)
        
        
        p1 <- ggtree(x) %<+% cl + 
              geom_tippoint(aes(color=Clade), size=input$p, shape=15)
        
        infor[[1]] <- p1
        df[[1]] <- infor[[1]]$data

        ## combine reference tree and other segment trees
        combined_tree <- infor[[1]]

        sum<-length(tree())
       
        ## retrieve max of x and file name
        label_pos <- max(df[[1]]$x)+input$distance
        label_name <- input$file1$name[1]
        
        for (i in 2:sum) {
        ## import segment files

            x<-tree()[[i]]

            #highlight clade
            p2<- ggtree(x) %<+% cl + geom_tippoint(aes(color=Clade), size=input$p, shape=15)

            infor[[i]] <- p2
            df[[i]] <- infor[[i]]$data

            ## distance between trees. 0.001 and 1 for divergence and time tree, respectively
            df[[i]]$x <- (df[[i]]$x + max(df[[i-1]]$x) + input$distance) + max(df[[1]]$x) + input$distance

            ## append tree to init tree
            combined_tree <- combined_tree +
                geom_tree(data=df[[i]]) +
                geom_tippoint(data=df[[i]], aes(color=Clade), size=input$p, shape=15) + 
              labs(input$file1$name[i])

            ## retrieve max of x and file name  
            label_pos <- append(label_pos, max(df[[i]]$x)+input$distance)
            label_name <- append(label_name, input$file1$name[i])
        }

        ## annotate file name based on their x positions
        #combined_tree <- combined_tree + annotate("text", x = label_pos, y=5, label = label_name)
        combined_tree <- combined_tree + annotate("text", x = label_pos, y=70, side=1, label = label_name)
        
        ## only keep tips for annotating lines
        combined_df <- bind_rows(df[1:8]) %>% filter(isTip == TRUE)
        print(combined_df$label)
        ## plot trees
        r <- rst()
    
        ### split the data for annotating different color and size
        rassort <- subset(combined_df, label %in% r$strain)
        #print(rassort)

        non_rassort <- subset(combined_df, !(label %in% r$strain))
      
        #print(non_rassort)

        combined_tree_w_rassort <- combined_tree + geom_line(data = rassort,
                                  aes(x, y, group = label),
                                  size = input$z,
                                  color = "#ff0000")
    
        combined_tree_w_rassort + geom_line(data = non_rassort,
                                     aes(x, y, group = label),
                                    size = input$n,
                                   color = "#646464")
        })
    
      #output showed in the interface
      output$treeplot_normal <- renderPlot({
        print(plotInput())
      })
      
      #Download file
      output$downloadPlot <- downloadHandler(
      filename = "report.png" ,
      tags$style(".skin-blue .sidebar a { color: #000000 }"),
      content = function(file) {
        png(file,
            height = input$y,
            width = input$x, 
            res = input$r)
        print(plotInput())
        dev.off()
      })

})
