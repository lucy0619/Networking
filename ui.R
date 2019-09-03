#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinydashboard)
library(rodham)
library(tidyverse)
library(devtools)
library(arcdiagram)
library(igraph)
library(ggraph)
library(visNetwork)
library(colourpicker)
library(DT)
library(networkD3)
library(threejs)

# data("emails")
# edges = edges_emails(emails)
# rownames(edges) <- c()
# a = matrix(unique(edges$to))
# b = matrix(unique(edges$from))
# c = rbind.data.frame(a, b)
# nodes = data.frame(id = seq(length(unique(c$V1))), label = unique(c$V1))
# 
# edge1 = merge(edges, nodes, by.x = "from", by.y = "label")
# names(edge1)[names(edge1) == 'id'] <- 'from1'
# edge2 = merge(edge1,
#               nodes,
#               by.x = "to",
#               by.y = "label",
#               sort = FALSE)
# names(edge2)[names(edge2) == 'id'] <- 'to1'
# edgefinal = select(edge2, from1, to1, freq)
# colnames(edgefinal) <- c("from", "to", "value")
# 
# variable = colnames(edgefinal)
#  variable = variable[variable != "from"]
#  variable = variable[variable != "to"]

# Define UI for application that draws a histogram
ui <- (
  fluidPage(
    useShinyjs(),
 
    headerPanel("Network Visualization"),
    sidebarLayout(
      sidebarPanel(
        width="3",
       
        fileInput("nodefile","Upload Node File",accept = c('text/csv',  'text/comma-separated-values,text/plain', '.csv')),
        fileInput("edgefile","Upload Connection File",accept = c('text/csv',  'text/comma-separated-values,text/plain', '.csv')),
        checkboxInput("data_collapse", "Data"),
       # selectInput( "variable", label = "Select a variable",selected = NULL,multiple = FALSE,choices = c("None", variable)),
        uiOutput("variable"),
        sliderInput( "range", "",min = 0,max = 100, step = 2,value = 5, post = "%" ),
        
        checkboxInput("node_collapse", "Node"),
        colourpicker::colourInput("nodecol","Node Color",value = "lightblue", allowTransparent = FALSE,returnName = FALSE ),
        sliderInput( "transparency", "Transparency", min = 0, max = 1, value = 0 ),
        selectInput( "colvariable",label = "Color by variable", selected = NULL, multiple = FALSE, choices = c("None", variable)),
        selectInput( "shape",label = "Shape", selected = NULL, multiple = FALSE, choices = c( "dot", "ellipse", "box", "circle",
                                                                                              "database", "diamond", "square", "star", "text", "triangle", "triangle down" ) ),
        checkboxInput("Shadow", label = "Shadow", value = FALSE),
        sliderInput( "shadowsize", "Shadow Size", min = 0, max = 30,value = 10 ),
        sliderInput( "highlightdegree","Highlight Degree",min = 1,max = 10, value = 1),
        sliderInput("opacityhighlight", "Transparency of Unhighlight nodes", min = 0, max = 1, value = 0.5 ),
  
        checkboxInput("edge_collapse", "Connection"),
        checkboxGroupInput("arrow",label = "Arrow", choices = c("to", "from"),inline = TRUE ),
       colourpicker::colourInput("edgecol", "Connection Color", value = "lightblue"),
        sliderInput( "etransparency","Transparency", min = 0, max = 1,value = 0 ),
        checkboxInput("dash", label = "Dashes", value = FALSE),
        
       checkboxInput("font_collapse", "Font"),
        textInput( "Title", inputId = "title",value = "Network Graph",placeholder = "Please enter title"),
        textInput( "SubTitle",inputId = "subtitle", value = "", placeholder = "Please enter subtitle"),
        textInput("Footer",inputId = "footer",value = "",placeholder = "Please enter footer" ),
       colourpicker::colourInput( "fontcol","Font Color",value = "black",showColour = c("both", "text","background"),
                     palette = c("square", "limited"),allowedCols = NULL,allowTransparent = TRUE,returnName = FALSE),
        sliderInput( "fontsize", "Font Size",min = 0, max = 30,value = 15)
        
      ),
    
    
    mainPanel(fluidPage(
      tabsetPanel(
        tabPanel("DataSet", DT::dataTableOutput("edgelist")),
        tabPanel("Vertices-Edges Graph", uiOutput("Tab1")),
        tabPanel(
          "Arc Graph",
          sliderInput(
            "labelopacity",
            "Transparency of Labels",
            min = 0,
            max = 1,
            value = 0
          ),
          tags$script(src = "https://d3js.org/d3.v4.min.js"),
          tags$script(src = "https://d3js.org/d3.v4.js"),
          tags$script(src = "https://d3js.org/d3-scale-chromatic.v1.min.js"),
          tags$script(src = "https://cdn.rawgit.com/eligrey/canvas-toBlob.js/f1a01896135ab378aa5c0118eadd81da55e698d8/canvas-toBlob.js"),
          tags$script(src = "https://cdn.rawgit.com/eligrey/FileSaver.js/e9d941381475b5df8b7d7691013401e171014e89/FileSaver.min.js"),
          tags$script(src = "jsdone.js"),
          tags$div(id = "d3"),
          tags$button(id = "saveButton", "Save")
          
        ),
        tabPanel(
          "Sankey Graph",
          sankeyNetworkOutput("sankey", height = "1200px", width = "800px"),
          textOutput("s"),
          tags$head(
            tags$style("#s{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }")
          )
        ),
        tabPanel("3D Graph",scatterplotThreeOutput("i3d",  height = "1200px", width = "800px"))
        
      )
    )
    
  )
    )
)
)
