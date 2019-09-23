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
#library(shinyWidgets)

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
  tags$div(style="height: 80px",fileInput("edgefile","Upload Connection File",accept = c('text/csv',  
                                                                            'text/comma-separated-values,text/plain', '.csv'))),
        actionButton("rmedge","Remove Connection File",icon = icon("far fa-trash-alt")),
  hr(),
  tags$div(style="height: 80px", fileInput("nodefile","Upload Node File",accept = c('text/csv', 
                                                                            'text/comma-separated-values,text/plain', '.csv'))),
       actionButton("rmnode","Remove Node File",icon = icon("far fa-trash-alt")),
        hr(),
        tags$div(style = "font-size: 22px; font-weight: bold;",  checkboxInput("data_collapse", "Data")),
        
        uiOutput("variable"),
        #sliderInput( "range", "Subset % data by variable",min = 0,max = 100, step = 2,value = 5, post = "%" ),
       uiOutput("range"),
    
       tags$div(style = "font-size: 22px; font-weight: bold;", checkboxInput("node_collapse", "Node")),
        colourpicker::colourInput("nodecol","Node Color",value = "lightblue", allowTransparent = FALSE,returnName = FALSE ),
       uiOutput("colbyvariable"),
        sliderInput( "transparency", "Node Transparency", min = 0, max = 1, value = 0 ),
          
        selectInput( "shape",label = "Shape", selected = NULL, multiple = FALSE, choices = c( "dot", "ellipse", "box", "circle",
                                                                                              "database", "diamond", "square", "star", "text", "triangle", "triangleDown","icon" ) ),
       uiOutput("shapebyvariable"),
        checkboxInput("Shadow", label = "Shadow", value = TRUE),
        sliderInput( "shadowsize", "Shadow Transparency", min = 0, max = 30,value = 20 ),
        sliderInput( "highlightdegree","Highlighted Degree",min = 1,max = 10, value = 1),
        sliderInput("opacityhighlight", "Transparency of nodes not highlighted", min = 0, max = 1, value = 0.5 ),
  
        #checkboxInput("edge_collapse", "Connection"),
       tags$div(style = "font-size: 22px; font-weight: bold;", checkboxInput("edge_collapse", "Connection")),
       
        checkboxGroupInput("arrow",label = "Arrow", choices = c("to", "from"),inline = TRUE,selected = c("to") ),
       colourpicker::colourInput("edgecol", "Connection Color", value = "lightblue"),
        sliderInput( "etransparency","Transparency", min = 0, max = 1,value = 0 ),
        checkboxInput("dash", label = "Dashes", value = FALSE),
        
       #checkboxInput("font_collapse", "Titles and Labels",width = "700px"),
       #prettySwitch("font_collapse", "Titles and Labels",value = FALSE, bigger = TRUE,width = "200%"),
       tags$div(style = "font-size: 22px; font-weight: bold;",checkboxInput("font_collapse", "Titles and Labels")),
        textInput( "Title", inputId = "title",value = "Network Graph",placeholder = "Please enter title"),
        textInput( "SubTitle",inputId = "subtitle", value = "", placeholder = "Please enter subtitle"),
        textInput("Footer",inputId = "footer",value = "",placeholder = "Please enter footer" ),
       colourpicker::colourInput( "fontcol","Font Color",value = "black",showColour = c("both", "text","background"),
                     palette = c("square", "limited"),allowedCols = NULL,allowTransparent = TRUE,returnName = FALSE),
        sliderInput( "fontsize", "Font Size",min = 0, max = 30,value = 15)
        
      ),
    
    
    mainPanel(fluidPage(
      tabsetPanel(
        tabPanel("Dataset",uiOutput("datatab")),
        #tabPanel("Vertices-Edges Graph", uiOutput("Tab1")),
        #uiOutput("Tab1"),
        tabPanel("Default",
                 fluidRow(
                   column(2,checkboxInput("configuration1", label="More controls below", value = FALSE,width='200%')),
                   column(7,selectInput("algorithm", "Layout Algorithm",width="200px", choices = c("Interactive Kamada-Kawai","Fruchterman-Reingold","Davidson-Harel","DrL graph"
                                                                                                   ,"GEM","Kamada-Kawai",
                                                                                                   "Multidimensional scaling","Sugiyama graph","graphopt","Large")))
                 ),
                 visNetworkOutput("network_random" ,height = "1000px",width = "800px" )),
        tabPanel("Hierarchy",checkboxInput("configuration2", label="More controls below", value = FALSE),visNetworkOutput("network_hierarchy",height = "600px",width = "800px")),
        tabPanel("Tree",checkboxInput("configuration3", label="More controls below", value = FALSE),visNetworkOutput("network_tree",height = "600px",width = "800px")),
        tabPanel("Circle",checkboxInput("configuration4", label="More controls below", value = FALSE),visNetworkOutput("network_circle",height = "600px",width = "800px")),
        tabPanel("Sphere",checkboxInput("configuration5", label="More controls below", value = FALSE),visNetworkOutput("network_sphere",height = "600px",width = "800px")),
        tabPanel("On Grid",checkboxInput("configuration6", label="More controls below", value = FALSE),visNetworkOutput("network_grid",height = "600px",width = "800px")),
        tabPanel("3D",
                 selectInput("algorithm3d", "Layout Algorithm",width="200px", choices = c("Random","On Grid","Fruchterman-Reingold"
                                                                                        ,"Kamada-Kawai",
                                                                                        "Drl Graph")),
                 scatterplotThreeOutput("i3d",  height = "600px", width = "800px")),
        tabPanel(
          "Arc line",
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
          "Sankey
          ",
          sankeyNetworkOutput("sankey", height = "1200px", width = "800px"),
          textOutput("s"),
          tags$head(
            tags$style("#s{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }")
          )
        )
        
      )
    )
    
  )
    )
)
)
