#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinydashboard)
#library(rodham)
library(tidyverse)
library(devtools)
library(igraph)
library(ggraph)
library(visNetwork)
library(colourpicker)
library(DT)
library(networkD3)
library(dplyr)
library(tidyr)
library(plyr)
library(d3r)
library(threejs)
library(palr)

# Define server logic required to draw a histogram
function(input, output,session) {
  
  # data("emails")
  # edges=edges_emails(emails)
  # rownames(edges)<-c()
  # a=matrix(unique(edges$to))
  # b=matrix(unique(edges$from))
  # c=rbind.data.frame(a,b)
  # nodes=data.frame(id=seq(length(unique(c$V1))),label=unique(c$V1))
  # 
  # 
  # edge1=merge(edges,nodes,by.x = "from",by.y = "label")
  # names(edge1)[names(edge1) == 'id'] <- 'from1'
  # edge2=merge(edge1,nodes,by.x = "to",by.y = "label",sort = FALSE)
  # names(edge2)[names(edge2) == 'id'] <- 'to1'
  # edgefinal=select(edge2,from1,to1,freq)
  # colnames(edgefinal)<-c("from","to","value")

 # nodes<- data.frame(nodes, title=nodes$label)
  
  observeEvent(input$data_collapse, {
   
    if(input$data_collapse){
 
    show(id = "variable")
     show(id = "range")
    }else{
 
 hide(id = "variable")
   hide(id = "range")
      }
  })
  
  observeEvent(input$node_collapse, {
   
    if(input$node_collapse){
 show(id = "nodecol")
    show(id = "transparency")
    show(id = "colvariable")
    show(id = "shape")
    show(id = "Shadow")
    show(id = "shadowsize")
    show(id = "highlightdegree")
    show(id = "opacityhighlight")
    }else{
    hide(id = "nodecol")
     hide(id = "transparency")
     hide(id = "colvariable")
     hide(id = "shape")
     hide(id = "Shadow")
     hide(id = "shadowsize")
     hide(id = "highlightdegree")
     hide(id = "opacityhighlight")
    }
  })

  observeEvent(input$edge_collapse, {
    
    if(input$edge_collapse){
      show(id = "arrow")
      show(id = "edgecol")
      show(id = "etransparency")
      show(id = "dash")
    
    }else{
      hide(id = "arrow")
      hide(id = "edgecol")
      hide(id = "etransparency")
      hide(id = "dash")
     
    }
  })
  
  observeEvent(input$font_collapse, {
   
    if(input$font_collapse){
      show(id = "title")
      show(id = "subtitle")
      show(id = "footer")
      show(id = "fontsize")
      show(id="fontcol")
    }else{
      hide(id = "title")
      hide(id = "subtitle")
      hide(id = "footer")
      hide(id = "fontsize")
      hide(id="fontcol")
    }
  })
  
  # observe({
  #   if(!is.null(input$nodefile) & !is.null(input$edgefile)){
  #   nodes = read.csv(input$nodefile$datapath)
  #   edgefinal = read.csv(input$edgefile$datapath)
  
 
  output$variable <- renderUI({
  #  variable = colnames(edgefinal)
  #  variable = variable[variable != "from"]
   # variable = variable[variable != "to"]
    if(!is.null(input$nodefile) & !is.null(input$edgefile)){
      edgefinal = read.csv(input$edgefile$datapath)
      variable = colnames(edgefinal)
      variable = variable[variable != "from"]
      variable = variable[variable != "to"]
    selectInput( "variable", label = "Select a variable",selected = NULL,multiple = FALSE,choices = c("None", variable))
    }else{selectInput( "variable", label = "Select a variable",selected = NULL,multiple = FALSE,choices = c("None"))}
  })
  #  }
 # })
  
 
  observe({
  
    if(!is.null(input$nodefile) & !is.null(input$edgefile)){
      nodes = read.csv(input$nodefile$datapath)
      edgefinal = read.csv(input$edgefile$datapath)
      
    
  make_netgraph = function(name,layout_name){
    variable <- input$variable
    value <-input$range
    last = nrow(edgefinal)*value/100
    if(variable!="None"){
      edgefinal_sort=edgefinal[order(-edgefinal$value),]
      subedge = edgefinal_sort[1:last,]
    }else{
      subedge = edgefinal[1:last,]}
    show_node = unique(c(subedge$from, subedge$to))
    show_ind=unlist(sapply(1:length(show_node),function(x) which(nodes$id==show_node[x])))
    subnode = nodes[show_ind,]
    if(is.null(input$arrow)||length(input$arrow)>1){
      arrow="to,from"
    }
    else{
      arrow = input$arrow
    }
    if(input$colvariable!="None"){
      a=matrix(c(subedge[,"from"],subedge[,"to"]),ncol=1)
      b=cbind(a,rep(subedge[,"value"],2))
      uniquegp = duplicated(b[,1])
      gp.df=b[!uniquegp,]
      colnames(gp.df) = c("id","group")
      subnode =merge(subnode,gp.df,by="id")
      legend=TRUE
    }else{
      subnode$group = 1
      col=col2rgb(input$nodecol)
      nodergb=paste("rgba(",col[1],",",col[2],",",col[3],",",abs(1-input$transparency),")",sep="")
      ecol=col2rgb(input$edgecol)
      edgergb=paste("rgba(",ecol[1],",",ecol[2],",",ecol[3],",",abs(1-input$etransparency),")",sep="")
      subnode$color = nodergb
      subedge$color = edgergb
      legend=FALSE
    }
    graph <- graph.data.frame(subedge, directed = T)
    degree_value <- degree(graph, mode = "in")
    subnode$value <- degree_value[match(subnode$id, names(degree_value))]
    if(input$title == ""){
      title = "Network graph"
    }else{title= input$title}
    colopahi <- paste0("rgba(200,200,200,", abs(1-input$opacityhighlight), ")")
    name <- visNetwork(subnode, subedge,main = input$title,
                                 submain = input$subtitle, footer = input$footer)%>%
      visLegend(enabled = legend)%>%
      visEdges(arrows = arrow,dashes = input$dash)%>%
      visOptions(
        highlightNearest = list(enabled = TRUE, hover = TRUE,degree = input$highlightdegree, hideColor = colopahi)                             ,
        selectedBy = "group",
        manipulation = TRUE, nodesIdSelection = (list(enabled=TRUE,style="width: 200px")),collapse = TRUE)%>%
       visLegend(enabled = legend)%>%
      # visInteraction(hideEdgesOnDrag = TRUE)%>%
       visEdges(smooth = TRUE)%>%
      visPhysics(stabilization = FALSE)%>%
      # visConfigure(enabled = input$configuration)%>%
      visExport()
    if (!(layout_name=="random")){
      if(layout_name=="hierarchy"){
        name <- name %>% visHierarchicalLayout(enabled = TRUE)
      }else
      name <- name %>% visIgraphLayout(layout = layout_name)
    }
    name
  }
  
  mani_graph = function(name){
    observe({
      colopahi <- paste0("rgba(200,200,200,", abs(1-input$opacityhighlight), ")")
      colopagp <- paste0("rgba(200,200,200,", abs(1-input$opacityhighlight), ")")
      visNetworkProxy(name) %>%
        visEdges(arrows = arrow,dashes = input$dash)%>%
        visOptions(selectedBy = list(variable = "group", hideColor = colopagp))%>%
        visLayout(randomSeed = 1,hierarchical = input$hierarchy)%>%
        visNodes(size = input$nodesize, shape = input$shape,shadow = list(enabled=input$Shadow, size=input$shadowsize),font = list(color=input$fontcol,size=input$fontsize ))%>%
        visOptions(
          highlightNearest = list(enabled = TRUE, hover = TRUE,degree = input$highlightdegree, hideColor = colopahi)                             ,
          selectedBy = "group",
          manipulation = TRUE, nodesIdSelection = (list(enabled=TRUE,style="width: 200px")),collapse = TRUE)%>%
        visInteraction(hideEdgesOnDrag = TRUE)%>%
        visEdges(smooth = TRUE)
    })
  }
  
  
  
  output$network_random <- renderVisNetwork({
     layouts = c("Random","Davidson-Harel","DrL graph","Fruchterman-Reingold"
                                    ,"GEM","graphopt","Kamada-Kawai","Large",
                                    "Multidimensional scaling","Sugiyama graph")
     names = c("random","layout_with_dh","layout_with_drl","layout_with_fr",
                                   "layout_with_gem","layout_with_graphopt","layout_with_kk",
                                   "layout_with_lgl","layout_with_mds","layout_with_sugiyama")
      lay=names[which(layouts==input$algorithm)]
      make_netgraph(network_random,lay)
    })
  mani_graph("network_random")
  
  output$network_circle <- renderVisNetwork({
    make_netgraph(network_circle,"layout_in_circle")
  })
  mani_graph("network_circle")
  
  output$network_grid <- renderVisNetwork({
    make_netgraph(network_grid,"layout_on_grid")
  })
  mani_graph("network_grid")
  
  output$network_sphere <- renderVisNetwork({
    make_netgraph(network_sphere,"layout_on_sphere")
  })
  mani_graph("network_sphere")
  
  output$network_tree <- renderVisNetwork({
    make_netgraph(network_tree,"layout_as_tree")
  })
  mani_graph("network_tree")
  
  output$network_hierarchy <- renderVisNetwork({
    make_netgraph(network_tree,"hierarchy")
  })
  mani_graph("network_hierarchy")
  
  output$edgelist <- DT::renderDataTable(
    {
      
      variable <- input$variable
      value <-input$range
      last = nrow(edgefinal)*value/100

      if(variable!="None"){
        edgefinal_sort=edgefinal[order(edgefinal$value,decreasing = TRUE),]
        subedge = edgefinal_sort[1:last,]
      }else{
        subedge = edgefinal[1:last,]}
      show_node = unique(c(subedge$from, subedge$to))

      show_ind=unlist(sapply(1:length(show_node),function(x) which(nodes$id==show_node[x],)))

      subnode = nodes[show_ind,]
      DT::datatable(subedge, options = list(pageLength = 25,scrollX = TRUE))
      # if(!is.null(input$edgefile)){
      # DT::datatable(read.csv(input$edgefile$datapath), options = list(pageLength = 25,scrollX = TRUE))
      # }
    })
  
  
 
   observe({
      variable <- input$variable
      value <-input$range
      last = nrow(edgefinal)*value/100
      if(variable!="None"){
        edgefinal_sort=edgefinal[order(edgefinal$value, decreasing = TRUE),]
        subedge = edgefinal_sort[1:last,]
       
      }else{
        subedge = edgefinal[1:last,]}
      colnames(subedge) = c("source","target","value")
      
      
      nodes_detail = nodes
  
      rownames(nodes_detail) = nodes_detail$id
      
      sl=nodes_detail[as.character(subedge$source),]
      tl=nodes_detail[as.character(subedge$target),]
      source_label = as.character(sl$label)
      target_label = as.character(tl$label)
     
      #arc_nodes=data.frame(
      #name = unique(c(as.character(subedge$source),as.character(subedge$target))),stringsAsFactors = FALSE)
      subedge$source = source_label
      subedge$target = target_label
      
      print(subedge)
      arc_nodes=data.frame(
        name = unique(c(source_label,target_label)),stringsAsFactors = FALSE)
      
      arc_nodes$count = sapply(arc_nodes$name, function(x) sum(x==source_label)+sum(x==target_label))
      
      network=graph_from_data_frame(d=subedge, vertices=arc_nodes, directed=T)
      # Transform it in a JSON format for d3.js
      data <- d3_igraph(network)
      
      #r2d3(data,script ="jsdone.js")
      session$sendCustomMessage(type="jsondata",data)
      session$sendCustomMessage(type="nodecol",input$nodecol)
      session$sendCustomMessage(type="edgecol",input$edgecol)
      session$sendCustomMessage(type="dash",input$dash)
      session$sendCustomMessage(type="fontcol", input$fontcol)
      session$sendCustomMessage(type="opacityhighlight", input$opacityhighlight)
      session$sendCustomMessage(type="edgeopacity", input$etransparency)
      session$sendCustomMessage(type="nodeopacity", input$transparency)
      session$sendCustomMessage(type="fontsize", input$fontsize)
      session$sendCustomMessage(type="labelopacity", input$labelopacity)
    })
    

  
  output$sankey <- renderSankeyNetwork({
 
    
    variable <- input$variable
    value <-input$range
    last = nrow(edgefinal)*value/100
    if(variable!="None"){
      edgefinal_sort=edgefinal[order(edgefinal$value,decreasing = TRUE),]
      subedge = edgefinal_sort[1:last,]
    }else{
      subedge = edgefinal[1:last,]}

    if(nrow(subedge)> 300){
      subedge = edgefinal[1:300,]
      output$s <- renderText({ 
        "Warning:Data size is too large to plot (maximun 300)"
      })
    }else{output$s <- renderText({ 
      ""
    })}
    
    nodes_df = data.frame(label=nodes$label,stringsAsFactors = FALSE)
    rownames(nodes_df) = nodes$id
    nodes = nodes_df
    
    from = nodes[sort(unique(subedge$from)),]
    to = nodes[sort(unique(subedge$to)),]
    print(nodes)
    
   subedge$source.label <- as.character(nodes[as.character(subedge$from),])
    # sl=nodes[match(subedge$from,nodes$label),]
    # tl=nodes[match(subedge$to,nodes$label),]
    # print(match(subedge$from,nodes$label))
    
   # subedge$source.label <- as.character(sl)
  
    subedge$target.label <- as.character(nodes[as.character(subedge$to),])
   # subedge$target.label <- as.character(tl)
    
    id = c(unique(subedge$source.label),unique(subedge$target.label))
   
   # node1 = data.frame(id = id,stringsAsFactors = FALSE,row.names = seq(1:length(id)))
    node1 = data.frame(id = id,stringsAsFactors = FALSE)
    rownames(node1) = seq(1:length(id))
    
    s1.id = node1$id[1:length(from)]
    
    t1.id = tail(node1$id,length(to))
    
    subedge$from <-sapply(seq(1,length(subedge$source.label)), function(x){which(subedge$source.label[x] == s1.id)-1})
    subedge$to <-sapply(seq(1,length(subedge$target.label)), function(x){which(subedge$target.label[x] == t1.id)+length(from)-1})
    
    networkD3::sankeyNetwork(Links = subedge, Nodes = node1, Source = 'from',Target = 'to', Value = 'value',iterations = 0,NodeID = 'id',fontSize = 12)
  })
  
  output$Tab1 <- renderUI({
    tabsetPanel(id = "layout", 
                tabPanel("Random",
                         checkboxInput("configuration1", label="Configuration", value = FALSE),
                         selectInput("algorithm", "Layout Algorithm", choices = c("Random","Fruchterman-Reingold","Davidson-Harel","DrL graph"
                                                                                  ,"GEM","Kamada-Kawai",
                                                                                  "Multidimensional scaling","Sugiyama graph","graphopt","Large")),
                         visNetworkOutput("network_random" ,height = "1000px",width = "800px" )),
                tabPanel("Hierarchy",checkboxInput("configuration2", label="Configuration", value = FALSE),visNetworkOutput("network_hierarchy",height = "600px",width = "800px")),
                tabPanel("Tree",checkboxInput("configuration3", label="Configuration", value = FALSE),visNetworkOutput("network_tree",height = "600px",width = "800px")),
                tabPanel("Circle",checkboxInput("configuration4", label="Configuration", value = FALSE),visNetworkOutput("network_circle",height = "600px",width = "800px")),
                tabPanel("Sphere",checkboxInput("configuration5", label="Configuration", value = FALSE),visNetworkOutput("network_sphere",height = "600px",width = "800px")),
                tabPanel("On Grid",checkboxInput("configuration6", label="Configuration", value = FALSE),visNetworkOutput("network_grid",height = "600px",width = "800px"))
    )
  })
  
  observe({
    visNetworkProxy("network_random") %>%
     visConfigure(enabled = input$configuration1)
  })

  observe({
    visNetworkProxy("network_hierarchy") %>%
      visConfigure(enabled = input$configuration2)
  })
  
  observe({
    visNetworkProxy("network_tree") %>%
      visConfigure(enabled = input$configuration3)
  })
  
  observe({
    visNetworkProxy("network_circle") %>%
      visConfigure(enabled = input$configuration4)
  })
  
  observe({
    visNetworkProxy("network_sphere") %>%
      visConfigure(enabled = input$configuration5)
  })
  
  observe({
    visNetworkProxy("network_grid") %>%
      visConfigure(enabled = input$configuration6)
  })
 
  output$i3d <- renderScatterplotThree({
    #node : id; label; size; color
    #edge : from; to; size; color
    variable <- input$variable
    value <-input$range
    last = nrow(edgefinal)*value/100
    if(variable!="None"){
      edgefinal_sort=edgefinal[order(edgefinal$value, decreasing = TRUE),]
      subedge = edgefinal_sort[1:last,]
    }else{
      subedge = edgefinal[1:last,]}
    show_node = unique(c(subedge$from, subedge$to))
    show_ind=unlist(sapply(1:length(show_node),function(x) which(nodes$id==show_node[x])))
    subnode_3d = nodes[show_ind,]
    subnode_3d$color = col2hex(input$nodecol,alpha =abs(1-input$transparency))
    graph_object = graph_from_data_frame(subedge)
    graphjs(edges = subedge, nodes = subnode_3d,  stroke = NULL,
            edge.color = input$edgecol, edge.alpha = abs(1-input$etransparency),curvature = 1,height = "600px",width = "800px" , main = "3D Network",
            layout = layout_with_drl(graph_object,dim=3))
    
  })
  
    }
  })

}
  
  

