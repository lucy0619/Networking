// set the dimensions and margins of the graph
var margin = {top: 0, right: 30, bottom: 50, left: 60},
  width = 650 - margin.left - margin.right,
  height = 400 - margin.top - margin.bottom;

//read data from shiny

Shiny.addCustomMessageHandler("jsondata",
function(message){
var data = message;

Shiny.addCustomMessageHandler("nodecol",
function(nodecol){
  var nodecol = nodecol;

Shiny.addCustomMessageHandler("edgecol",
function(edgecol){
  var edgecol = edgecol;

Shiny.addCustomMessageHandler("dash",
function(dash){
  var link_dash = null;
  if(dash){
    link_dash = 4;
  }else{link_dash = null;}

Shiny.addCustomMessageHandler("fontcol",
function(fontcol){
  var fontcol = fontcol;

Shiny.addCustomMessageHandler("opacityhighlight",
function(opacityhighlight){
  var opacityhighlight = Math.abs(1-opacityhighlight);

Shiny.addCustomMessageHandler("edgeopacity",
  function(edgeopacity){
    var edgeopacity = Math.abs(1-edgeopacity);

Shiny.addCustomMessageHandler("nodeopacity",
    function(nodeopacity){
      var nodeopacity = Math.abs(1-nodeopacity);
  
Shiny.addCustomMessageHandler("fontsize",
function(fontsize){
var fontsize = fontsize;

Shiny.addCustomMessageHandler("labelopacity",
    function(labelopacity){
      var labelopacity = Math.abs(1-labelopacity);

//remove the previous svg
d3.select("svg").remove(); 

 var nodeRadius = d3.scaleSqrt().range([3, 7]);
 var linkWidth = d3.scaleLinear().range([1.5, 2 * nodeRadius.range()[0]]);
 nodeRadius.domain(d3.extent(data.nodes, function (d) { return d.count; }));
 linkWidth.domain(d3.extent(data.links, function (d) { return d.value; }));


// append the svg object to the body of the page
var svg = d3.select("#d3")
  .append("svg")
   .attr("width", 800)
    .attr("height", 800)
  .append("g")
    .attr("transform",
        "translate(" + margin.left + "," + margin.top + ")");



  // List of node names
  var allNodes = data.nodes.map(function(d){return d.name})

  // List of groups
  var allGroups = data.nodes.map(function(d){return d.grp})
  allGroups = [...new Set(allGroups)]

  // A color scale for groups:
  var color = d3.scaleOrdinal()
    .domain(allGroups)
    .range(d3.schemeSet3);



  // A linear scale to position the nodes on the X axis
  var x = d3.scalePoint()
    .range([0, width])
    .domain(allNodes)

  // In my input data, links are provided between nodes -id-, NOT between node names.
  // So I have to do a link between this id and the name
  var idToNode = {};
  data.nodes.forEach(function (n) {
    idToNode[n.id] = n;
  });
 


  // And give them a label
  var labels = svg
    .selectAll("mylabels")
    .data(data.nodes)
    .enter()
    .append("text")
      .attr("x", 0)
      .attr("y", 0)
      .text(function(d){ return(d.name)} )
      .style("text-anchor", "end")
      .attr("transform", function(d){ return( "translate(" + (x(d.name)) + "," + (height-15) + ")rotate(-45)")})
      .style("font-size", fontsize)  
      .style("fill",fontcol)
      .style("opacity",labelopacity)
  

  // Add arrow
  svg.append("svg:defs").append("svg:marker")
  .attr("id", "triangle")
  .attr("refX", 6)
  .attr("refY", 6)
  .attr("markerWidth", 30)
  .attr("markerHeight", 30)
  .attr("markerUnits","userSpaceOnUse")
  .attr("orient", "auto")
  .append("path")
  .attr("d", "M 0 0 12 6 0 12 3 6")
  .style("fill", "black");



  // Add the links
  var links = svg
    .selectAll('mylinks')
    .data(data.links)
    .enter()
    .append('path')
    .attr("marker-end", "url(#triangle)")
    .attr('d', function (d) {
      start = x(idToNode[d.source].name)    // X position of start node on the X axis
      end = x(idToNode[d.target].name)      // X position of end node
      return ['M', start, height-30,    // the arc starts at the coordinate x=start, y=height-30 (where the starting node is)
        'A',                            // This means we're gonna build an elliptical arc
        (start - end)/2,     // Next 2 lines are the coordinates of the inflexion point. Height of this point is proportional with start - end distance
        (start - end)/2, 0, 0, 
        1, end, height-30] // We always want the arc on top. So if end is before start, putting 0 here turn the arc upside down.
        .join(' ');
    })
    .style("fill", "none")
    .style("stroke-dasharray",link_dash)
    .attr("stroke", edgecol)
    .style("stroke-width", function (d) { return linkWidth(d.value); })
    .style('stroke-opacity',edgeopacity)
    .on('mouseover', function (d) {
        links.style('stroke-opacity', .2);
        d3.select(this).style('stroke-opacity', 1);
        nodes.style('opacity', function (node_d) {
          return node_d.id === d.source || node_d.id === d.target ? 1 : .2;
        });
        labels
        .style("font-size", function(label_d){ return label_d.name === d.source || label_d.id === d.target ? fontsize+6 : 2 } )
        .attr("y", function(label_d){ return label_d.name === d.source || label_d.id === d.target? 10 : 0 } )
        .style("opacity",1)
      })
      .on('mouseout', function (d) {
        links.style('stroke-opacity', edgeopacity);
        nodes.style('opacity', nodeopacity);
        labels.style("font-size", fontsize )
        labels.style("opacity",labelopacity)
  
      })

 
  // Add the circle for the nodes
  var nodes = svg
    .selectAll("mynodes")
    .data(data.nodes)
    .enter()
    .append("circle")
      .attr("cx", function(d){ return(x(d.name))})
      .attr("cy", height-30)
      .attr("r", function (d) { return nodeRadius(d.count); })
    //  .style("fill", function(d){ return color(d.grp)})
      .style("fill", nodecol)
      .attr("stroke", "white")
      .style('opacity',nodeopacity)


  // Add the highlighting functionality
  nodes
    .on('mouseover', function (d) {
      // Highlight the nodes: every node is green except of him
      nodes
      .style("opacity",opacityhighlight)
      d3.select(this)
        .style('opacity', 1)

      // Highlight corresponding nodes
      var nodesToHighlight = data.links.map(function (e) { return e.source === d ? e.target : e.target === d ? e.source : 0})
        .filter(function (d) { return d; });
      nodes.filter(function (d) { return nodesToHighlight.indexOf(d) >= 0; })
        .style('opacity', 1);

      // Highlight the connections
      links
        .style('stroke', function (link_d) { return link_d.source === d.id || link_d.target === d.id ? color(d.grp) : edgecol;})
        .style('stroke-opacity', function (link_d) { return link_d.source === d.id || link_d.target === d.id ? 1 : .2;})
       // .style('stroke-width', function (link_d) { return link_d.source === d.id || link_d.target === d.id ? 4 : 1;})
      labels
        .style("font-size", function(label_d){ return label_d.name === d.name ? fontsize+6 : 2 } )
        .attr("y", function(label_d){ return label_d.name === d.name ? 10 : 0 } )
        .style("opacity",1)

    })
    .on('mouseout', function (d) {
      nodes.style('opacity', nodeopacity)
      links
        .style('stroke', edgecol)
        .style('stroke-opacity', edgeopacity)
       // .style('stroke-width', '1')
      labels
        .style("font-size", fontsize )
        .style("opacity",labelopacity)
      })
    })
  })
})
})
})
})
})
})
})
  /*
   
    d3.select('#saveButton').on('click', function(){
      var svgString = getSVGString(d3.select('svg').node());
      svgString2Image( svgString, 2*width, 2*height, 'png', save ); // passes Blob and filesize String to the callback
    
      function save( dataBlob, filesize ){
        saveAs( dataBlob, 'D3 vis exported to PNG.png' ); // FileSaver.js function
      }
    });
    
    /*
    // Below are the functions that handle actual exporting:
    // getSVGString ( svgNode ) and svgString2Image( svgString, width, height, format, callback )
    function getSVGString( svgNode ) {
      svgNode.setAttribute('xlink', 'http://www.w3.org/1999/xlink');
    //	var cssStyleText = getCSSStyles( svgNode );
    //	appendCSS( cssStyleText, svgNode );
    
      var serializer = new XMLSerializer();
      var svgString = serializer.serializeToString(svgNode);
      svgString = svgString.replace(/(\w+)?:?xlink=/g, 'xmlns:xlink='); // Fix root xlink without namespace
      svgString = svgString.replace(/NS\d+:href/g, 'xlink:href'); // Safari NS namespace fix
    
      return svgString;
    
    }
    
    
    function svgString2Image( svgString, width, height, format, callback ) {
      var format = format ? format : 'png';
    
      var imgsrc = 'data:image/svg+xml;base64,'+ btoa( unescape( encodeURIComponent( svgString ) ) ); // Convert SVG string to data URL
    
      var canvas = document.createElement("canvas");
      var context = canvas.getContext("2d");
    
      canvas.width = width;
      canvas.height = height;
    
      var image = new Image();
      image.onload = function() {
        context.clearRect ( 0, 0, width, height );
        context.drawImage(image, 0, 0, width, height);
    
        canvas.toBlob( function(blob) {
          var filesize = Math.round( blob.length/1024 ) + ' KB';
          if ( callback ) callback( blob, filesize );
        });
    
        
      };
    
      image.src = imgsrc;
    }
*/
  })
