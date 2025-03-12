// !preview r2d3 data = jsonlite::read_json("www/js/test-graph.json"), d3_version = 6, css = "www/css/graph.css", options = list(debug = TRUE, use_size = FALSE, use_weight_as_stroke = TRUE, stroke_scale_factor = 10, scale_weights = TRUE, colour_nodes = TRUE, charge_strength = -100, link_distance = 50)

const testing = false;
const radius = 10;
const min_node_size = 10;
const max_node_size = 40;
const node_colour_scale = d3.scaleOrdinal(d3.schemeTableau10);

r2d3.onRender(function(graph, svg, width, height, options) {
    // set defaults for options
    const debug = options.debug == undefined ? false : options.debug;
    const use_size = options.use_size == undefined ? false : options.use_size;
    const use_weight_as_stroke = options.use_weight_as_stroke == undefined ? false : options.use_weight_as_stroke;
    const stroke_scale_factor = options.stroke_scale_factor == undefined ? 10 : options.stroke_scale_factor;
    const scale_weights = options.scale_weights == undefined ? false : options.scale_weights;
    const colour_nodes = options.colour_nodes == undefined ? false : options.colour_nodes;
    const charge_strength = options.charge_strength == undefined ? -100 : options.charge_strength;
    const link_distance = options.link_distance == undefined ? false : options.link_distance;
    const link_strength = options.link_strength == undefined ? false : options.link_strength;

    // First remove all previous elements
    svg.selectAll("g").remove();

    //draw lines for the links
    const links = svg.append("g")
        .attr("class", "links")
        .selectAll("line")
        .data(graph.edges)
        .enter().append("line")
          .attr("stroke-width", 2);

    // scale for link weights
    const link_scale = d3.scaleLinear(
        [ Math.min(...graph.edges.map(d => d.weight)), Math.max(...graph.edges.map(d => d.weight)) ],
        [ 0.1, 1 ]
    );
    if (scale_weights) {
      graph.edges.forEach(d => {
        d.weight = link_scale(d.weight);
      });
    }
    if (use_weight_as_stroke) {
      links.attr("stroke-width", d => d.weight*stroke_scale_factor);
    }

    //draw circles for the nodes
    const node_unselected_color = "#eeeeee";
    const nodes = svg.append("g")
        .attr("class", "nodes")
        .attr("cursor", "grab")
        .selectAll("circle")
        .data(graph.nodes)
        .join("circle")
        .attr("r", radius)
        .attr("cx", width / 2)
        .attr("cy", height / 2)
        .attr("fill",
            function(d) {
              if (colour_nodes) {
                return node_colour_scale(d.cluster_id);
              } else {
                if (d.cluster_id == 1) {
                    return node_colour_scale(d.cluster_id);
                } else {
                    return node_unselected_color;
                }
              }
            })
        // .attr("fill", function(d) { return color(d.cluster_id); })
        .attr("stroke", "#333333")
        .call(d3.drag()
          .on("start", dragstarted)
          .on("drag", dragged)
          .on("end", dragended))
          .on("click", colour_cluster);

    // scale for node size
    const node_size_scale =
      d3.scaleSqrt(
        [ Math.min(...graph.nodes.map(d => d.size)), Math.max(...graph.nodes.map(d => d.size)) ],
        [ min_node_size, max_node_size ]
      );
    // set size of nodes if using
    if (use_size) {
      nodes.attr("r", d => node_size_scale(d.size) ?? 10);
    }

    function dragstarted(event, d) {
        if (debug) {
          if (d.gene_name != undefined) {
            console.log(d.gene_name);
          }
        }
        d3.select(this)
            .raise()
            .attr("fill", "#000000");
        nodes.attr("cursor", "grabbing");
        if (!event.active) simulation.alphaTarget(0.3).restart();
        event.subject.fx = event.subject.x;
        event.subject.fy = event.subject.y;
    }
    function dragged(event) {
        event.subject.fx = event.x;
        event.subject.fy = event.y;
    }
    function dragended(event, d) {
        if (!event.active) simulation.alphaTarget(0);
        event.subject.fx = null;
        event.subject.fy = null;
        if (d3.select(this).classed("selected")) {
            d3.select(this)
                .attr("fill", node_colour_scale(d.cluster_id));
        } else {
            d3.select(this)
                .attr("fill", node_unselected_color);
        }
        nodes.attr("cursor", "grab");
    }

    const simulation = d3.forceSimulation()
      .force("link",
        d3.forceLink().id(function(d) { return d.node_idx; })
          .strength((d) => d.weight))
      .force("charge", d3.forceManyBody().strength(-100))
      .force("collision", d3.forceCollide((d) => d.size))
      .force("center", d3.forceCenter(width / 2, height / 2));

    simulation
    .nodes(graph.nodes)
    .on("tick", ticked);

    simulation.force("link")
        .links(graph.edges);

    function ticked() {
      //constrains the nodes to be within a box
      nodes
        .attr("cx", function(d) { return Math.max(radius, Math.min(width - radius, d.x)); })
        .attr("cy", function(d) { return Math.max(radius, Math.min(height - radius, d.y)); });

      links
        .attr("x1", function(d) { return d.source.x; })
        .attr("y1", function(d) { return d.source.y; })
        .attr("x2", function(d) { return d.target.x; })
        .attr("y2", function(d) { return d.target.y; });
    }

    // Colours
    function colour_cluster(event, d) {
        if (event.shiftKey) {
            cluster_idx = d.cluster_id;
            if (debug) {
                console.log(cluster_idx);
            }
            if (d3.select(this).classed("selected")) {
                svg.selectAll("circle")
                .filter(function(d){ return d.cluster_id ==  cluster_idx ? this : null; })
                .attr("fill", node_unselected_color)
                .each(function(){ this.classList.toggle("selected"); });
            } else {
                svg.selectAll("circle")
                .filter(function(d){ return d.cluster_id ==  cluster_idx ? this : null; })
                .attr("fill", node_colour_scale(d.cluster_id))
                .each(function(){ this.classList.toggle("selected"); });
            }
        }
        if (event.defaultPrevented) return; // dragged
    }

    // // Add zoom
    // const g = svg.selectAll("g");
    // function zoomed({transform}) {
    //     g.attr("transform", transform);
    // }
    // const zoom = d3.zoom()
    //     .extent([[0, 0], [width, height]])
    //     .scaleExtent([1, 40])
    //     .on("zoom", zoomed);

    // svg.call(zoom);

    // nodes.on("mouseover", node_info)
    //     .on("mouseout", function(){
    //         tooltip_div.transition()
    //             .duration(500)
    //             .style("opacity", 0);
    //     });

    // function node_info(event) {
    //     if (debug) {
    //         console.log(event)
    //         console.log(event.target.__data__)
    //     }
    //     d = event.target.__data__;
    //     let id;
    //     if (d.gene_name) {
    //       id = d.gene_name;
    //     } else {
    //       id = d.node_idx;
    //     }

    //     box_width = 40 + id.length*6 + 10;
    //     if (debug) {
    //         console.log(box_width);
    //     }
    //     tooltip_div.html("name = " + id + "<br/>"  + "cluster = " + d.cluster_id)
    //                 .style("left", (event.pageX) + "px")
    //                 .style("top", (event.pageY - 28) + "px")
    //                 .style("width", box_width);
    //     tooltip_div.transition()
    //         .duration(200)
    //         .style("opacity", '.9');
    // }

    // // Add functions for Zoom In, Out and Reset
    // d3.select("#zoom-in").on("click", function(){ svg.transition().call(zoom.scaleBy, 2); });
    // d3.select("#zoom-out").on("click", function(){ svg.transition().call(zoom.scaleBy, 0.5); });
    // d3.select("#reset-zoom").on("click", reset);
    // function reset() {
    //     svg.transition().duration(750).call(
    //       zoom.transform,
    //       d3.zoomIdentity,
    //       d3.zoomTransform(svg.node()).invert([width / 2, height / 2])
    //     );
    // }
});
