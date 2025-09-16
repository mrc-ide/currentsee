#' @export
make_nodes <- function(x){
  # nodes
  nodes <- data.frame(name = sort(unique(x$package)))
  nodes$node_name <- sapply(nodes$name, function(x){
    gsub(", ", "\n",x)
  })
  nodes$id <- paste(0:(nrow(nodes) - 1))
  return(nodes)
}

#' @export
make_links <- function(x){
  links <- x |>
    dplyr::arrange(id, step) |>
    dplyr::mutate(
      next_package = dplyr::lead(package),
      next_step    = dplyr::lead(step),
      .by = "id"
    ) |>
    dplyr::count(package, next_package, step, name = "value") |>
    dplyr::mutate(
      source = match(package, nodes$name) - 1L,
      target = match(next_package, nodes$name) - 1L,
      label = gsub(", ", "\n", package),
      id = paste(source)
    ) |>
    dplyr::mutate(
      change = purrr::map2_chr(
        strsplit(package, ",\\s*"),
        strsplit(next_package, ",\\s*"),
        ~ {
          if (any(is.na(.y))) return(NA_character_)
          new <- setdiff(.y, .x)
          paste(new, collapse = ", ")
        }
      )
    ) |>
    dplyr::mutate(
      tooltip = paste("→: add ", change, "\n←: remove ", change, sep = "")
    )
  return(links)
}

#' @export
make_colours <- function(id){
  pal <- c(
    "#AEC6CF", # pastel blue
    "#FFB347", # pastel orange
    "#77DD77", # pastel green
    "#FF6961", # pastel red/coral
    "#CBAACB", # pastel lilac
    "#F49AC2", # pastel pink
    "#FFD1DC", # very pale rose
    "#B5EAD7", # pastel mint
    "#FFFACD", # pastel lemon
    "#ADD8E6", # soft sky blue
    "#E0BBE4", # pastel lavender
    "#F7CAC9", # blush pink
    "#BFD8B8"  # pastel sage green
  )

  # Build a D3 ordinal scale string
  colourScale <- sprintf(
    'd3.scaleOrdinal().domain(%s).range(%s)',
    jsonlite::toJSON(id, auto_unbox = TRUE),
    jsonlite::toJSON(pal[seq_along(id)], auto_unbox = TRUE)
  )

  return(colourScale)
}

#' @export
makes_sankey <- function(
    nodes,
    links,
    colours,
    font_size = 14,                 # px
    link_alpha = 0.35,              # 0..1 (used if value_scaled_alpha = FALSE)
    value_scaled_alpha = FALSE,     # if TRUE, alpha scaled by link value
    alpha_range = c(0.2, 0.8),      # used when value_scaled_alpha = TRUE
    node_width = 30,              # NULL = leave as-is, or numeric px e.g. 30
    split_newlines = TRUE,          # turn '\n' in labels into stacked <tspan> lines
    center_labels = TRUE,           # horizontally centre labels on each node
    place_labels_above = FALSE,     # if TRUE, nudge text above node
    label_y_offset = -6             # px shift when placing above
) {
  stopifnot(length(alpha_range) == 2)

  sn <- networkD3::sankeyNetwork(
    Links  = dplyr::filter(links, !is.na(next_package)),
    Nodes  = nodes,
    Source = "source",
    Target = "target",
    Value  = "value",
    NodeID = "node_name",
    NodeGroup = "id",
    LinkGroup = "id",
    colourScale = colours
  )

  sn$x$links$tooltip <- dplyr::filter(links, !is.na(next_package))$tooltip


  js_bool <- function(x) if (isTRUE(x)) "true" else "false"
  js_num_or_null <- function(x) if (is.null(x)) "null" else as.character(x)

  js <- sprintf('
function(el, x) {

  function applyLineBreaks(sel) {
    if (!%s) return;
    sel.each(function(){
      var self = d3.select(this);
      var text = self.text();
      if (text && text.indexOf("\\n") > -1) {
        var lines = text.split("\\n");
        self.text(null);
        for (var i = 0; i < lines.length; i++) {
          self.append("tspan")
              .text(lines[i])
              .attr("x", null)
              .attr("dy", i === 0 ? 0 : "1.2em");
        }
      }
    });
  }

  function setNodeWidth() {
    var desired = %s; // null or number
    if (desired === null) return;
    d3.select(el).selectAll(".node rect").attr("width", desired);
  }

  function recenterLabels() {
    var firstRect = d3.select(el).select(".node rect");
    if (firstRect.empty()) return;
    var nodeWidth = +firstRect.attr("width") || 24;

    var texts = d3.select(el).selectAll(".node text");

    if (%s) {
      texts.attr("text-anchor", "middle")
           .attr("x", nodeWidth / 2);
      d3.select(el).selectAll(".node text tspan")
           .attr("x", nodeWidth / 2);
    }

    if (%s) {
      texts.attr("y", %d);
    }

    texts.style("font-size", "%dpx");
  }

  function setLinkOpacity() {
    var links = d3.select(el).selectAll(".link");

    if (%s) {
      var vals = (x.links || []).map(function(d){ return +d.value; }).filter(isFinite);
      if (vals.length) {
        var min = Math.min.apply(null, vals), max = Math.max.apply(null, vals);
        if (min === max) {
          links.style("stroke-opacity", %f);
        } else {
          var lin = (d3.scaleLinear || d3.scale.linear);
          var op  = lin().domain([min, max]).range([%f, %f]);
          links.style("stroke-opacity", function(d){ return op(+d.value); });
        }
      }
    } else {
      links.style("stroke-opacity", %f);
    }
  }

  function setTooltips() {
    // Replace default <title> on links
    var linkSel = d3.select(el).selectAll(".link");
    linkSel.select("title").remove();
    linkSel.append("title")
      .text(function(d){
        // If tooltip property exists, use it
        if (typeof d.tooltip !== "undefined" && d.tooltip !== null) return d.tooltip;
        // Fallback: source → target + value
        var fmt = (d3.format ? d3.format(",.0f") : function(x){return x;});
        var src = d.source && d.source.name ? d.source.name : "";
        var tgt = d.target && d.target.name ? d.target.name : "";
        return src + " → " + tgt + "\\n" + fmt(+d.value);
      });
  }

  // First pass
  var texts = d3.select(el).selectAll(".node text");
  applyLineBreaks(texts);
  setNodeWidth();
  recenterLabels();
  setLinkOpacity();
  setTooltips();

  // Second pass after layout to resist widget re-positioning
  requestAnimationFrame(function(){
    setNodeWidth();
    recenterLabels();
    setLinkOpacity();
    setTooltips();
  });
}
',
js_bool(split_newlines),
js_num_or_null(node_width),
js_bool(center_labels),
js_bool(place_labels_above), as.integer(label_y_offset),
as.integer(font_size),
js_bool(value_scaled_alpha),
as.numeric(link_alpha),
as.numeric(alpha_range[1]), as.numeric(alpha_range[2]),
as.numeric(link_alpha)
  )

  htmlwidgets::onRender(sn, js)
}
