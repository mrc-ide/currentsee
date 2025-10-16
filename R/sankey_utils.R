#' add_title
#' @param widget A networkD3::sankeyNetwork() htmlwidget.
#' @param title Character title text to place above the widget.
#' @param font_size Numeric font size in pixels.
#' @param align Character alignment: "left", "center", or "right".
#' @export
add_title <- function(widget, title, font_size = 18, align = "center") {
  htmlwidgets::prependContent(
    widget,
    htmltools::tags$h3(
      title,
      style = sprintf(
        "text-align:%s;margin:0 0 8px 0;font-weight:600;font-size:%spx;",
        align, font_size
      )
    )
  )
}

#' add_caption
#' @param widget A networkD3::sankeyNetwork() htmlwidget.
#' @param caption Character caption text to place below the widget.
#' @param font_size Numeric font size in pixels.
#' @param align Character alignment: "left", "center", or "right".
#' @param colour Character CSS colour for the caption text.
#' @param italic Logical; if TRUE, render caption in italics.
#' @export
add_caption <- function(widget, caption, font_size = 12, align = "center", colour = "#555", italic = FALSE) {
  htmlwidgets::appendContent(
    widget,
    htmltools::tags$p(
      caption,
      style = sprintf(
        "text-align:%s;margin:6px 0 0 0;font-size:%spx;color:%s;%s",
        align, font_size, colour,
        if (italic) "font-style:italic;" else ""
      )
    )
  )
}

#' add_linebreaks_in_labels
#' @param widget A networkD3::sankeyNetwork() htmlwidget.
#' @export
add_linebreaks_in_labels <- function(widget) {
  js <- "
  function(el) {
    var root = d3.select(el);
    root.selectAll('.node text').each(function() {
      var self = d3.select(this);
      var text = self.text();
      if (!text || text.indexOf('\\n') === -1) return;
      var lines = text.split('\\n');
      self.text(null);
      lines.forEach(function(line, i){
        self.append('tspan')
            .text(line)
            .attr('x', self.attr('x'))
            .attr('dy', i === 0 ? 0 : '1.2em');
      });
    });
  }"
  htmlwidgets::onRender(widget, js)
}

#' center_node_labels
#' @param widget A networkD3::sankeyNetwork() htmlwidget.
#' @param nudge Numeric horizontal pixel offset to fine-tune centring.
#' @export
center_node_labels <- function(widget, nudge = 0) {
  js <- sprintf("
  function(el) {
    var root = d3.select(el);

    function recenter() {
      root.selectAll('.node').each(function(d) {
        var node = d3.select(this);
        var rect = node.select('rect');
        var text = node.select('text');
        if (rect.empty() || text.empty()) return;

        // nodeD3 usually has rect x=0 inside translated <g>; width is node width
        var rectWidth = parseFloat(rect.attr('width')) ||
                        (d && (d.width || d.dx)) || 0;
        var centerX = rectWidth / 2 + (%f);

        text
          .attr('text-anchor', 'middle')
          .attr('x', centerX)
          .attr('dx', null);

        // If label has tspans (e.g. after line-breaking), keep them aligned too
        text.selectAll('tspan').attr('x', centerX);
      });
    }

    // Run after initial render and on resize
    setTimeout(recenter, 0);
    if (window) {
      var ro;
      if (window.ResizeObserver) {
        ro = new ResizeObserver(function(){ recenter(); });
        ro.observe(el);
      } else {
        window.addEventListener('resize', recenter);
      }
    }
  }", nudge)
  htmlwidgets::onRender(widget, js)
}

#' add_link_tooltips
#' @param widget A networkD3::sankeyNetwork() htmlwidget.
#' @param tooltips Character vector of tooltips, one per link (order must match Links).
#' @export
#'
# Keyed link-tooltips: matches by (source,target) indices, not by order
# Keyed (by display names) + robust fallback + forced hover
add_link_tooltips <- function(widget, tooltip) {
  widget$x$links$tooltip <- tooltip
  htmlwidgets::onRender(
    widget,
    '
  function(el, x) {
    d3.selectAll(".link").select("title")
    .text(function(d) { return d.tooltip; });
  }
  '
  )
}

#' add_node_tooltips
#' @param widget A networkD3::sankeyNetwork() htmlwidget.
#' @param tooltips Character vector of tooltips, one per node (order must match Nodes).
#' @export
add_node_tooltips <- function(widget, tooltip) {
  widget$x$nodes$tooltip <- tooltip
  htmlwidgets::onRender(
    widget,
    '
  function(el, x) {
    d3.selectAll(".node").select("title")
    .text(function(d) { return d.tooltip; });
  }
  '
  )
}

remove_ghost <- function(widget){
  htmlwidgets::onRender(
    widget,
    "
  function(el, x) {
    var root = d3.select(el);

    // Hide any node whose tooltip is exactly 'NA'
    root.selectAll('.node')
      .filter(function(d) {
        return d.tooltip === 'ghost';
      })
      .style('opacity', 0)
      .style('pointer-events', 'none');

    // Also hide any links connected to those nodes
    root.selectAll('path.link, .link')
      .filter(function(d) {
        var src = d.source && (d.source.tooltip === 'ghost');
        var tgt = d.target && (d.target.tooltip === 'ghost');
        return src || tgt;
      })
      .style('opacity', 0)
      .style('stroke-opacity', 0)
      .style('pointer-events', 'none');
  }
  "
  )
}


#' make_colour_scale
#' @param unknown Character CSS colour used for unmapped/unknown groups.
#' @export
make_colour_scale <- function(unknown = "#cccccc") {
  sprintf(
    "d3.scaleOrdinal().domain(%s).range(%s).unknown(%s)",
    jsonlite::toJSON(package_id$package_id, auto_unbox = TRUE),
    jsonlite::toJSON(package_id$colour, auto_unbox = TRUE),
    jsonlite::toJSON(unknown,      auto_unbox = TRUE)
  )
}
