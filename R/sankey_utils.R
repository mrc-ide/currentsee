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
add_caption <- function(widget, caption, font_size = 12, align = "center", colour = "#555", italic = TRUE) {
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
#' @param ensure_hover Logical; if TRUE, enable pointer events on links.
#' @export
#'
# Keyed link-tooltips: matches by (source,target) indices, not by order
# Keyed (by display names) + robust fallback + forced hover
add_link_tooltips <- function(widget, tooltips, source, target, ensure_hover = TRUE, delay_ms = 350) {
  stopifnot(length(tooltips) == length(source), length(source) == length(target))
  ensure_hover_js <- if (isTRUE(ensure_hover)) "true" else "false"

  js <- sprintf("
  function(el, x){
    var tips   = %s,
        srcIdx = %s,
        tgtIdx = %s;

    // Inject CSS to guarantee hover works (some themes disable it)
    d3.select(el).selectAll('style.__nd3_tip_fix__').data([1]).enter()
      .append('style').attr('class','__nd3_tip_fix__')
      .text('.link{pointer-events:stroke !important;}');

    // Build index->name from payload nodes (NodeID column becomes .name)
    var nodeNames = (x && Array.isArray(x.nodes))
      ? x.nodes.map(function(n){ return n && (n.name || n.id || n.node_name || ''); })
      : [];

    // Build a name-keyed map: 'srcName|tgtName' -> tooltip (first wins)
    var nameMap = {};
    for (var i=0;i<srcIdx.length;i++){
      var si = srcIdx[i], ti = tgtIdx[i];
      if (si == null || ti == null) continue;
      var sName = nodeNames[si] != null ? String(nodeNames[si]) : null;
      var tName = nodeNames[ti] != null ? String(nodeNames[ti]) : null;
      if (sName == null || tName == null) continue;
      var key = sName + '|' + tName;
      if (!(key in nameMap)) nameMap[key] = (tips[i] == null ? '' : String(tips[i]));
    }

    var root = d3.select(el);

    function apply(){
      var paths = root.selectAll('path.link, .link');
      if (paths.empty()) return;

      if (%s) paths.style('pointer-events','stroke');

      // For pairs rendered multiple times, use a counter per key
      var used = {};

      paths.each(function(d, i){
        var sel = d3.select(this);
        sel.selectAll('title').remove();

        // Try to read display names from bound d
        var sName = (d && d.source && (d.source.name || d.source.id)) || null;
        var tName = (d && d.target && (d.target.name || d.target.id)) || null;

        var tip = null;
        if (sName && tName){
          var key = String(sName) + '|' + String(tName);
          if (Object.prototype.hasOwnProperty.call(nameMap, key)){
            var k = (used[key] = (used[key] || 0));
            tip = nameMap[key];           // first wins; change to array & index if you want per-dup texts
            used[key] = k + 1;
          }
        }
        // Fallback: preserve your original order-based behaviour
        if ((tip == null || tip === '') && tips && tips.length > i) tip = tips[i];

        if (tip != null && tip !== '') sel.append('title').text(String(tip));
      });
    }

    // Apply after render + keep it applied on changes/resizes
    setTimeout(apply, %d);

    if (window && window.ResizeObserver){
      var ro = new ResizeObserver(function(){ setTimeout(apply, 0); });
      ro.observe(el);
    } else if (window){
      window.addEventListener('resize', apply);
    }

    // Re-apply when the SVG content mutates (layout/transition/redraw)
    var svg = el.querySelector('svg');
    if (window && window.MutationObserver && svg){
      var mo = new MutationObserver(function(){ setTimeout(apply, 0); });
      mo.observe(svg, {childList:true, subtree:true});
    }
  }",
                jsonlite::toJSON(tooltips, auto_unbox = TRUE, null = "null"),
                jsonlite::toJSON(source,   auto_unbox = TRUE),
                jsonlite::toJSON(target,   auto_unbox = TRUE),
                ensure_hover_js,
                as.integer(delay_ms)
  )

  htmlwidgets::onRender(widget, js)
}
# add_link_tooltips <- function(widget, tooltips, ensure_hover = TRUE) {
#   ensure_hover_js <- if (isTRUE(ensure_hover)) "true" else "false"
#
#   js <- sprintf("
#   function(el, x) {
#     var tips = %s;  // tooltip vector passed from R
#     var root = d3.select(el);
#
#     function apply() {
#       var paths = root.selectAll('path.link, .link');
#       if (paths.empty()) return;
#
#       if (%s) paths.style('pointer-events', 'stroke');
#
#       paths.each(function(d, i) {
#         var sel = d3.select(this);
#         sel.selectAll('title').remove();
#         if (tips && tips.length > i && tips[i] != null && tips[i] !== '') {
#           sel.append('title').text(String(tips[i]));
#         }
#       });
#     }
#
#     apply();
#     setTimeout(apply, 80);
#     if (window && window.ResizeObserver) {
#       var ro = new ResizeObserver(function(){ setTimeout(apply, 0); });
#       ro.observe(el);
#     } else if (window) {
#       window.addEventListener('resize', apply);
#     }
#   }",
#                 jsonlite::toJSON(tooltips, auto_unbox = TRUE, null = "null"),
#                 ensure_hover_js
#   )
#
#   htmlwidgets::onRender(widget, js)
# }

#' add_node_tooltips
#' @param widget A networkD3::sankeyNetwork() htmlwidget.
#' @param tooltips Character vector of tooltips, one per node (order must match Nodes).
#' @param ensure_hover Logical; if TRUE, enable pointer events on node rectangles.
#' @export
add_node_tooltips <- function(widget, tooltips, ensure_hover = TRUE) {
  ensure_hover_js <- if (isTRUE(ensure_hover)) "true" else "false"

  js <- sprintf("
  function(el, x) {
    var tips = %s;  // tooltip vector passed from R
    var root = d3.select(el);

    function apply() {
      var rects = root.selectAll('.node rect');
      if (rects.empty()) return;

      if (%s) rects.style('pointer-events', 'all');

      rects.each(function(d, i) {
        var sel = d3.select(this);
        sel.selectAll('title').remove();
        if (tips && tips.length > i && tips[i] != null && tips[i] !== '') {
          sel.append('title').text(String(tips[i]));
        }
      });
    }

    apply();
    setTimeout(apply, 80);
    if (window && window.ResizeObserver) {
      var ro = new ResizeObserver(function(){ setTimeout(apply, 0); });
      ro.observe(el);
    } else if (window) {
      window.addEventListener('resize', apply);
    }
  }",
                jsonlite::toJSON(tooltips, auto_unbox = TRUE, null = "null"),
                ensure_hover_js
  )

  htmlwidgets::onRender(widget, js)
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
