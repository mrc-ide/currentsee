devtools::load_all()
set.seed(1)
df <- simulate(11)
df
nodes <- make_nodes(df)
links <- make_links(df)
colours <- make_colours(nodes$id)
makes_sankey(nodes, links, colours)


launch_step_app(df)
