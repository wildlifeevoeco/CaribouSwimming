# Load drake
library(drake)

# Code to function
get_islands <- code_to_function('scripts/1-osm-islands.R')
extract_island_locs <- code_to_function('scripts/2-extract-island-locs.R')
generate_edges <- code_to_function('scripts/3-generate-edges.R')
fig_2 <- code_to_function('scripts/4-figure2.R')
tables <- code_to_function('scripts/5-tables.R')
render_md <- code_to_function('scripts/6-render-md.R')


# The plan
plan <- drake_plan(
  islands = get_islands(),
  locs = extract_island_locs(islands),
  edges = generate_edges(locs),
  fig2 = fig_2(edges),
  tabs = tables(edges),
  md = render_md(fig2, tabs)
)