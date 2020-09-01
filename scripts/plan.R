# Load drake
library(drake)

# Code to function
get_islands <- code_to_function('scripts/1-osm-islands.R')
extract_island_locs <- code_to_function('scripts/2-extract-island-locs.R')
generate_edges <- code_to_function('scripts/3-generate-edges.R')
fig_2 <- code_to_function('scripts/4-figure2.R')
table_1 <- code_to_function('scripts/5-table1.R')
render_md <- code_to_function('scripts/6-render-md.R')


# The plan
plan <- drake_plan(
  islands = get_islands(),
  locs = extract_island_locs(islands),
  edges = generate_edges(island_locs),
  fig2 = fig_2(edges),
  tab1 = table_1(edges),
  md = render_md(fig2, tab1)
)