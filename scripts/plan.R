# Load drake
library(drake)

# Code to function
get_islands <- code_to_function('scripts/1-osm-islands.R')
extract_island_locs <- code_to_function('scripts/2-extract-island-locs.R')
generate_edges <- code_to_function('scripts/3-generate-edges.R')
tables <- code_to_function('scripts/4-tables.R')
fig_2 <- code_to_function('scripts/5-figure2.R')
render_md <- code_to_function('scripts/6-render-md.R')
island_dist <- code_to_function('scripts/7-island-dist.R')
extract_lc <- code_to_function('scripts/8-extract-lc.R')
fig_s1 <- code_to_function('scripts/9-figureS1.R')
render_sup <- code_to_function('scripts/10-render-sup.R')

# The plan
plan <- drake_plan(
  islands = get_islands(),
  locs = extract_island_locs(islands),
  edges = generate_edges(locs),
  tabs = tables(edges),
  fig2 = fig_2(edges, tabs),
  md = render_md(fig2, tabs),
  isldist = island_dist(locs, edges),
  lc = extract_lc(locs),
  figs1 = fig_s1(locs, edges),
  sup = render_sup(lc, figs1)
)