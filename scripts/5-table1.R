### Table 1 ====
# Quinn M.R. Webber, Jack G. Hendrix, Alec L. Robitaille,  Eric Vander Wal


### Packages ----
library(data.table)



### Table 1 ----
tab1 <- data.table(
  island = c('Fogo Island', 'Western Perry Island', 'Eastern Perry Island'),
  movto = ,
  movfrom = ,
  proplic = ,
  density = ,
  grpsize = ,
  calfcow = 
)

setnames(tab1,
         c('',
           'Moves to',
           'Moves away',
           'Proportion lichen',
           'Estimated density (caribou per km2)',
           'Average group size',
           'Calf:cow ratio between June and August'))


### Output ----
saveRDS(tab1, 'output/table1.Rds')