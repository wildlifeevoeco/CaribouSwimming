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
  density = cI,
  grpsize = c('3.05 (2.7, 3.4)',
              '2.95 (1.89, 4.02)',
              '3.71 (1.97, 5.46)'),
  calfcow = c('0.34, (0.28, 0.39) (n = 283 groups)',
              '0.22, (0.05, 0.39) (n = 23 groups)',
              '0.67, (0.23, 1.00) (n = 7 groups)'),
  area = c(255, 7.5, 3.8)
)

tab1[, density := density / area]
tab1[, area := NULL]

setnames(tab1,
         c('',
           'Moves to',
           'Moves away',
           'Proportion lichen',
           'Estimated density (caribou per km2)',
           'Average group size (95% CI)',
           'Calf:cow ratio between June and August'))


### Output ----
saveRDS(tab1, 'output/table1.Rds')