### Packages ----
library(knitcitations)

## only 15 citations

## Bergerud 1985
citep('10.1139/z85-199')

## Bergerud 1990
citep('10.1016/S0003-3472(05)80882-6')

## Leblond et al 2016
citep('10.1186/s40462-016-0079-4')

## Miller and Gunn 1986 
citep('10.14430/arctic2052')

### Write out bib ----
write.bibtex(file = "references/knitcite.bib")

