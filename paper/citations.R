### Packages ----
library(knitcitations)

## only 15 citations

## Bergerud 1985
citep('10.1139/z85-199')

## Bergerud 1990
citep('10.1016/S0003-3472(05)80882-6')

## Bradbury et al 2015
citep('10.1093/beheco/arv078')

## Leblond et al 2016
citep('10.1186/s40462-016-0079-4')

## Miller et al 1977
citep('10.1139/z77-131')

## Miller and Gunn 1986 
citep('10.14430/arctic2052')

## Morris 1987
citep('10.2307/2937087')

## Schaefer 2016
citep('10.1093/jmammal/gyv184')

### Write out bib ----
write.bibtex(file = "references/knitcite.bib")

