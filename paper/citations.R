### Packages ----
library(knitcitations)

## only 15 citations

## Bergerud 1985
citep('10.1139/z85-199')

## Leblond et al 2016
citep('10.1186/s40462-016-0079-4')

### Write out bib ----
write.bibtex(file = "references/knitcite.bib")

