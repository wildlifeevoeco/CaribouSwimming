### Packages ----
library(knitcitations)

## only 15 citations

## Bergerud 1985
citep('10.1139/z85-199')


### Write out bib ----
write.bibtex(file = "references/knitcite.bib")

