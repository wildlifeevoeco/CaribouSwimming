### Packages ----
library(knitcitations)

## only 15 citations

## Bergerud 1985
citep('10.1139/z85-199')

## Bergerud and Page 1987
citep('10.1139/z87-249')

## Bergerud and Mercer 1989 - get actual doi
#citep('10.2307/3782635')

## Bergerud 1990
citep('10.1016/S0003-3472(05)80882-6')

## Bradbury et al 2015
citep('10.1093/beheco/arv078')

## Jeffrey et al 2007
citep('10.7557/2.27.4.320')

## Leblond et al 2016
citep('10.1186/s40462-016-0079-4')

## Miller et al 1977
citep('10.1139/z77-131')

## Miller and Gunn 1986 
citep('10.14430/arctic2052')

## Miller 2002 -- get actual doi
#citep('10.2307/40512446')

## Morris 1987
citep('10.2307/2937087')

## Schaefer 2016
citep('10.1093/jmammal/gyv184')

## Ricca et al 2012
citep('10.1007/s10530-012-0195-z')

## Webber and Vander Wal 2018
citep('10.1111/1365-2656.12773')

## van Beest et al 2014
citep('10.1111/1365-2656.12115')

### Write out bib ----
write.bibtex(file = "references/knitcite.bib")

