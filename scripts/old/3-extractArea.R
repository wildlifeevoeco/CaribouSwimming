### Island area vs ====
# Quinn M.R. Webber, Jack G. Hendrix, Alec L. Robitaille,  Eric Vander Wal

### Packages ----
libs <- c('data.table', 'ggplot2', 'dplyr',
          'raster', 'prettymapr', 'rosm', 
          'stars', 'fasterize', 'rgdal')
lapply(libs, require, character.only = TRUE)

## load files
duration <- fread("output/duration.csv")
r <- readRDS('output/islandsPoly.RDS')

## subset duration data
duration <- duration[, c("ANIMAL_ID","Year","StartIsland","counter")]



## calculate area


areaIsl <- data.table(st_area(r))
areaIsl$V1 <- areaIsl$V1/100000
areaIsl$StartIsland <- rownames(areaIsl)
areaIsl$StartIsland[areaIsl$StartIsland == 43] <- "Fogo"
#areaIsl$StartIsland[areaIsl$StartIsland == 53] <- "North Long"
areaIsl$StartIsland[areaIsl$StartIsland == 55] <- "North Long"
areaIsl$StartIsland[areaIsl$StartIsland == 58] <- "Blundon"
areaIsl$StartIsland[areaIsl$StartIsland == 67] <- "Brother"
areaIsl$StartIsland[areaIsl$StartIsland == 68] <- "W. Indian"
areaIsl$StartIsland[areaIsl$StartIsland == 70] <- "South Long"
areaIsl$StartIsland[areaIsl$StartIsland == 71] <- "E. Indian"
areaIsl$StartIsland[areaIsl$StartIsland == 74] <- "Kate"

areaIsl <- areaIsl[StartIsland == "Fogo" | StartIsland == "North Long" |
                   StartIsland == "Blundon" | StartIsland == "Brother" | 
                   StartIsland == "W. Indian" | StartIsland == "South Long" |
                   StartIsland == "E. Indian" | StartIsland == "Kate"]
areaIsl$

  strsplit(areaIsl$V1, " ")  
  
duration2 <- merge(duration, areaIsl, by = "StartIsland")

ggplot(duration) +
  geom_histogram(aes(log(counter/12), fill = ANIMAL_ID), alpha = 0.5) +
  facet_wrap(~StartIsland, scale = 'free')

ggplot(duration2) +
  geom_jitter(aes(as.numeric(V1), log(counter/12), 
                  color = factor(StartIsland)), alpha = 0.5, width = 10)
