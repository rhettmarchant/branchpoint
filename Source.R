#Sourcing Genetic Data from UCSC
library(jsonlite)
library(dplyr)
library(tidyr)

get_Genes <- fromJSON("https://api.genome.ucsc.edu/getData/track?genome=hg19;track=knownGene;chrom=chr12")

geneframe <- tbl_df(as.data.frame(get_Genes))

gexons <- geneframe %>% select(knownGene.name, knownGene.exonCount, knownGene.exonStarts, knownGene.exonEnds)

for (i in length(bfa$knownGene.exonStarts)){
  bfa2$knownGene.exonStarts[i] <- strsplit(bfa$knownGene.exonStarts[i], ",")
  bfa2$knownGene.exonEnds[i] <- strsplit(bfa$knownGene.exonEnds[i], ",")
}

for (j in length(bfa$knownGene.exonStarts[i])){
      bgfa <- data.frame(name=factor(),exonCount=integer(),exonStarts=integer(),exonEnds=integer())
      rbind(bgfa, name = bfa[1][i], exonCount = bfa[2][i], exonStarts = bfa[3][j], exonEnds = bfa$knownGene.exonStarts[4][j])
}

