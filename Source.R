#Sourcing Genetic Data from UCSC
library(jsonlite)
library(dplyr)
library(tidyr)

get_Genes <- fromJSON("https://api.genome.ucsc.edu/getData/track?genome=hg19;track=knownGene;chrom=chr12")

geneframe <- tbl_df(as.data.frame(get_Genes))

gexons <- geneframe %>% select(knownGene.name, knownGene.exonCount, knownGene.exonStarts, knownGene.exonEnds)


bfa2 <- data.frame(name=rep(bfa$knownGene.name, bfa$knownGene.exonCount), exonStarts = unlist(strsplit(bfa$knownGene.exonStarts, ",")), exonEnds = unlist(strsplit(bfa$knownGene.exonEnds,",")))

temp=0

for(i in 1:length(unique(bfa2$name))){
  temp[i] = list(seq(from=1, to=bfa$knownGene.exonCount[i]))
}

bfa2 <- bfa2 %>% mutate(exonNum = unlist(temp),exonStarts = as.numeric(exonStarts), exonEnds = as.numeric(exonEnds), BPSZ = exonStarts-100) %>% select(name,exonNum,BPSZ,exonStarts,exonEnds)

head(bfa2, 50)
