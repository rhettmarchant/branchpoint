#Collecting genes and converting into individual exon coordinates
library(jsonlite)
library(tidyverse)
library(stringr)

#Collect ncbiRefSeq table for chr12 from UCSC API
#THIS DOES NOT GET ALL GENES FOR chr12 ONLY 750000 to 55700000
genecall <- paste("http://api.genome.ucsc.edu/getData/track?genome=hg38;track=ncbiRefSeq;chrom=chr12;start=750000;end=55700000")
genecall_JSON <- fromJSON(genecall)

#Convert API JSON to data.frame containing only ncbiRefSeq data
RefSeqdf <- data.frame(genecall_JSON$ncbiRefSeq)

#Create attributes to list each exon independently
#Assign a number to each exon based on the exon count and its position in the gene
exnCount=0
for(i in 1:length(unique(RefSeqdf$name))){exnCount[i] = list(seq(from=1, to=RefSeqdf$exonCount[i]))}

refs = rep(RefSeqdf$name, RefSeqdf$exonCount)
genenames = rep(RefSeqdf$name2, RefSeqdf$exonCount)
strands = rep(RefSeqdf$strand, RefSeqdf$exonCount)
exonNums = unlist(exnCount)
EXONStarts = as.numeric(unlist(strsplit(RefSeqdf$exonStarts, ",")))
EXONEnds = as.numeric(unlist(strsplit(RefSeqdf$exonEnds,",")))

#Extract dataframe from RefSeqdf containing reference number (ref), gene name(name), strand direction(strand)
#coordinates of exon start (exonStart), coordinates of exon end (exonEnd) and the branch point search zone 100nt downstream of exonStart (BPSZ)

allisoformExons <- data.frame(ref=refs,name=genenames,strand=strands,exonNum=exonNums,exonStarts = EXONStarts,exonEnds = EXONEnds)

#Create dataframes of forward and reverse exons for troubleshooting/development

fwdexons <- allisoformExons %>% filter(strand == "+") %>% mutate(BPSZ = exonStarts - 100)
rvsexons <-  allisoformExons %>% filter(strand == "-") %>% mutate(BPSZ = exonEnds + 100)
allisoformExons <- rbind(fwdexons,rvsexons)

#Remove all duplicate exons i.e. multiple isoforms
collector <- 0
newExons <- 0
for(i in seq_along(allisoformExons$name)){
    if(!(paste(allisoformExons$name[i],allisoformExons$exonNum[i],sep="_") %in% collector)){
        collector[i] <- paste(allisoformExons$name[i],allisoformExons$exonNum[i],sep="_")
        newExons[i] <- i
    }
}

#Create new data frame containing each gene exon only once
alluniqueExons <- allisoformExons[newExons,] %>% filter(!is.na(name))

#Create a searchable index for exonname
alluniqueExons <- alluniqueExons %>% mutate(exonname = paste(name,"#",exonNum, sep =""))
