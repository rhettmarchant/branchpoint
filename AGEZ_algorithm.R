#Returns the AG exclusion zone for sequences in get_Seqs tables
#CURRENTLY RETURNING:
#Error in if (Seq[i] %in% c("g", "G") & Seq[i - 1] %in% c("a", "A") & i <  : argument is of length zero

AGEZ <- function(y){
    AGEZdf <- data.frame(name=y$name,term=rep("0",nrow(y)),AG3SS=rep("0",nrow(y)),length=rep(0,nrow(y)),sequence=rep("0",nrow(y)), stringsAsFactors = FALSE)
    g <- y$sequence
    for(x in seq_along(g)){
        SeqL <- str_length(g[x])
        Seq <- strsplit(g[x],"")[[1]]
        #print(c(Seq[1:10], SeqL))
        errors <- 0
        counter <- 0
    
        if(Seq[SeqL] %in% c("g","G") & Seq[SeqL-1] %in% c("a","A")){
            AGEZdf$term[x] = "AG"
            for(i in seq(SeqL,1,-1)){
                if(i==1){
                    AGEZSeq <- Seq[seq(1,SeqL,1)]
                    AGEZdf$sequence[x] <- paste(AGEZSeq, sep="", collapse="")
                    AGEZdf$length[x] <- str_length(AGEZdf$sequence[x])
                    AGEZdf$AG3SS[x] <- TRUE
                    break
                }  else if(Seq[i] %in% c("g","G") & Seq[i-1] %in% c("a","A")){
                    counter <- counter + 1
                    if(counter == 2 & i>12){
                        AGEZSeq <- Seq[seq((i-12),SeqL,1)]
                        AGEZdf$sequence[x] <- paste(AGEZSeq, sep="", collapse="")
                        AGEZdf$length[x] <- str_length(AGEZdf$sequence[x])
                        AGEZdf$AG3SS[x] <- TRUE
                        break
                    }
                    else if(i == 1){
                        AGEZSeq <- Seq[seq(1,SeqL,1)]
                        AGEZdf$sequence[x] <- paste(AGEZSeq, sep="", collapse="")
                        AGEZdf$length[x] <- str_length(AGEZdf$sequence[x])
                        AGEZdf$AG3SS[x] <- TRUE
                        break
                    }

                } 
        
            }
        }else{
            AGEZdf$AG3SS[x] <- FALSE
            AGEZdf$term[x] <- paste(Seq[(SeqL-1):SeqL],sep="",collapse="")
            AGEZSeq <- Seq[seq(1,SeqL,1)]
            AGEZdf$sequence[x] <- paste(AGEZSeq, sep="", collapse="")
            AGEZdf$length[x] <- str_length(AGEZdf$sequence[x])
        }
    }
    tbl_df(AGEZdf %>% filter(AG3SS=="TRUE"))
    
}