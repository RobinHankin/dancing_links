#This file sets up a text file that solves the knapsack problem:
#enumerate the ways that
#$\sum_{i=1}^nx_i=N,x_i\in\mathbbm{N},x_i>0,x_i\neq x_j$ So for the
#70x70 square we seek S\subseteq[24] with sum_{i\in S}i=70.


filename <- "knapsack.txt"
S <- 1:10   # integers to choose from
N <- 20     # desired total


padder <- function(i){# pad with a leading zero if necessary
  if(length(i)>1){return(sapply(i,padder))}
  if(i<10){  
    jj <- "0"
  } else {
    jj <- ""
  }
  paste(jj,i,sep="")
}



pieces <- paste("p",padder(S),sep="")
locs <- paste("l",padder(seq_len(N)),sep="")

write(paste(c(locs,"|",pieces),collapse=" "),
      file=filename,append=FALSE)
# that's the first line done, now for the rest of the file.

for(i in S){
  for(j in seq_len(N-i+1)){  #eg 70-1+1
    piece <- paste("p",padder(i),sep="")
    locs <-    #"locs" for "locations"
      paste("l",padder(seq(from=j, to=j+i-1)),sep="")
    write(paste(c(piece,locs),collapse=" "),file=filename,append=TRUE)
  }
}

      

      
