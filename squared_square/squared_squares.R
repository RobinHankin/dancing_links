filename <- "f.txt"
big_square_size <- 112

little_squares <- c(50,35,27,8,19,15,17,11,2,6,29,25,9,7,18,24,16,33,4,37,42) # sides of little squares ("pieces")

stopifnot(sum(little_squares^2) == big_square_size^2)  #areas must match


padder <- function(i){# pad with a leading zero if necessary
  if(i<10){  
    jj <- "0"
  } else {
    jj <- ""
  }
  paste(jj,i,sep="")
}

# write first line

locm <- as.matrix(expand.grid(seq_len(big_square_size),seq_len(big_square_size)))
locsx <- sapply(locm[,1],padder)
locsy <- sapply(locm[,2],padder)

locs <- paste(locsx,locsy,sep="_")
pieces <- paste(paste("p",sapply(little_squares,padder),sep=""),collapse=" ")


write(paste(c(pieces,locs),collapse=" "),
      file=filename,append=FALSE)
# that's the first line done, now for the rest of the file.


 # now the rest of the file:
for(ss in little_squares){   # 'ss'= 'square size'
  print(paste("starting ss=",ss))
  ss_string <- paste("p",padder(ss),sep="")
  for(ix in seq_len(big_square_size-ss+1)){# pad (ix,iy)= coords of lower left corner
    for(iy in seq_len(big_square_size-ss+1)){
      jj <- as.matrix(expand.grid(seq_len(ss),seq_len(ss)))
      jj <- sweep(jj,2,c(ix,iy)-1,"+")
      jj1 <- sapply(jj[,1],padder)
      jj2 <- sapply(jj[,2],padder)
      places <- paste(paste(jj1,jj2,sep="_"),collapse=" ") # ie places occupied by square
      textstring <- c(paste(ss_string,places))
      write(textstring,file=filename,append=TRUE)
    }
  }
  print(paste("finished ss=",ss))

}


