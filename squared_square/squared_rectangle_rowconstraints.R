filename <- "rec4.txt"


{
big_square_size <- c(32,33)   #so c(32,33) means a 32-by-33 rectangle
little_squares <- c(18,15,14,4,7,8,10,9,1)
}


{
  big_square_size <- c(112,75)   #so c(32,33) means a 32-by-33 rectangle
  little_squares <- c(39,31,42,11,20,36,3,14,9,33,5,24,19)
}


stopifnot(sum(little_squares^2) == prod(big_square_size))  #areas must match


padder <- function(i){# pad with a leading zero if necessary
  if(i<10){  
    jj <- "0"
  } else {
    jj <- ""
  }
  paste(jj,i,sep="")
}

# write first line

locm <- as.matrix(expand.grid(seq_len(big_square_size[1]),seq_len(big_square_size[2])))
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
  for(ix in seq_len(big_square_size[1]-ss+1)){# pad (ix,iy)= coords of lower left corner
    for(iy in seq_len(big_square_size[2]-ss+1)){
      jj <- as.matrix(expand.grid(seq_len(ss),seq_len(ss)))
      jj <- sweep(jj,2,c(ix,iy)-1,"+")
      jj1 <- sapply(jj[,1],padder)
      jj2 <- sapply(jj[,2],padder)
      browser() 
      places <- paste(paste(jj1,jj2,sep="_"),collapse=" ") # ie places occupied by square
      textstring <- c(paste(ss_string,places))
      write(textstring,file=filename,append=TRUE)
    }
  } 
  print(paste("finished ss=",ss))
}
