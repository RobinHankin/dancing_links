# This file is '70t.R'.  The "t" means "torus": it writes a script, to be used with 


filename <- "ftt.txt"  # ftt.txt means the p01_OK device is used
big_square_size <- 70   # ie a 70x70 big square.
little_square_size <- 24 # little squares are the pieces, 1x1, 2x2, ..., 24x24

require(magic)  # need the process() function


"padder" <- function(i){# pad with a leading zero if necessary
  if(i<10){  
    jj <- "0"
  } else {
    jj <- ""
  }
  paste(jj,i,sep="")
}


  

# write first line to <filename>:

locm <- as.matrix(expand.grid(seq_len(big_square_size),seq_len(big_square_size)))
locsx <- sapply(locm[,1],padder)
locsy <- sapply(locm[,2],padder)

locs <- paste(locsx,locsy,sep="_")
pieces <- paste(paste("p",sapply(seq_len(little_square_size),padder),sep=""),collapse=" ")

write(paste(c(pieces,locs,"p01_OK"),collapse=" "),
      file=filename,append=FALSE)



# that's the first line done, now for the other lines:

write("p01 01_01 p01_OK",file=filename,append=TRUE) # this forces the
                                                     # algorithm to
                                                     # put p01 [ie the
                                                     # 1x1 square] in
                                                     # location 01_01,
                                                     # because it has
                                                     # to match
                                                     # "p01_OK".


for(ss in seq_len(little_square_size)){   # 'ss'= 'square size'
  print(paste("ss=",ss))
  ss_string <- paste("p",padder(ss),sep="")
  for(ix in seq_len(big_square_size)){# pad (ix,iy)= coords of lower left corner
    for(iy in seq_len(big_square_size)){
      jj <- as.matrix(expand.grid(seq_len(ss),seq_len(ss)))
      jj <- sweep(jj,2,c(ix,iy)-1,"+")
      jj <- process(jj,big_square_size)
      jj1 <- sapply(jj[,1],padder)
      jj2 <- sapply(jj[,2],padder)
      places <- paste(paste(jj1,jj2,sep="_"),collapse=" ") # ie places occupied by square
      textstring <- c(paste(ss_string,places))
      write(textstring,file=filename,append=TRUE)
    }
  }
}
