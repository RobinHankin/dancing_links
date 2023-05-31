# This file creates 'data2.txt'  for use with robindance.w.
# It supercedes code.R and code2.R

# The basic structure is: define functions fournums(), tabs(),
# check(), nums(), gettabs(), alltabs(), where(), and get_piece().
# Then a list called DLlist is created of length 49*49*4, and
# then DLlist is written to data2.txt.

require(magic)

filename <- "edata2.txt"


# Function fournums() takes an integer argument corresponding to the
# big numbers in bugs_7x7.svg, and returns an integer vector
# of length 4.  Consider f(9) for example.  This returns c(55,300,94,269)
# These are the leftmost numbers of the four sides in
# clockwise order (N,E,S,W) ["leftmost" means that you are looking at
# the piece oriented so the edge in question is uppermost].  A "NA"
# means that the edge does not exist because it is off the end of the
# puzzle (the puzzle does not specify anything about the edges of the
# arrangement).  Eg, f1(1)=c(


fournums <- function(n){
       if(n==01){out <- c( NA,260,054, NA) } # top left corner
  else if(n==02){out <- c( NA,295,059,264) } # top edge
  else if(n==03){out <- c( NA,330,064,299) } # top edge
  else if(n==04){out <- c( NA,365,069,334) } # top edge
  else if(n==05){out <- c( NA,400,074,369) } # top edge
  else if(n==06){out <- c( NA,435,079,404) } # top edge
  else if(n==07){out <- c( NA, NA,084,439) } # top right corner
       
  else if(n==08){out <- c(050,265,089, NA) } # left edge, second row
  else if(n==09){out <- c(055,300,094,269) } # interior point
  else if(n==10){out <- c(060,335,099,304) } # interior point
  else if(n==11){out <- c(065,370,104,339) } # interior point
  else if(n==12){out <- c(070,405,109,374) } # interior point
  else if(n==13){out <- c(075,440,114,409) } # interior point
  else if(n==14){out <- c(080, NA,119,444) } # right edge, second row

  else if(n==15){out <- c(085,270,124, NA) } # left edge, third row
  else if(n==16){out <- c(090,305,129,274) } # interior point
  else if(n==17){out <- c(095,340,134,309) } # interior point
  else if(n==18){out <- c(100,375,139,344) } # interior point
  else if(n==19){out <- c(105,410,144,379) } # interior point
  else if(n==20){out <- c(110,445,149,414) } # interior point
  else if(n==21){out <- c(115, NA,154,449) } # right edge, third row

  else if(n==22){out <- c(120,275,159, NA) } # left edge, fourth row
  else if(n==23){out <- c(125,310,164,279) } # interior point
  else if(n==24){out <- c(130,345,169,314) } # interior point
  else if(n==25){out <- c(135,380,174,349) } # interior point
  else if(n==26){out <- c(140,415,179,384) } # interior point
  else if(n==27){out <- c(145,450,184,419) } # interior point
  else if(n==28){out <- c(150, NA,189,454) } # right edge, fourth row

  else if(n==29){out <- c(155,280,194, NA) } # left edge, fifth row 
  else if(n==30){out <- c(160,315,199,284) } # interior point
  else if(n==31){out <- c(165,350,204,319) } # interior point
  else if(n==32){out <- c(170,385,209,354) } # interior point
  else if(n==33){out <- c(175,420,214,389) } # interior point
  else if(n==34){out <- c(180,455,219,424) } # interior point
  else if(n==35){out <- c(185, NA,224,459) } # right edge, fifth row 

  else if(n==36){out <- c(190,285,229, NA) } # left edge, sixth row 
  else if(n==37){out <- c(195,320,234,289) } # interior point
  else if(n==38){out <- c(200,355,239,324) } # interior point
  else if(n==39){out <- c(205,390,244,359) } # interior point
  else if(n==40){out <- c(210,425,249,394) } # interior point
  else if(n==41){out <- c(215,460,254,429) } # interior point
  else if(n==42){out <- c(220, NA,259,464) } # right edge, sixth row 

  else if(n==43){out <- c(225,290, NA, NA) } # bottom left corner
  else if(n==44){out <- c(230,325, NA,294) } # bottom row
  else if(n==45){out <- c(235,360, NA,329) } # bottom row
  else if(n==46){out <- c(240,395, NA,364) } # bottom row
  else if(n==47){out <- c(245,430, NA,399) } # bottom row
  else if(n==48){out <- c(250,465, NA,434) } # bottom row
  else if(n==49){out <- c(255, NA, NA,469) } # bottom right corner
       
  else {stop("argument must be in range 1-49")}
       return(out)
}

allowable_side_types <- c((-9):(-1), 1:9)

# Function tabs() takes an integer which is a type of side. Side "+n"
# fits into side "-n" for n=1..7.  In the case of bugs, positive
# numbers 1..5 correspond to the right half of a bug, and negative
# numbers 1..5 correspond to the left half.  This convention is
# implemented in function convert_string_to_edgetype() below.  Numbers
# 6..9 and (-6)..(-9) are "special" edge types discussed below.  See
# how 'regular' edge types have either three T's and two F's, or two
# T's and three F's [depending on sig], while the special edge types
# have either four T's and one F, or one T and four F's.

"tabs" <- function(n){

  stopifnot(n %in% allowable_side_types)
  
       if(n == +1){out <- c(F,F,T,T,T) }
  else if(n == +2){out <- c(T,F,F,T,T) }
  else if(n == +3){out <- c(T,T,T,F,F) }
  else if(n == +4){out <- c(T,F,T,F,T) }
  else if(n == +5){out <- c(F,T,T,T,F) }
  else if(n == +6){out <- c(F,T,T,T,T) }  # "special" edge type
  else if(n == +7){out <- c(T,F,T,T,T) }  # "special" edge type
  else if(n == +8){out <- c(T,T,F,T,T) }  # "special" edge type
  else if(n == +9){out <- c(T,T,T,F,T) }  # "special" edge type

  else if(n == -1){out <- c(F,F,F,T,T) }
  else if(n == -2){out <- c(F,F,T,T,F) }
  else if(n == -3){out <- c(T,T,F,F,F) }
  else if(n == -4){out <- c(F,T,F,T,F) }
  else if(n == -5){out <- c(T,F,F,F,T) }
  else if(n == -6){out <- c(F,F,F,F,T) }  # "special" edge type
  else if(n == -7){out <- c(F,F,F,T,F) }  # "special" edge type
  else if(n == -8){out <- c(F,F,T,F,F) }  # "special" edge type
  else if(n == -9){out <- c(F,T,F,F,F) }  # "special" edge type
  
  else {stop("argument should be in range 1-5 or -1 to -5")}
       return(out)
}

check <- function(n){stopifnot(xor(tabs(n),rev(tabs(-n))))}
sapply(seq_len(5),check)


#Function nums() takes an integer 'n' (1 to 49; a square position), an
#orientation (1 to 4 for NESW); and a side type (-7 to -1 or 1 to 7).
#It returns the numbers of the occupied tabs as per
#bugs_7x7.svg, with NULL meaning 'nothing'.

nums <- function(n,orient,side_type){
  stopifnot(n >=  1)
  stopifnot(n <= 49)
  stopifnot(length(n)==1)

  stopifnot(orient >= 1)
  stopifnot(orient <= 4)
  stopifnot(length(orient) == 1)

  jj <- fournums(n)[orient]

    if(is.na(jj)){ #edge of puzzle: no need to match
      return(NULL) }
  
  if((jj %% 5)== 0){  # increasing,  eg 60-61-62-63-64-65 or 250-251-252-253-254-255
    out <- seq(from=jj,length=5,by= +1)[tabs(side_type)]
  } else if ((jj %% 5)==4){ # decreasing, eg 314-313-312-311-310
    out <- seq(from=jj,length=5,by= -1)[tabs(side_type)]
  } else {
    stop('number not correct modulo 5')
  }
  return(out)  
}

# Function getnums() takes a number n, 1<=n<=49 representing a place
# in the puzzle and four side_types, for the N,E,S,W sides
# respectively.  It returns the occupied cells.

gettabs <- function(n, side_types){
  stopifnot(n >=  1)
  stopifnot(n <= 49)
  stopifnot(length(n) == 1)

  stopifnot(length(side_types) == 4)
  stopifnot(all(side_types %in% allowable_side_types))

  out <- c(
           nums(n,1,side_types[1]),
           nums(n,2,side_types[2]),
           nums(n,3,side_types[3]),
           nums(n,4,side_types[4])
           )

  return(out)
}

# Function alltabs() takes an integer n representing a location in the
# puzzle, and a vector of side_types, as per gettabs() above.  A list
# of length four is returned, with each element an integer vector.
# Each element corresponds to one orientation of the piece.  The piece
# is rotated four times clockwise.

alltabs <- function(n,side_types,description){
  out <- list()
  for(i in 1:4){
    jj <- list(gettabs(n,side_types))

    out <- c(out , jj)
    side_types <- shift(side_types)  #ie rotate the piece
  }
  n_string <- sprintf("%02i",n)
  names(out) <- paste(description,
                      paste("location_",n_string,sep=""),
                      c("twelveclock",
                        "_threeclock",
                        "___sixclock",
                        "__nineclock"),sep="_")
  return(out)
}


# Now read the database and use f() to translate, eg, "ligrL" to -3,
# because ligr is type 3 [see levels<-() call in function
# convert_string_to_edgetype() below] and "L" means "left side of bug"
# which means that the sign is negative ("R" being positive).


a <- read.table("tiles_bugs7x7.txt" , header=TRUE)
# Dataframe is 'a' used in get_piece() below.

# see above near 'allowable_side_types' for details.
# Define a function to translate the names appearing in tiles_bugs_7x7.txt to numbers:

convert_string_to_edgetype <- function(x){
  y <- factor(substr(as.character(x),1,4))
  levels(y) <- c('blue','dagr','ligr','oran','redd')  # 'ligr' = 'light green' ; 'dagr' = 'dark green'
  signstring <- substr(as.character(x),5,5)
  signval <- rep(0,length(signstring))
  signval[signstring=='L'] <- -1L
  signval[signstring=='R'] <- +1L
  c(y) * signval
}

# Now translate the names appearing in tiles_bugs_7x7.txt to numbers:
a[,3] <- convert_string_to_edgetype(a[,3]) 
a[,4] <- convert_string_to_edgetype(a[,4]) 
a[,5] <- convert_string_to_edgetype(a[,5]) 
a[,6] <- convert_string_to_edgetype(a[,6]) 

# Now some "special" edge types, as detailed in special_edges.svg, and
# discussed with tabs() above.  These pieces have a 'watermark', which
# is a barely-visible 'O' on three of the pieces.  These pieces are
# allocated a special edge type, which only fit into each other:

use_watermark <- TRUE


# N=3, E=4, S=5, W=6


if(use_watermark){
  a[10,5] <- +6 # piece fire3, South side (was -5)
  a[29,5] <- -6 # piece lady1, South side (was +5)
  
  a[10,6] <- +7 # piece fire3, West  side (was +3)
  a[30,5] <- -7 # piece lady2, South side (was -3)
  
  a[09,6] <- +8 # piece fire2, West  side (was +4)
  a[30,6] <- -8 # piece lady2, West  side (was +4)

  a[29,4] <- +9 # piece lady1, East  side (was -1)
  a[09,3] <- -9 # piece fire2, North side (was +1)

}
layout <- matrix(1:49,7,7,byrow=TRUE)
stopifnot(minmax(diff(sort(layout))))

where <- function(i){
  stopifnot(i >=  1)
  stopifnot(i <= 49)
  drop(which(i == layout,arr.ind=TRUE))
}

#Function get_piece() takes an integer and returns a list with first
#element being the description of the piece and the second a vector of
#length 4 holding the side types.

get_piece <- function(i){
  stopifnot(i >=  1)
  stopifnot(i <= 49)
  if(i<10){  # pad with a leading zero if necessary
    jj <- "0"
  } else {
    jj <- ""
  }
    i_string <- paste(jj,i,sep="")

  list(piece_description    = paste("piece_",a[i,1],a[i,2],sep=""),
       side_types           = a[i,3:6]
       )
}
   
DLlist <- list()
upto <- 49  #use smaller values for debugging

for(i in 1:upto){  #'i' cycles through the 49 pieces
  jj <- get_piece(i)
  for(j in 1:upto){ # 'j' cycles through the 49 locations
    DLlist <- c(DLlist, alltabs(j,jj$side_types, jj$piece_description))
  }
}


# Use 'head(DLlist)' to see what this is.  The piece description and
# location and orientation are held in the names (separated by
# underscores) and the elements are vectors holding the locations of
# the tabs.  It has to be a list because the elements are not all the
# same length.

 # check for all names being same length:
stopifnot(minmax(sapply(names(DLlist),nchar)))

bugnames <- c("copp","fire","viol","emer","lady","gold","sapp")
stopifnot(minmax(nchar(bugnames)))

{  #do first line
  jj <- c(
          outer(paste("piece",bugnames,sep="_"),1:7,paste,sep=""),
          paste("loc_",sprintf("%02i",1:49),sep=""),
          outer(paste(bugnames,"row",sep="_"),1:7,paste,sep=""),
          outer(paste(bugnames,"col",sep="_"),1:7,paste,sep=""),
          paste("nwse",bugnames,sep="_"),
          paste("nesw",bugnames,sep="_"),
          50:469   # small numbers on bugs_7x7.svg
          )

  write(jj,file=filename,ncolumns=length(jj),append=FALSE)
}


# Now the rest of the file.  The write() is at the end of the for loop.
for(i in seq_along(DLlist)){

  where_tabs <- DLlist[[i]]
  where_tabs_string <- paste(where_tabs,collapse=" ")
  
  u <- names(DLlist)[i]
  colour <- substr(u,7,10) # note difference from butterfly/code2.R (due to longer fish type names)
  piece  <- paste("piece_",substr(u,7,11),sep="")  # note difference from butterfly

  loc <- as.numeric(substr(u,22,23))  # note difference from butterfly
  loc_string <- paste("loc_",sprintf("%02i",loc),sep="")
  
  p_row <- where(loc)[1]
  p_col <- where(loc)[2]

  colour_row <- paste(colour,"_row",as.character(p_row),sep="")
  colour_col <- paste(colour,"_col",as.character(p_col),sep="")
    
  s <- paste(piece, loc_string, colour_row, colour_col, where_tabs_string,sep=" ")


  # now ensure that precisely one piece of each type is on the NW-SE diagonal:
  if(p_row == p_col){  # ie, location is on NW-SE diagonal
    s <- paste(s, paste("nwse_",colour,sep=""),paste=" ")
  }

    if(p_row == 8-p_col){  # ie, on NE-SW diagonal
    s <- paste(s, paste("nesw_",colour,sep=""),paste=" ")
  }

  write(s,file=filename,append=TRUE)
}

# Following documentation refers to butterfly puzzle (5x5), not bugs.

# An example; the first line of data2.txt is
# piece_whit1 loc_01 whit_row3 whit_col3 61 63 65 186 188 189 89 88 86 164 163 161
# "piece_whit1" is the piece: white piece, number 1.  Last piece would be "piece_oran5"
# "01" is the location (recall that 1 is the middle of the board).  Last location is 25
# "whit_row3" says that there is a white piece in row 3.  
# "whit_col3" says that there is a white piece in column 3.
# Then the numbers 61,63,...,161 are the locations of the

# The first four rows of data.txt are identical apart from the tabs,
# because these are the same piece in the same location (viz, piece
# "white1" in location 1).  The four rows correspond to the four
# orientations that this piece may adopt without moving from location
# 1.
