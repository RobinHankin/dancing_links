# THIS FILE IS SUPERCEDED BY code3.R
# THIS FILE IS SUPERCEDED BY code3.R
# THIS FILE IS SUPERCEDED BY code3.R
# THIS FILE IS SUPERCEDED BY code3.R
# THIS FILE IS SUPERCEDED BY code3.R
# THIS FILE IS SUPERCEDED BY code3.R
# THIS FILE IS SUPERCEDED BY code3.R

# This file is based on dancing_links/butterfly_puzzle/code2.R, with the butterfly-puzzle-specific stuff replaced with fish-specific stuff.

# This file creates 'data2.txt'  for use with robindance.w.
# It supercedes code.R.

# The basic structure is: define functions fournums(), tabs(),
# check(), nums(), gettabs(), alltabs(), where(), and get_piece().
# Then a list called DLlist is created of length 25*25*4=2500, and
# then DLlist is written to data2.txt.

require(magic)


# Function fournums() takes an integer argument corresponding to the
# big numbers in butterfly_puzzle.svg, and returns an integer vector
# of length 4.  Consider f(1) for example.  This returns c(61,186,
# 90,165).  These are the leftmost numbers of the four sides in
# clockwise order (N,E,S,W) ["leftmost" means that you are looking at
# the piece oriented so the edge in question is uppermost].  A "NA"
# means that the edge does not exist because it is off the end of the
# puzzle (the puzzle does not specify anything about the edges of the
# arrangement).


fournums <- function(n){
       if(n==01){out <- c( NA,187,041, NA) } # top left corner
  else if(n==02){out <- c( NA,217,046,191) } # top edge
  else if(n==03){out <- c( NA,247,051,221) } # top edge
  else if(n==04){out <- c( NA,277,056,251) } # top edge
  else if(n==05){out <- c( NA,307,061,281) } # top edge
  else if(n==06){out <- c( NA, NA,066,311) } # top right corner

  else if(n==07){out <- c(037,192,071, NA) } # left edge
  else if(n==08){out <- c(042,222,076,196) } # interior point
  else if(n==09){out <- c(047,252,081,226) } # interior point
  else if(n==10){out <- c(052,282,086,256) } # interior point
  else if(n==11){out <- c(057,312,091,286) } # interior point
  else if(n==12){out <- c(062, NA,096,316) } # right edge

  else if(n==13){out <- c(067,197,101, NA) } # left edge
  else if(n==14){out <- c(072,227,106,201) } # interior point
  else if(n==15){out <- c(077,257,111,231) } # interior point
  else if(n==16){out <- c(082,287,116,261) } # interior point
  else if(n==17){out <- c(087,317,121,291) } # interior point
  else if(n==18){out <- c(092, NA,126,321) } # right edge

  else if(n==19){out <- c(097,202,131, NA) } # left edge
  else if(n==20){out <- c(102,232,136,206) } # interior point
  else if(n==21){out <- c(107,262,141,236) } # interior point
  else if(n==22){out <- c(112,292,146,266) } # interior point
  else if(n==23){out <- c(117,322,151,296) } # interior point
  else if(n==24){out <- c(122, NA,156,326) } # right edge
       
  else if(n==25){out <- c(127,207,161, NA) } # left edge
  else if(n==26){out <- c(132,237,166,211) } # interior point
  else if(n==27){out <- c(137,267,171,241) } # interior point
  else if(n==28){out <- c(142,297,176,271) } # interior point
  else if(n==29){out <- c(147,327,181,301) } # interior point
  else if(n==30){out <- c(152, NA,186,331) } # right edge

  else if(n==31){out <- c(157,212, NA, NA) } # lower left corner
  else if(n==32){out <- c(162,242, NA,216) } # bottom edge
  else if(n==33){out <- c(167,272, NA,246) } # bottom edge
  else if(n==34){out <- c(172,302, NA,276) } # bottom edge
  else if(n==35){out <- c(177,332, NA,306) } # bottom edge
  else if(n==36){out <- c(182, NA, NA,336) } # lower right corner
  else {stop("argument must be in range 1-36")}

       return(out)
}

allowable_side_types <- c(-7,-6,-5,-4,-3,-2,-1,1,2,3,4,5,6,7)

#Function tabs() takes an integer which is a type of side. Side "+n"
#fits into side "-n" for n=1..5.

# Remember that the pieces don't rotate in the fish puzzle.
# The code is bluef=1 goldf=2 green=3 johnd=4 strip=5 blugo=6 gold2=7 

# On the North and East sides of a piece, the numbers are positive
# On the South and West sides of a piece, the numbers are negative.


tabs <- function(n){

stopifnot(n %in% allowable_side_types)
  
       if(n == +1){out <- c(F,F,T,T,T) }
  else if(n == +2){out <- c(T,F,F,T,T) }
  else if(n == +3){out <- c(T,T,T,F,F) }
  else if(n == +4){out <- c(T,F,T,F,T) }
  else if(n == +5){out <- c(F,T,T,T,F) }
  else if(n == +6){out <- c(T,T,F,T,F) }
  else if(n == +7){out <- c(T,F,T,T,F) }

  else if(n == -1){out <- c(F,F,F,T,T) }
  else if(n == -2){out <- c(F,F,T,T,F) }
  else if(n == -3){out <- c(T,T,F,F,F) }
  else if(n == -4){out <- c(F,T,F,T,F) }
  else if(n == -5){out <- c(T,F,F,F,T) }
  else if(n == -6){out <- c(T,F,T,F,F) }
  else if(n == -7){out <- c(T,F,F,T,F) }
  else {stop("argument should be in range 1-7 or -1 to -7")}
       return(out)
}

check <- function(n){stopifnot(xor(tabs(n),rev(tabs(-n))))}
sapply(seq_len(5),check)

#Function nums() takes an integer 'n' (1 to 36; a square position), an
#orientation (1 to 4 for NESW); and a side type (-7 to -1 or 1 to 7).
#It returns the numbers of the occupied tabs as per
#fish_6x6.svg, with NULL meaning 'nothing'.

nums <- function(n,orient,side_type){
  stopifnot(n >=  1)
  stopifnot(n <= 36)
  stopifnot(length(n)==1)

  stopifnot(orient == 1)
  stopifnot(length(orient) == 1)

  jj <- fournums(n)[orient]

    if(is.na(jj)){ #edge of puzzle: no need to match
      return(NULL) }
  
  if((jj %% 5)== 2){  # increasing, eg 42-43-44-45-46, or 317-318-319-320-321
    out <- seq(from=jj,length=5,by= +1)[tabs(side_type)]
  } else { # decreasing, eg 81-80-79-78-77
    out <- seq(from=jj,length=5,by= -1)[tabs(side_type)]
  }
  return(out)  
}


# Function getnums() takes a number n, 1<=n<=36 representing a place
# in the puzzle and four side_types, for the N,E,S,W sides
# respectively.  It returns the occupied cells.

gettabs <- function(n, side_types){
  stopifnot(n >=  1)
  stopifnot(n <= 36)
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
# is rotated four times clockwise.... NOT!  THE FISH 6x6 PUZZLE DOES NOT
# ALLOW THE PIECES TO ROTATE!

alltabs <- function(n,side_types,description){
  out <- list()
  for(i in 0){  # NB!  THE FISH PUZZLE DOES NOT ALLOW THE PIECES TO ROTATE!
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


a <- read.table("tiles_fish6x6.txt" , header=TRUE)


layout <- matrix(1:36,6,6,byrow=TRUE)


stopifnot(minmax(diff(sort(layout))))

where <- function(i){
  stopifnot(i >=  1)
  stopifnot(i <= 36)
  drop(which(i == layout,arr.ind=TRUE))
}

#Function get_piece() takes an integer and returns a list with first
#element being the description of the piece and the second a vector of
#length 4 holding the side types.
get_piece <- function(i){
  stopifnot(i >=  1)
  stopifnot(i <= 36)
  if(i<10){
    jj <- "0"
  } else {
    jj <- ""
  }
    i_string <- paste(jj,i,sep="")

  list(piece_description    = paste("piece_",a[i,1],a[i,2],sep=""),
       side_types           = unlist(a[i,3:6])
       )
}
   
DLlist <- list()
upto <- 36  # use smaller values for debugging

for(i in 1:upto){#'i' cycles through the 36 pieces
  jj <- get_piece(i)
  for(j in 1:upto){ # 'j' cycles through the 36 locations
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

{  #do first line
  jj <- c(
          outer(c("piece_puffa","piece_zebra","piece_angel","piece_nemo_","piece_color", "piece_coral"),1:6,paste,sep=""),
          paste("loc_",sprintf("%02i",1:25),sep=""),
          outer(c("puffa_row","zebra_row","angel_row","nemo__row","color_row","coral_row"),1:6,paste,sep=""),
          outer(c("puffa_col","zebra_col","angel_col","nemo__col","color_col","coral_row"),1:6,paste,sep=""),
          36:225
          )

  write(jj,file="data2.txt",ncolumns=length(jj),append=FALSE)
}

# Now the rest of the file.  The write() is at the end of the for loop.
for(i in 1:length(DLlist)){

  where_tabs <- DLlist[[i]]
  where_tabs_string <- paste(where_tabs,collapse=" ")
  
  u <- names(DLlist)[i]

  colour <- substr(u,7,10)
  piece  <- paste("piece_",substr(u,7,11),sep="")

  loc <- as.numeric(substr(u,22,23))
  loc_string <- paste("loc_",sprintf("%02i",loc),sep="")
  
  p_row <- as.character(where(loc)[1])
  p_col <- as.character(where(loc)[2])

  colour_row <- paste(colour,"_row",p_row,sep="")
  colour_col <- paste(colour,"_col",p_col,sep="")
    
  s <- paste(piece, loc_string, colour_row, colour_col, where_tabs_string,sep=" ")
  write(s,file="data2.txt",append=TRUE)
}


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


