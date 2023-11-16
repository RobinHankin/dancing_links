require(magic)


# Function fournums() takes an integer argument corresponding to the big
# numbers in butterfly_puzzle.svg, and returns an integer vector of
# length <=4.  Consider f(1) for example.  This returns c(61,186,
# 90,165).  These are the leftmost numbers of the four sides in
# clockeise order (N,E,S,W) ["leftmost" means that you are looking at
# the piece oriented so the edge in question is uppermost].



fournums <- function(n){
       if(n==01){out <- c(061,186,090,165) }
  else if(n==02){out <- c(066,211,095,190) }
  else if(n==03){out <- c(091,216,120,195) }
  else if(n==04){out <- c(086,191,115,170) }
  else if(n==05){out <- c(081,166,110,145) }
  else if(n==06){out <- c(056,161,085,140) }
  else if(n==07){out <- c(031,156,060,135) }
  else if(n==08){out <- c(036,181,065,160) }
  else if(n==09){out <- c(041,206,070,185) }
  else if(n==10){out <- c(046, NA,075,210) }
  else if(n==11){out <- c(071, NA,100,215) }
  else if(n==12){out <- c(096, NA,125,220) }
  else if(n==13){out <- c(121, NA, NA,225) }
  else if(n==14){out <- c(116,221, NA,200) }
  else if(n==15){out <- c(111,196, NA,175) }
  else if(n==16){out <- c(106,171, NA,150) }
  else if(n==17){out <- c(101,146, NA, NA) }
  else if(n==18){out <- c(076,141,105, NA) }
  else if(n==19){out <- c(051,136,080, NA) }
  else if(n==20){out <- c(026,131,055, NA) }
  else if(n==21){out <- c( NA,126,030, NA) }
  else if(n==22){out <- c( NA,151,035,130) }
  else if(n==23){out <- c( NA,176,040,155) }
  else if(n==24){out <- c( NA,201,045,180) }
  else if(n==25){out <- c( NA, NA,050,205) }
  else {stop("argument must be in range 1-25")}


       return(out)
}


allowable_side_types <- c(-5,-4,-3,-2,-1,1,2,3,4,5)

#Function tabs() takes an integer which is a type of side. Side "+n"
#fits into side "-n" for n=1..5.


tabs <- function(n){

stopifnot(n %in% allowable_side_types)
  
       if(n == +1){out <- c(T,F,T,F,T) }
  else if(n == +2){out <- c(T,F,T,T,F) }
  else if(n == +3){out <- c(F,T,T,F,T) }
  else if(n == +4){out <- c(F,T,T,F,T) }
  else if(n == +5){out <- c(F,T,T,T,F) }
  else if(n == -1){out <- c(F,T,F,T,F) }
  else if(n == -2){out <- c(T,F,F,T,F) }
  else if(n == -3){out <- c(F,T,F,F,T) }
  else if(n == -4){out <- c(F,T,F,F,T) }
  else if(n == -5){out <- c(T,F,F,F,T) }
  else {stop("argument should be in range 1-5 or -1 to -5")}
       return(out)
}

check <- function(n){stopifnot(xor(tabs(n),rev(tabs(-n))))}
sapply(seq_len(5),check)


#Function nums() takes an integer 'n' (1 to 25; a square position), an
#orientation (1 to 4 for NESW); and a side type (-5 to -1 or 1 to 5).
#It returns the numbers of the occupied tabs, with NULL meaning
#'nothing'.

nums <- function(n,orient,side_type){
  stopifnot(n >=  1)
  stopifnot(n <= 25)
  stopifnot(length(n)==1)

  stopifnot(orient >= 1)
  stopifnot(orient <= 4)
  stopifnot(length(orient) == 1)

  jj <- fournums(n)[orient]

    if(is.na(jj)){ #edge of puzzle: no need to match
      return(NULL) }
  
  if((jj %% 5)== 1){  # increasing, eg 126...130
    out <- seq(from=jj,length=5,by= +1)[tabs(side_type)]
  } else { # decreasing, eg 155...151
    out <- seq(from=jj,length=5,by= -1)[tabs(side_type)]
  }
    
  return(out)
  
}






# Function getnums() takes a number n, 1<=n<=25 representing a place in the puzzle and four side_types, for the N,E,S,W sides respectively.  It returns the occupied cells.

gettabs <- function(n, side_types){
  stopifnot(n >=  1)
  stopifnot(n <= 25)
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


a <- read.table("butterfly.txt" , header=TRUE)

layout <- matrix(c(
                   21,22,23,24,25,
                   20,07,08,09,10,
                   19,06,01,02,11,
                   18,05,04,03,12,
                   17,16,15,14,13),
                 5,5,byrow=TRUE)


stopifnot(minmax(diff(sort(layout))))

where <- function(i){
  stopifnot(i >=  1)
  stopifnot(i <= 25)
  drop(which(i == layout,arr.ind=TRUE))
}


  
#Function get_piece() takes an integer and returns a list with first
#element being the description of the piece and the second a vector of
#length 4 holding the side types.
get_piece <- function(i){
  stopifnot(i >=  1)
  stopifnot(i <= 25)
  if(i<10){
    jj <- "0"
  } else {
    jj <- ""
  }
    i_string <- paste(jj,i,sep="")

  list(piece_description    = paste("piece_",substr(a[i,1],1,2),a[i,2],sep=""),
       side_types           = a[i,3:6]
       )
}
   
DLlist <- list()

upto <- 25

for(i in 1:upto){#'i' cycles through the 25 pieces
  jj <- get_piece(i)
  for(j in 1:upto){ # 'j' cycles through the 25 locations
    DLlist <- c(DLlist, alltabs(j,jj$side_types, jj$piece_description))
  }
}

 # check for all names being same length:
stopifnot(minmax(sapply(names(DLlist),nchar)))

{  #do first line
  jj <- c(
          outer(c("wh","ye","bl","br","or"),1:5,paste,sep=""),
          sprintf("%02i",1:25),          
          outer(c("Wh","Ye","Bl","Br","Or"),1:5,paste,sep=""),
          outer(c("wH","yE","bL","bR","oR"),1:5,paste,sep=""),
          26:225
          )

  write(jj,file="data.txt",ncolumns=length(jj),append=FALSE)
}

for(i in 1:length(DLlist)){

  where_tabs <- DLlist[[i]]
  where_tabs_string <- paste(where_tabs,collapse=" ")
  
  u <- names(DLlist)[i]
  colour <- substr(u,7,8)
  piece  <- substr(u,7,9)

  loc <- as.numeric(substr(u,20,21))
  loc_string <- sprintf("%02i",loc)
  
  p_row <- as.character(where(loc)[1])
  p_col <- as.character(where(loc)[2])

  colour_row <- paste(colour,p_row,sep="")
  colour_col <- paste(colour,p_col,sep="")

  substr(colour_row,1,1) <- toupper(substr(colour_row,1,1))
  substr(colour_col,2,2) <- toupper(substr(colour_col,2,2))
 
  s <- paste(piece, loc_string, colour_row, colour_col, where_tabs_string,sep=" ")
  write(s,file="data.txt",append=TRUE)
}

# An example; the first line is
# wh1 01 Wh3 wH3 61 63 65 186 188 189 89 88 86 164 163 161
# "wh1" is the piece: white piece, number 1.  Goes to "or5"
# "01" is the location (recall that 1 is the middle of the board). Goes to 25
# "Wh3" says that there is a white piece in row 3.  "white" is "wh" and because this is a row constraint, the first letter is capitalized.
# "wH3" says that there is a white piece in column 3.  Because this is a row constraint, the first letter is capitalized.
# Then the numbers 61 to 161 are the locations of the tabs.
# The first four rows of data.txt are identical, because these are the same piece in the same location (viz, piece wh1 in location 1).  The four rows correspond to the four orientations that this piece may adopt.


