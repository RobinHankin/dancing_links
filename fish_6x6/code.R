# This file is based on dancing_links/butterfly_5x5/code.R and has had
# the butterfly-puzzle-specific stuff changed to fish-specific stuff.


# Function fournums() takes an integer argument corresponding to the
# big numbers in fish_6z6.svg, and returns an integer vector
# of length 4.  Consider f(1) for example.  This returns c(61,186,
# 90,165).  These are the leftmost numbers of the four sides in
# clockeise order (N,E,S,W) ["leftmost" means that you are looking at
# the piece oriented so the edge in question is uppermost].  A "NA"
# means that the edge does not exist because it is off the end of the
# puzzle (the puzzle does not specify anything about the edges of the
# arrangement).


fournums <- function(n){{
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

