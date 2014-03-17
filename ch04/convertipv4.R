#Convert IPv4 addresses to/from 32-bit integers
library(bitops)

# Define function for converting IP address to/from integers
# take IP address string in dotted octets and convert it to
# a 32-bit long integer
ip2long <- function(ip){
  #convert string into vector of chars
  ips <- unlist(strsplit(ip, '.', fixed=TRUE))
  # set up a function to bit-shift, then "OR" the octets
  octet <- function(x,y) bitOr(bitShiftL(x, 8), y)
  # Reduce applies a function cumulatively left to right
  Reduce(octet, as.integer(ips))
}

# take a 320bit integer IP address and convert it to a dotted octet
long2ip <- function(longip){
  #reverse bit manupulation
  octet <- function(nbits) bitAnd(bitShiftR(longip, nbits),0xFF)
  #Map applies a function to each element of the argument
  # paste converts arguments to character and concatenates them
  paste(Map(octet, c(24,16,8,0)),sep="", collapse=".")
}