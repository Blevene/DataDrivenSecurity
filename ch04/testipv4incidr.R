# Define function for testing IP CIDR membership
# take an IP address (string) and a CIDR (string)and
# return whether the given IP address is in the CIDR range
# REQUIRES converipv4.R

ip.is.in.cidr <- function(ip, cidr) {
  long.ip <- ip2long(ip)
  cidr.parts <- unlist(strsplit(cidr, "/"))
  cidr.range <- ip2long(cidr.parts[1])
  cidr.mask <- bitShiftL(bitFlip(0),
                         (32-as.integer(cidr.parts[2])))
  return(bitAnd(long.ip, cidr.mask) == bitAnd(cidr.range, cidr.mask))
}