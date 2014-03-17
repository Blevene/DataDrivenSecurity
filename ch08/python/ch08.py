# Listing 8-1
# This code sets up the python environemnt for the chapter
# set working directory to chapter location
# (change for where you set up files in ch 2)
setwd("~/book/ch08")
# make sure the packages for this chapter are installed


# Listing 8-3
# Requires: bsddb3 and Berkeley DB llibrary
# Python code to interface with BDB
from bsddb3 import db
import struct
import socket

# initialize and open BDB database
av_db = db.DB()
av_db.open('av.db',None,db.DB_BTREE, db.DB_DIRTY_READ)

# get first key/value pair
cursor = av_db.cursor()
av_rec = cursor.first()

# print it out to show it worked
print av_rec

## ('24.62.253.107', '43.2555,-70.8829')

av_db.close() # close BDB file
