# Listing 8-5
# Redis log watcher python query script
# Save this as "lastseen.py"
# Start it in one shell window prompt with
#    python query.py
# Requires: Python redis package
from datetime import datetime
import redis
import pickle
import sys

# setup Redis connection
red = redis.StrictRedis(host='localhost', port=6379, db=0)

# get IP address from the command line & query Redis
ipaddr = sys.argv[1]
ioc = red.get("ip:%s" % ipaddr)

# if found
if (ioc != None):
      b = pickle.loads(ioc)
      print("IP [%s] was last seen on [%s].\nTotal times seen ")
      print("since we started counting: [%d]." %
            (ipaddr, datetime.fromtimestamp(b['ls']),b['ct']))
else:
   print("%s has not been seen, yet." % ipaddr)
