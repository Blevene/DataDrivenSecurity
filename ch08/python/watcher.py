# Listing 8-4
# Web server log watcher/Redis importer
# Save this as "watcher.py"
# Start it in one shell window prompt with
#    python watcher.py
# Requires: Python redis package
import time
import re
import redis
import pickle

# setup regex to parse web log entries
logparts = r'(\S+) (\S+) (\S+) \[(.*?)\] \
   "(\S+) (\S+) (\S+)" (\S+) (\S+)'
logpart = re.compile(logparts)

# map field names to extracted regex values
def field_map(dictseq,name,func):
   for d in dictseq:
      d[name] = func(d[name])
      yield d

# extract data from weblog
def web_log(lines):
   groups = (logpart.match(line) for line in lines)
   tuples = (g.groups() for g in groups if g)
   colnames = ('host','referrer','user',
               'datetime','method', 'request',
               'proto','status','bytes')
   log = (dict(zip(colnames,t)) for t in tuples)
   log = field_map(log,"bytes",
             lambda s: int(s) if s != '-' else 0)
   log = field_map(log,"status",int)
   return log

# "tail" for python
def follow(thefile):
   thefile.seek(0,2)
   while True:
      line = thefile.readline()
      if not line:
         time.sleep(0.1)
         continue
      yield line

# setup log watching
# change this to an active, accessible web server log
logfile = open("/var/log/nginx/access.log")
loglines = follow(logfile)
log = web_log(loglines)

# setup Redis connection
# for large environments, you will substitute
# localhost with a dedicated server host name
red = redis.StrictRedis(host='localhost',
                        port=6379, db=0)

# for each entry, store pythonic-data structure in
# associated with a key (could also use Redis hash
# for more language-independence)
for line in log:
   l = line['host']
   a = red.get("ip:%s" % l)
   if (a == None):
      a = {}
      a['ls'] = time.time()
      a['ct'] = 1
      red.set("ip:%s" % l,pickle.dumps(a))
   else:
      a = pickle.loads(a)
      a['ls'] = time.time()
      a['ct'] += 1
      red.set("ip:%s" % l,pickle.dumps(a))
