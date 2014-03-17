#
# name ch02.py
#

# create a new data frame

import numpy as np
import pandas as pd

# create a new data frame of hosts & high vuln counts
assets_df = pd.DataFrame( { 
    "name" : ["danube","gander","ganges","mekong","orinoco" ],
    "os" : [ "W2K8","RHEL5","W2K8","RHEL5","RHEL5" ],
    "highvulns" : [ 1,0,2,0,0 ] 
    } )

# take a look at the data frame structure & contents
print(assets_df.dtypes)
assets_df.head()
        
# show a "slice" just the operating systmes
assets_df.os.head()
    
# add a new column
assets_df['ip'] = [ "192.168.1.5","10.2.7.5","192.168.1.7",
                     "10.2.7.6", "10.2.7.7" ]
      
# show only nodes with more than one high vulnerabilty               
assets_df[assets_df.highvulns>1].head()

# divide nodes into network 'zones' based on IP address
assets_df['zones'] = np.where(
    assets_df.ip.str.startswith("192"), "Zone1", "Zone2")

# get one final view
assets_df.head()