# Listing 8-6
# Retrieve VCDB files, import into mongo and perform a query
# Requires mongodb and git
# clone the VCDB github repository

git clone https://github.com/vz-risk/VCDB.git

# import all the incdients
cd VCDB/incidents
ls | head -5

## 0012CC25-9167-40D8-8FE3-3D0DFD8FB6BB.json
## 002599D4-A872-433B-9980-BD9F257B283F.json
## 005C42A3-3FE8-47B5-866B-AFBB5E3F5B95.json
## 0096EF99-D9CB-4869-9F3D-F4E0D84F419B.json
## 00CC39F6-D2E0-4FF4-9383-AE3E28922015.json

for f in *.json ; do mongoimport -d veris -c public --jsonArray $f ; done

# find all financial firms with security incident in the VCDB
# 52 is NAICS code for financial firms

echo 'db.public.find({"victim.industry": { $regex : "^52" } }, 
                        { "victim.victim_id" : 1, _id : 0 } )' | mongo veris

## { "victim" : [  
## {  "victim_id" : "Blue Cross & Blue Shield of Rhode Island" } ] }
## { "victim" : [  
## {  "victim_id" : "Group Health Incorporated" } ] }
## { "victim" : [ 
## { "victim_id" : "Delta Dental of Pennsylvania" },
## { "victim_id" : "ZDI" } ] }
## { "victim" : [  
## {  "victim_id" : "UK National Health Service" } ] }
## { "victim" : [ 
## { "victim_id" : "Mundo.com" }, 
## { "victim_id" : "Public Defender of Venezula" }, 
## { "victim_id" : "Caroni Seguros SA" } ] }

