import ee
import json

with open('private-key.json') as f:
          creds = json.load(f)
          credentials = ee.ServiceAccountCredentials(
                 creds['client_email'], 
                 'private-key.json'
                 )
ee.Initialize(credentials)
print('GEE authenticated via Python!')
