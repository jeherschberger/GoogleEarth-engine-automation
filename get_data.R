library(rgee)
library(jsonlite)

# 1. Authenticate Earth Engine using your secret token string
creds_json <- Sys.getenv("EARTHENGINE_TOKEN")

if (creds_json == "") {
  print("Please add GoogleEarth Engine secret key to repository!!")
}

# 2. Initialize Earth Engine (Replace with your actual GCP Project ID)
# rgee automatically manages the Python backend connection via reticulate
ee_Initialize(project = "ee-jakeberger92")
