library(rgee)
library(jsonlite)

# 1. Authenticate Earth Engine using your secret token string
creds_json <- Sys.getenv("EARTHENGINE_TOKEN")

if (creds_json != "") {
  cred_path <- expandPath("~/.config/earthengine")
  if (!dir.exists(cred_path)) dir.create(cred_path, recursive = TRUE)
  writeLines(creds_json, file.path(cred_path, "credentials"))
}

# 2. Initialize Earth Engine (Replace with your actual GCP Project ID)
# rgee automatically manages the Python backend connection via reticulate
ee_Initialize()
