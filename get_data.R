library(jsonlite)
library(rgee)

# 1. Authenticate Earth Engine using the secret token string stored on github secrets ----
creds_json <- Sys.setenv(
  GOOGLE_APPLICATION_CREDENTIALS = normalizePath("sa_key.json")
)

if (creds_json == "") {
  print("Please add GoogleEarth Engine secret key to repository!!")
}

# 2. Initialize Earth Engine (Replace with your actual GCP Project ID) ----
# rgee automatically manages the Python backend connection via reticulate
ee_Initialize(project = "ee-jakeberger92",
             auth_mode = "appdefault",
             quiet = TRUE
             )

# 3. Load data and wrapper functions ----
## load survey-level data (summarized from script 01)

source("GEE_wrapper_functions.R")

data_survey <- read.csv('data/data_surveylevel_summarized.csv')
head(data_survey)

data_gradients<- read.csv('data/data_survey_rawgradient.csv')
head(data_gradients)

data_new<-data_survey[!data_survey$surveyID %in% data_gradients$surveyID,]

# 4. Extract gradient data ####

# The data was not re-scaled unless stated otherwise. 
# See the links for information on various variables
# data source: 
# https://git.wur.nl/isric/soilgrids/soilgrids.notebooks/-/blob/master/markdown/access_on_gee.md
data_new<-Gradient_extract(Data = data_new,GEE_path="projects/soilgrids-isric/",subpath='bdod_mean',Grad_var="Soil")

# data source:
# https://developers.google.com/earth-engine/datasets/catalog/ESA_WorldCover_v200
data_new<-Gradient_extract(Data = data_new,GEE_path = 'ESA/WorldCover/v200',Grad_var ="Landcover")

# data source:
# https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MOD13A2
data_new<-Gradient_extract(Data = data_new,GEE_path = 'MODIS/006/MOD13A2',Grad_var = 'NDVI',subpath = "NDVI")

# data source:
# https://csidotinfo.wordpress.com/2019/01/24/global-aridity-index-and-potential-evapotranspiration-climate-database-v3/
# This raster was downloaded and then multiplied by 0.0001 to set the original scale.
# Then this raster was hosted on a personal GEE account.
data_new<-Gradient_extract(Data = data_new,GEE_path='projects/ee-jakeberger92/assets/Aridity',Grad_var="Aridity")

# data source:
# https://developers.google.com/earth-engine/datasets/catalog/WORLDCLIM_V1_BIO
data_new<-Gradient_extract(Data = data_new,GEE_path='WORLDCLIM/V1/BIO',Grad_var="Climate")

# data source:
# https://developers.google.com/earth-engine/datasets/catalog/ECMWF_ERA5_LAND_MONTHLY_AGGR#bands
data_new<-Gradient_extract(Data = data_new,GEE_path='ECMWF/ERA5_LAND/MONTHLY_AGGR',Grad_var="LeafIndex")


# data source: NPP
# https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MOD17A3HGF
data_npp<-Extract_var_with_const_date_updated(pp1,"MODIS/061/MOD17A3HGF",c("Npp","Npp_QC"),
                                              unit="",select = T,buffer = 1000,Pop = "surveyID")
data_npp<-data_npp %>%   filter(Npp_QC <= 30) %>% 
  mutate(Year=year(image_date))%>% 
  group_by(surveyID,Year) %>% 
  summarise(NPP_y=mean(Npp)/1e4)

write.csv(data_npp, "data/Yearly_NPP_filtered_by_QC_30.csv",row.names = F)

# Any new variables would have to use the data_gradients data frame.
# This will add the new variable at the end. 
# Please contact Jacob Herschberger if new variables are needed.

data_survey<-bind_rows(data_gradients,data_new)

# 4. Export file as .csv for further processing in script 03 ####
write.csv(st_drop_geometry(data_survey), "data/data_survey_rawgradient.csv",row.names = F)


# Add code to only run new sample sites.
# I checked the new data and it is highly correlated with the old data.
# Add information about the code and data in the README file


