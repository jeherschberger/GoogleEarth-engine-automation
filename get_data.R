library(jsonlite)
library(rgee)
library(tidyverse)

# 1. Authenticate Earth Engine using the secret token string stored on github secrets ----
creds_json <- fromJSON('private-key.json')

if (creds_json$private_key == "") {
  print("Please add GoogleEarth Engine secret key to repository!!")
}

reticulate::use_virtualenv('rgee')

# Initialize Earth Engine
ee <- reticulate::import("ee")
credentials <- ee$ServiceAccountCredentials(
  creds_json$client_email,
  'private-key.json'
)

ee$Initialize(credentials)

# 2. Load data and wrapper functions ----
## load survey-level data (summarized from script 01)

source("GEE_wrapper_functions.R")

data_survey <- read.csv('data/data_surveylevel_summarized.csv')

data_gradients<- read.csv('data/data_survey_rawgradient.csv')

data_new<-data_survey[!data_survey$surveyID %in% data_gradients$surveyID,]
head(data_new)
# 4. Extract gradient data ####

# The data was not re-scaled unless stated otherwise. 
# See the links for information on various variables
# data source: 
# https://git.wur.nl/isric/soilgrids/soilgrids.notebooks/-/blob/master/markdown/access_on_gee.md
data_soils<-Extract_var_with_const_date(data_new,"projects/soilgrids-isric/",'bdod_mean',"bdod_0-5cm_mean",F,unit = "",buffer=1000,scale=500)

# data source:
# https://developers.google.com/earth-engine/datasets/catalog/ESA_WorldCover_v200
data_land<-Extract_var_with_const_date(pp1,'ESA/WorldCover/v200',"Map",'',select=F,unit="",unit1="")

# data source:
# https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MOD13A2
data_NVDI<-Extract_var_with_const_date(pp1,'MODIS/061/MOD13A2','NDVI',"",T,unit="",unit1="",buffer=500)

# data source:
# https://csidotinfo.wordpress.com/2019/01/24/global-aridity-index-and-potential-evapotranspiration-climate-database-v3/
# This raster was downloaded and then multiplied by 0.0001 to set the original scale.
# Then this raster was hosted on a personal GEE account.
data_Aridity<-Extract_var_with_const_date(pp1,'projects/ee-jakeberger92/assets/Aridity',"","Aridity",select=F,buffer = 500)

# data source:
# https://developers.google.com/earth-engine/datasets/catalog/WORLDCLIM_V1_BIO
data_clims<-Extract_var_with_const_date(pp1,'WORLDCLIM/V1/BIO',"","climate",select=F,buffer = 500)

# data source:
# https://developers.google.com/earth-engine/datasets/catalog/ECMWF_ERA5_LAND_MONTHLY_AGGR#bands
data_LAI_high<-Extract_var_with_const_date(pp1,'ECMWF/ERA5_LAND/MONTHLY_AGGR',"leaf_area_index_high_vegetation","",select=T,buffer = 500,unit="")
data_LAI_low<-Extract_var_with_const_date(pp1,'ECMWF/ERA5_LAND/MONTHLY_AGGR',"leaf_area_index_high_vegetation","",select=T,buffer = 500,unit="")


# data source: NPP
# https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MOD17A3HGF
data_npp<-Extract_var_with_const_date(pp1,"MODIS/061/MOD17A3HGF",c("Npp","Npp_QC"),
                                              unit="",select = T,buffer = 1000)
data_npp<-data_npp %>%   filter(Npp_QC <= 30) %>% 
  mutate(Year=year(image_date))%>% 
  group_by(surveyID,Year) %>% 
  summarise(NPP_y=mean(Npp)/1e4)

#write.csv(data_npp, "data/Yearly_NPP_filtered_by_QC_30.csv",row.names = F)

# Any new variables would have to use the data_gradients data frame.
# This will add the new variable at the end. 
# Please contact Jacob Herschberger if new variables are needed.

#data_survey<-bind_rows(data_gradients,data_new)

# 4. Export file as .csv for further processing in script 03 ####
#write.csv(st_drop_geometry(data_survey), "data/data_survey_rawgradient.csv",row.names = F)


# Add code to only run new sample sites.
# I checked the new data and it is highly correlated with the old data.
# Add information about the code and data in the README file


