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
data_bdod_soil<-Extract_var_with_const_date(data_new,"projects/soilgrids-isric/",'bdod_mean',"bdod_0-5cm_mean",F,unit = "",buffer=1000,scale=250)
data_cec_soil<-Extract_var_with_const_date(data_new,"projects/soilgrids-isric/",'cec_mean',"cec_0-5cm_mean",F,unit = "",buffer=1000,scale=250)
data_cfvo_soil<-Extract_var_with_const_date(data_new,"projects/soilgrids-isric/",'cfvo_mean',"cfvo_0-5cm_mean",F,unit = "",buffer=1000,scale=250)
data_clay_soil<-Extract_var_with_const_date(data_new,"projects/soilgrids-isric/",'clay_mean',"clay_0-5cm_mean",F,unit = "",buffer=1000,scale=250)
data_nitrogen_soil<-Extract_var_with_const_date(data_new,"projects/soilgrids-isric/",'nitrogen_mean',"nitrogen_0-5cm_mean",F,unit = "",buffer=1000,scale=250)
data_ocd_soil<-Extract_var_with_const_date(data_new,"projects/soilgrids-isric/",'ocd_mean',"ocd_0-5cm_mean",F,unit = "",buffer=1000,scale=250)
data_phh2o_soil<-Extract_var_with_const_date(data_new,"projects/soilgrids-isric/",'phh2o_mean',"phh2o_0-5cm_mean",F,unit = "",buffer=1000,scale=250)
data_silt_soil<-Extract_var_with_const_date(data_new,"projects/soilgrids-isric/",'silt_mean',"silt_0-5cm_mean",F,unit = "",buffer=1000,scale=250)
data_sand_soil<-Extract_var_with_const_date(data_new,"projects/soilgrids-isric/",'sand_mean',"sand_0-5cm_mean",F,unit = "",buffer=1000,scale=250)
data_soc_soil<-Extract_var_with_const_date(data_new,"projects/soilgrids-isric/",'soc_mean',"soc_0-5cm_mean",F,unit = "",buffer=1000,scale=250)

soil_list <- mget(ls(pattern = "_soil$"), envir = .GlobalEnv)

soil_mean <- lapply(soil_list, function(df) {
  df |> 
    select(-c(lat,lon)) |> 
    group_by(surveyID) |>                       # key
    summarise(
      across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),  # numeric columns
      .groups = "drop"                            # drop the grouping afterwards
    )
})

joined_soil <- reduce(soil_mean, function(x, y) full_join(x, y,by='surveyID'))

# data source:
# https://developers.google.com/earth-engine/datasets/catalog/ESA_WorldCover_v200
data_land<-Extract_var_with_const_date(data_new,'ESA/WorldCover/v200',"Map",'',select=F,unit="",unit1="")

code_to_name <- c(
  "10" = "Tree cover",
  "20" = "Shrubland",
  "30" = "Grassland",
  "40" = "Cropland",
  "50" = "Buildings",
  "60" = "Bare",
  "70" = "Snow",
  "80" = "Water",
  "90" = "Herb Wetland",
  "95" = "Mangroves",
  "100" = "Moss"
)

print("Completed soils")

data_land <- data_land |> 
  mutate(
    # in case the column is numeric, turn it into character so the names in
    # `code_to_name` can be matched
    class_id = as.character(X2021_Map),
    
    # replace the codes with names; if a value is not in the mapping it
    # simply stays as‑is
    class_name = recode(X2021_Map, !!!code_to_name)
  ) %>%
  group_by(surveyID, class_name) |>        # one row per survey × cover type
  summarise(count = n(), .groups = "drop") |> 
  group_by(surveyID) |>                   # totals per survey
  mutate(prop = count / sum(count)) |>     # proportion of each cover type
  ungroup() %>% 
  arrange(surveyID, desc(prop)) |> 
  pivot_wider(names_from = class_name,
              values_from = prop,
              id_cols = surveyID)
  

print("Completed land types")

# data source:
# https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MOD13A2
data_NVDI<-Extract_var_with_const_date(data_new,'MODIS/061/MOD13A2','NDVI',"",T,unit="",unit1="",buffer=500) |> 
  mutate(Year=year(image_date)) |>
  summarise(.by= c(surveyID,Year),
            Yearly_NDVI=mean(NDVI))

print("Completed NDVI")
# data source:
# https://csidotinfo.wordpress.com/2019/01/24/global-aridity-index-and-potential-evapotranspiration-climate-database-v3/
# This raster was downloaded and then multiplied by 0.0001 to set the original scale.
# Then this raster was hosted on a personal GEE account.
data_Aridity<-Extract_var_with_const_date(data_new,'projects/ee-jakeberger92/assets/Aridity',"","Aridity",select=F,buffer = 500) |> 
  select(surveyID,b1) |> 
  summarise(.by=surveyID,
            b1=mean(b1))

print("Completed Aridity")
# data source:
# https://developers.google.com/earth-engine/datasets/catalog/WORLDCLIM_V1_BIO
data_clims<-Extract_var_with_const_date(data_new,'WORLDCLIM/V1/BIO',"","climate",select=F,buffer = 500) |> 
  summarise(.by = surveyID,
            across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

print("Completed climate")
# data source:
# https://developers.google.com/earth-engine/datasets/catalog/ECMWF_ERA5_LAND_MONTHLY_AGGR#bands
data_LAI_high<-Extract_var_with_const_date(data_new,'ECMWF/ERA5_LAND/MONTHLY_AGGR',"leaf_area_index_high_vegetation","",select=T,buffer = 500,unit="") |> 
  mutate(Year=year(image_date)) |>
  summarise(.by= c(surveyID,Year),
            Yearly_leaf_area_index_high_vegetation=mean(leaf_area_index_high_vegetation))


data_LAI_low<-Extract_var_with_const_date(data_new,'ECMWF/ERA5_LAND/MONTHLY_AGGR',"leaf_area_index_low_vegetation","",select=T,buffer = 500,unit="")|> 
  mutate(Year=year(image_date)) |>
  summarise(.by= c(surveyID,Year),
            Yearly_leaf_area_index_low_vegetation=mean(leaf_area_index_low_vegetation))

print("Completed leaf index")
# data source: NPP
# https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MOD17A3HGF
data_npp<-Extract_var_with_const_date(data_new,"MODIS/061/MOD17A3HGF",c("Npp","Npp_QC"),
                                              unit="",select = T,buffer = 1000) %>%   filter(Npp_QC <= 30) %>% 
  mutate(Year=year(image_date))%>% 
  summarise(.by = c(surveyID, Year),
            NPP_y=mean(Npp)/1e4)

print("Completed leaf NPP")
master_list <- list(
  joined_soil,
  data_land,
  data_NVDI,
  data_Aridity,
  data_clims,
  data_LAI_high,
  data_LAI_low,
  data_npp
)

data_final <- reduce(master_list, function(x, y) right_join(x, y))

# 4. Export file as .csv for further processing in script 03 ####
data_survey<-rbind(data_final,data_gradients) |> rename("Long"=lon,
                                                     "Lat"=lat,)
write.csv(data_survey, "data/data_survey_rawgradient.csv",row.names = F)


# Add code to only run new sample sites.
# I checked the new data and it is highly correlated with the old data.
# Add information about the code and data in the README file


