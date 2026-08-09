## Extract multiple bands and dates from google earth engine----
Extract_var_with_const_date <- function(df,path,subpath,band,select,
                                        begin = 5,
                                        end=7,
                                        unit="month",
                                        begin1 = 5,
                                        end1 =7,
                                        unit1 = "",
                                        buffer=1500,
                                        scale1=400) {
  
  
  
  ee_to_df <- function(ee_obj) {
    info <- ee_obj$getInfo()
    map_df(info$features, ~as.data.frame(.$properties, stringsAsFactors = FALSE))
  }
  
  standardize_coords <- function(df) {
    cols <- tolower(names(df))
    
    # Find latitude column
    lat_idx <- grep("^lat|latitud|^y$|northing", cols)[1]
    if (!is.na(lat_idx)) names(df)[lat_idx] <- "lat"
    
    # Find longitude column
    lon_idx <- grep("^lon|^lng|longitud|^x$|easting", cols)[1]
    if (!is.na(lon_idx)) names(df)[lon_idx] <- "lon"
    
    return(df)
  }
  
  loc<-standardize_coords(df)
  #print(batch_size)

  if(select!=T){
    
    if(nchar(subpath)==0) {
    image<-ee$Image(as.character(path))
    n_images <- 1
    } else if(nchar(subpath)>0) {
      image<-ee$Image(as.character(paste0(path,subpath)))
      if(nchar(band)>0) {
        image<-image$select(as.character(band))
        image
      }
      n_images<-1
    }
    
    
    if(nchar(band)==0) {
    image<-ee$ImageCollection(as.character(path))$select(as.character(subpath))$toBands()
    n_images <- image$bandNames()$length()$getInfo()
  }
  batch_size=round(sqrt(2500/(3.14*(buffer/scale1)^2)))*n_images
  } else if(select==T){
  batch_size=round(sqrt(2500/(3.14*(buffer/scale1)^2)))
    image<-ee$ImageCollection(as.character(path))$select(as.character(subpath))

    if(nchar(unit)>1) {
      image<-image$filter(
        ee$Filter$calendarRange(
          begin,
          end,
          unit
        )
      )
    }

    if(nchar(unit1)>1) {
      image<-image$filter(
        ee$Filter$calendarRange(
          begin1,
          end1,
          unit1
        )
      )
    }


  n_images <- image$size()$getInfo()
  }
  
  fill_closest_neighbors <- function(img) {
    # Radius = 1 targets the closest cardinal pixels
    neighborhood_average <- img$focalMean(
      radius = 4, 
      kernelType = "circle", 
      units = "pixels"
    )
    # Fill missing gaps using that neighborhood average
    return(img$unmask(neighborhood_average))
  }
  
  if (inherits(image, "ee.imagecollection.ImageCollection")) {
    image <- image$map(fill_closest_neighbors)
  } else {
    image <- fill_closest_neighbors(image)
  }
  
  
  
  

  num_iterations <- ceiling(nrow(loc) / batch_size)

  results<-list()
  # Loop through each batch
  for (i in 1:num_iterations) {
    # Calculate the start and end rows for the current batch
    start_row <- (i - 1) * batch_size + 1
    end_row <- min(i * batch_size, nrow(loc))
    print(paste("Processing locations", start_row,"-",end_row, "of", nrow(loc)))
    # Extract current batch
    batch <- ee$FeatureCollection(
      lapply(start_row:end_row, function(k) {
        props <- as.list(loc[k, ])
        ee$Feature(
          ee$Geometry$Point(c(loc$lon[k], loc$lat[k]), proj = "EPSG:4326"),
          props
        )
      })
    )$map(function(pt) {
      return(pt$buffer(buffer))
    })
    #print(batch)

    im_iter <- ceiling(n_images / batch_size)


    if(select!=T) {
      row<-image$sampleRegions(
        collection = batch,
        scale = scale1,
        projection = "EPSG:4326",
        geometries = F
      ) %>%
        ee_to_df()

      results[[paste(i)]]<-row
      #print(results)

    } else {
    for (j in 1:im_iter) {
      start_im <- (j - 1) * batch_size + 1
      end_im <- min(j * batch_size, n_images)

      print(paste("Processing images", start_im,"-",end_im, "of", n_images))

      # Get the i-th image (0-indexed in EE)
      img <- ee$ImageCollection$fromImages(image$toList(n_images)$slice(start_im,end_im))

    # Perform ee_extract for the current batch
    row <-img$map(
      ee_utils_pyfunc(function(img) {
        img_date <- img$date()$format("YYYY-MM-dd")

        sampled <- img$sampleRegions(
          collection = batch,
          scale = scale1,
          projection = "EPSG:4326",
          geometries = F
        )
        sampled$map(
          ee_utils_pyfunc(function(feature) {
            feature$set("image_date", img_date)
          })
        )
      })
     )$flatten() %>% ee_to_df()
    results[[paste(i,j)]]<-row
    }

    }

  }

  results<-do.call(rbind, results)


  results
}
