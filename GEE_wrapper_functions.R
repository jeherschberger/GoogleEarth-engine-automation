## Extract multiple bands and dates from google earth engine----
Extract_var_with_const_date <- function(loc,path,subpath,band,select,
                                        begin = 5,
                                        end=7,
                                        unit="month",
                                        begin1 = 5,
                                        end1 =7,
                                        unit1 = "",
                                        buffer=1500,
                                        scale=500) {
  batch_size=round(sqrt(6000/(3.14*(buffer/scale)^2)))
  #print(batch_size)

  if(select!=T){
    
    if(nchar(subpath)==0) {
    image<-ee$Image(as.character(path))
    n_images <- 1
    } else if(nchar(band)>0) {
      image<-ee$Image(as.character(paste0(path,subpath)))
      if(nchar(band)>0) {
        image<-image$select(as.character(band))
      }
      n_images <- 1
    }
    
    if(nchar(band)==0) {
    image<-ee$ImageCollection(as.character(path))$select(as.character(subpath))$toBands()
    print("hello")
    n_images <- 1
  }

  } else if(select==T){
    
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

  num_iterations <- ceiling(nrow(loc) / batch_size)

  results<-list()
  # Loop through each batch
  for (i in 1:num_iterations) {
    # Calculate the start and end rows for the current batch
    start_row <- (i - 1) * batch_size + 1
    end_row <- min(i * batch_size, nrow(loc))
    print(paste("Processing locations", start_row,"-",end_row, "of", nrow(loc)))
    # Extract current batch
    batch <- sf_as_ee(loc[start_row:end_row, ])$map(function(pt) {                         # pt is an ee.Feature
      return(pt$buffer(buffer)) 
      
    })
    #print(ee_as_sf(batch))

    im_iter <- ceiling(n_images / batch_size)


    if(select!=T) {
      row<-image$sampleRegions(
        collection = batch,
        scale = scale,
        geometries = T
      ) %>%
        ee_as_sf() %>% sf::st_drop_geometry()

      results[[paste(i)]]<-row
      print(results)

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
          scale = scale,
          geometries = T
        )
        sampled$map(
          ee_utils_pyfunc(function(feature) {
            feature$set("image_date", img_date)
          })
        )
      })
     )$flatten() %>% ee_as_sf() %>% sf::st_drop_geometry()
    results[[paste(i,j)]]<-row
    }

    }

  }

  results<-do.call(rbind, results)


  results
}
