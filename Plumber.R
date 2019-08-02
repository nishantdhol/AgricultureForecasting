# Plumber.R

#'@apiTitle Forecast API

#' Forecast the Yield of Crop.
#'@param crop Enter the Crop name.
#'@param location Enter the Location where crop is present.
#'@param landsize Enter the land Size.
#'@post /value
#'@json
function(crop , location , landsize) {
  landsize <- as.numeric(landsize)
  newvalue = data.frame(crop,location,landsize)
  if(newvalue$crop %in% unique_crop_name){
    test(newvalue)

  }
  else{
    print("Incorrect crop name or location")
  }
}

