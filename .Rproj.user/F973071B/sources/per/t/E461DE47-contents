#Creating different data frame and model for different crop.
for(i in unique(scaled_data$crop_name)){
  assign(paste("crop_name_",paste(i),sep = ""),subset.data.frame(scaled_data, crop_name == paste(i)))
  assign(paste("model_",paste(i),sep = ""),lm(myForm, data = subset.data.frame(scaled_data, crop_name == paste(i))))
}
# cropname <- readline(prompt = "Enter crop name: ")
# modelname <- paste("model_",paste(cropname),sep = "")
# Yield_Prediction <- predict(get(modelname),testmodel)
# Yield_Prediction*scales$Yield_Generated$sd + scales$Yield_Generated$mean

test <- function(newvalue){
  input_value <- newvalue
  input_query <- scaled_data[crop_name == input_value$crop,]
  set.seed(100)
  test_query <- input_query[sample(nrow(input_query),1)]
  # test_query$start_date <- sample(input_query$start_date,1)
  # test_query$end_date <- sample(input_query$end_date,1)
  # test_query$crop_name <- input_value$crop
  # test_query$total_geofenced_area <- sample(input_query$total_geofenced_area,1)
  # test_query$nitrogen <- sample(input_query$nitrogen,1)
  # test_query$phosphorous <- sample(input_query$phosphorous,1)
  # test_query$potassiom <- sample(input_query$potassiom,1)
  # test_query$moisture <- sample(input_query$moisture,1)
  # test_query$longitude <- sample(input_query$longitude,1)
  # test_query$latitude <- sample(input_query$latitude,1)
  # test_query$Total_Area <- sample(input_query$Total_Area,1)
  # test_query$temperatureMin <- sample(input_query$temperatureMin,1)
  # test_query$temperatureMax <- sample(input_query$temperatureMax,1)
  # test_query$visibilty <- sample(input_query$visibilty,1)
  # test_query$precipIntensity <- sample(input_query$precipIntensity,1)
  test_query$Yield_Generated <- NULL
  test_query$Total_Area <- (input_value$landsize - scales$Total_Area$mean)/scales$Total_Area$sd

  # test_query <- test_query[1,c(sample(input_query$start_date,1),sample(input_query$end_date,1),input_value$crop,sample(input_query$total_geofenced_area,1),sample(input_query$nitrogen,1),sample(input_query$phosphorous,1),sample(input_query$potassiom,1),sample(input_query$moisture,1),sample(input_query$longitude,1),sample(input_query$latitude,1),sample(input_query$Total_Area,1),sample(input_query$temperatureMin,1),sample(input_query$temperatureMax,1),sample(input_query$visibilty,1),sample(input_query$precipIntensity,1))]
  # test_query
  modelname <- paste("model_",paste(input_value$crop),sep = "")
  Yield_Prediction <- predict(get(modelname),test_query)
  Yield_Prediction*scales$Yield_Generated$sd + scales$Yield_Generated$mean
}
# test("Wheat","ahmd",1000)
# View(test_query)
# input_query <-NULL
