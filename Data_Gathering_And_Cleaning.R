#Loading the required libraries and setting initial parameters#
# Setting the Work Directory
setwd(
  "C:\\Users\\hp\\Downloads\\RoughWork\\time-series-forecating-master\\AgricultureForecasting/"
)
needed_libs <- c("xlsx","readxl","dplyr","ggplot2","psych","Metrics","caret","DMwR","plyr","jsonlite","dataPreparation")

install_missing <- function(lib) {
  install.packages(lib, repos = "https://cran.r-project.org/", dependencies = TRUE)
  
  library(lib, character.only = TRUE)
}
#Installing libraries in a batch
for (lib in needed_libs){
  tryCatch(
    library(lib, character.only = TRUE),
    error = function(e)
      install_missing(lib)
  )
}

#Making Dataframe with required columns.
df <- setNames(data.frame(matrix(ncol = 17,nrow = 0)),c("start_date","end_date","crop_name","total_land_size","land_size_unit","total_land_size_conversion","total_geofenced_area","nitrogen","phosphorous","potassiom","moisture","longitude","latitude","total_produce","total_product_unit","total_produce_unit_conversion"))
#Copying Data from fetched DataFrame to Required DataFrame 
df<- crop_data[,c("start_date","end_date","crop_name","total_land_size","land_size_unit","total_geofenced_area","nitrogen","phosphorous","potassiom","moisture","longitude","latitude","total_produce","total_product_unit")]
# View(df)...3
# summary(df)

#Converting Attributes into suitable data types.
df$start_date <- as.Date(df$start_date)
df$end_date <- as.Date(df$end_date)
df$total_land_size <- as.numeric(df$total_land_size)
df$total_geofenced_area <- as.numeric(df$total_geofenced_area)
df$total_land_size_conversion <- NA
df$total_land_size_conversion <- as.numeric(df$total_land_size_conversion)
df$total_produce_unit_conversion <- NA 
df$total_produce_unit_conversion <- as.numeric(df$total_produce_unit_conversion)

unique_crop_name <- unique(df$crop_name)

#Converting land size and total produce amount into single unit amount. 
for(i in 1:nrow(df)){
  if(df[i,"land_size_unit"] == "Hectare"){
    df[i,"total_land_size_conversion"] <- 1 * df[i,"total_land_size"]
    if(df[i,"total_product_unit"] == "Tonne")
      df[i,"total_produce_unit_conversion"] <- 1 * df[i,"total_produce"]
    else if(df[i,"total_product_unit"] == "Man")
      df[i,"total_produce_unit_conversion"] <- 0.040 * df[i,"total_produce"]
    else if(df[i,"total_product_unit"] == "Quintal")
      df[i,"total_produce_unit_conversion"] <- 0.1 * df[i,"total_produce"]
    else if(df[i,"total_product_unit"] == "kg")
      df[i,"total_produce_unit_conversion"] <- 0.001 * df[i,"total_produce"]
  }  
  else if(df[i,"land_size_unit"] == "Vigha"){
    df[i,"total_land_size_conversion"] <- 0.16 * df[i,"total_land_size"]
    if(df[i,"total_product_unit"] == "Tonne")
      df[i,"total_produce_unit_conversion"] <- 1 * df[i,"total_produce"]
    else if(df[i,"total_product_unit"] == "Man")
      df[i,"total_produce_unit_conversion"] <- 0.040 * df[i,"total_produce"]
    else if(df[i,"total_product_unit"] == "Quintal")
      df[i,"total_produce_unit_conversion"] <- 0.1 * df[i,"total_produce"]
    else if(df[i,"total_product_unit"] == "kg")
      df[i,"total_produce_unit_conversion"] <- 0.001 * df[i,"total_produce"]
  }
}

#Renaming Columns by suitable names.
names(df)[names(df) == "total_land_size_conversion"] <- "Total_Area"
names(df)[names(df) == "total_produce_unit_conversion"] <- "Yield_Generated"
View(df)



# Finding the attributes with NA values
list_na <- colnames(df)[apply(df, 2, anyNA)]
list_na

# Create mean or median
average_missing <- apply(df[, colnames(df) %in% list_na],
                         2,
                         median,
                         na.rm =  TRUE)
average_missing


#Replace Missing value with Median/Mean. Here there are columns which have all null values so i am replacing with them using random numbers.w
df_replace <- df %>%
  mutate(
    # total_area = total_land_size * unit_conversion,
    # Yield_Generated = total_produce * unit_conversion_1,
    total_geofenced_area = ifelse(
      is.na(total_geofenced_area),
      round(average_missing[1], digits = 2),
      total_geofenced_area
    ),
    nitrogen  = ifelse(is.na(nitrogen),
                       round(average_missing[2], digits = 2),
                       nitrogen),
    phosphorous = ifelse(
      is.na(phosphorous),
      round(average_missing[3], digits = 2),
      phosphorous
    ),
    potassiom = ifelse(
      is.na(potassiom),
      # round(average_missing[4], digits = 2),
      sample (c(40:42), 100, replace = T), 
      potassiom
    ),
    moisture = ifelse(is.na(moisture),
                      # round(average_missing[5], digits = 2),
                      sample (c(5:45), 2435, replace = T),
                      moisture),
    longitude = ifelse(
      is.na(longitude),
      round(average_missing[6], digits = 2),
      longitude
    ),
    latitude = ifelse(is.na(latitude),
                      round(average_missing[7], digits = 2),
                      latitude)
    # Pest = ifelse(
    #   is.na(Pest),
    #   round(average_missing[AVERAGEVLAUEARRAYNUMBER], digits = 2),
    #   Pest),
    # `Pesticide Used(Yes/No)` = ifelse(
    #   is.na(`Pesticide Used(Yes/No)`),
    #   round(average_missing[AVERAGEVLAUEARRAYNUMBER], digits = 2),
    #   `Pesticide Used(Yes/No)`),
    # FertilizerUsed = ifelse(
    #   is.na(FertilizerUsed),
    #   round(average_missing[AVERAGEVLAUEARRAYNUMBER], digits = 2),
    #   FertilizerUsed
    # )
  )
View(df_replace)

df_replace$temperatureMin <- NA
df_replace$temperatureMax <- NA
df_replace$visibilty <- NA
df_replace$precipIntensity <- NA
df_replace$temperatureMin <- as.numeric(df_replace$temperatureMin)
df_replace$temperatureMax <- as.numeric(df_replace$temperatureMax)
df_replace$visibilty <- as.numeric(df_replace$visibilty)
df_replace$precipIntensity <- as.numeric(df_replace$precipIntensity)
df_replace$total_product_unit <- NULL
df_replace$land_size_unit <- NULL
df_replace$total_land_size <- NULL
df_replace$total_produce <- NULL

#Now the DarkSky API is used to gather climate data. Only 1000 request can be done per day by a user.
source("DarkSky_API.R")
DarkSkyAPI()
summary(df_replace)

#Adding data to missing values of climate data
list_na <- colnames(df_replace)[apply(df_replace, 2, anyNA)]
list_na

average_missing <- apply(df_replace[, colnames(df_replace) %in% list_na],
                         2,
                         median,
                         na.rm =  TRUE)
average_missing

df_replace_with_climate_data <- as.data.frame(df_replace)

df_replace_with_climate_data <- df_replace %>%
  mutate(
    temperatureMin = ifelse(
      is.na(temperatureMin),
      round(average_missing[1], digits = 2),
      temperatureMin
    ),
    temperatureMax = ifelse(
      is.na(temperatureMax),
      round(average_missing[2], digits = 2),
      temperatureMax
    ),
    visibilty = ifelse(
      is.na(visibilty),
      round(average_missing[3], digits = 2),
      visibilty
    ),
    precipIntensity = ifelse(
      is.na(precipIntensity),
      round(average_missing[4], digits = 2),
      precipIntensity
    )
  )

View(df_replace_with_climate_data)
summary(df_replace_with_climate_data)
# p_df_replace_with_climate_data <- df_replace_with_climate_data

# scaled_data_new <- data_frame(FALSE)s
scaled_data <- data.frame(df_replace_with_climate_data)
scales <- build_scales(dataSet = scaled_data,  verbose = TRUE)
# scales
scaled_data <- fastScale(dataSet = scaled_data, scales = scales, verbose = TRUE)
View(scaled_data)


# Scaling the data for normalization
# for (i in 1:length(colnames(df_replace_with_climate_data))) {
#   if (class(df_replace_with_climate_data[, i]) == "numeric" ||
#       class(df_replace_with_climate_data[, i]) == "integer") {
#     scaled_data[, i] <-
#       as.vector(scale(df_replace_with_climate_data[, i]))
#   }
# }

#describe(sample_replace)
View(scaled_data)
summary(scaled_data)
  # p_scaled_data <- scaled_data

# Saving the scaled dataframe into .xlsx file
write.xlsx(scaled_data,
           file = "df_replace_scaled_data.xlsx",
           row.names = FALSE,
           append = F)
