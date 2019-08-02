library(RMySQL)
# R needs a full path to find the settings file.
rdb.settingsfile<-"C:\\Users\\hp\\Downloads\\RoughWork\\time-series-forecating-master\\AgricultureForecasting\\mysqldatabase.cnf"

#Database Name And Connection
rmysql.db<-"mycrop_in1"
crop_database<-dbConnect(RMySQL::MySQL(),default.file=rdb.settingsfile,group=rmysql.db) 

# list the table. This confirms we connected to the database.
dbListTables(crop_database)
# query to select table and store into dataframe.   
query <- paste("SELECT
farmer_crop_info.`user_id`, farmer_crop_info.`start_date`, farmer_crop_info.`end_date`, crop_master.`crop_name`, crop_master.`crop_name_en`, land_info.`total_land_size`,
               CASE WHEN land_info.`total_land_size_in`=1 THEN 'Guntha'
               WHEN land_info.`total_land_size_in`=2 THEN 'Vigha'
               WHEN land_info.`total_land_size_in`=3 THEN 'Acre'
               WHEN land_info.`total_land_size_in`=4 THEN 'Hectare'
               ELSE land_info.`total_land_size_in` END AS `land_size_unit`,
               land_info.`total_geofenced_area`, land_info.`soil_testing` , land_info.`nitrogen`, land_info.`phosphorous`, land_info.`potassiom`,
               land_info.`moisture`, map_geofense_points.`longitude`, map_geofense_points.`latitude`,
               farmer_crop_info.`total_produce`,
               CASE WHEN farmer_crop_info.`total_produce_in`=1 THEN 'Man'
               WHEN farmer_crop_info.`total_produce_in`=2 THEN 'kg'
               WHEN farmer_crop_info.`total_produce_in`=3 THEN 'Tonne'
               WHEN farmer_crop_info.`total_produce_in`=4 THEN 'Quintal'
               ELSE farmer_crop_info.`total_produce_in` END AS `total_product_unit`
               
               
               FROM farmer_crop_info
               LEFT JOIN crop_master ON farmer_crop_info.`crop_id` = crop_master.`id`
               LEFT JOIN land_info ON farmer_crop_info.`land_id`=land_info.`id`
               LEFT JOIN map_geofense_points ON farmer_crop_info.`land_id`=map_geofense_points.`land_id`;")
#print(query)
rs = dbSendQuery(crop_database,query)
dbRows<-dbFetch(rs)

#Saving Fetched rows to DataFrame
crop_data<- data.frame(dbRows)
View(crop_data)

#Clearing Result Set for freeing resource allocated.  
dbClearResult(rs)

#disconnect to clean up the connection to the database.
dbDisconnect(crop_database)

write.xlsx(
  crop_data,
  file = "Database_crop_data.xlsx",
  row.names = FALSE,
  append = F
)
