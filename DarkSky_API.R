DarkSkyAPI <- function()
{
  api <- "enter api key here" #API KEY
  for (i in 1:nrow(df_replace)) {
    if (is.na(df_replace[i, "temperatureMin"]) == TRUE ||
        is.na(df_replace[i, "temperatureMax"]) == TRUE ||
        is.na(df_replace[i, "visibilty"]) == TRUE ||
        is.na(df_replace[i, "precipIntensity"]) == TRUE)
    {
      vlatitude <- as.numeric(df_replace[i, "latitude"])
      vlongitude <- as.numeric(df_replace[i, "longitude"])
      sd <- df_replace[i, "start_date"]
      ed <- df_replace[i, "end_date"]
      meandate <- sd + (ed - sd) / 2
      # meandate <- as.Date(sample_replace[i,"start_date"]+(sample_replace[i,"end_date"]-sample_replace[i,"start_date"])/2 ,format="%Y-%m-%d")
      # meandate <- as.Date(meandate,format="%Y-%m-%d")
      vdate  <-
        formatC(as.numeric(as.POSIXct(meandate, "%Y-%m-%d")), format =
                  "fg")
      # vdate
      weatherdata <-
        data.frame(fromJSON(
          paste(
            "https://api.darksky.net/forecast/",
            paste(api),
            paste("/"),
            paste(vlatitude),
            paste(","),
            paste(vlongitude),
            paste(","),
            paste(vdate),
            sep = "",
            "?exclude=minutely,currently,hourly,alerts,flags&units=si"
          )
        ))
      if (length(weatherdata) >= 10)
      {
        if ("daily.data.temperatureMin" %in% colnames(weatherdata)) {
          df_replace[i, "temperatureMin"] <-
            weatherdata$daily.data.temperatureMin
        }
        if ("daily.data.temperatureMax" %in% colnames(weatherdata)) {
          df_replace[i, "temperatureMax"] <-
            weatherdata$daily.data.temperatureMax
        }
        if ("daily.data.visibility" %in% colnames(weatherdata)) {
          df_replace[i, "visibilty"] <-
            weatherdata$daily.data.visibility
        }
        if ("daily.data.precipIntensity" %in% colnames(weatherdata)) {
          df_replace[i, "precipIntensity"] <-
            weatherdata$daily.data.precipIntensity
        }
      }
    }
  }
}
