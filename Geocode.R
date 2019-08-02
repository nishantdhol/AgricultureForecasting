devtools::install_github("dkahle/ggmap", ref = "tidyup", force=TRUE)
library(ggmap)
ggmap::register_google(key ="YOUR API KEY")
geocode("LOCATION")
