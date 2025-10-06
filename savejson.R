library(jsonlite)
library(dplyr)
library(lubridate)
library(raster)
library(marmap)


#create a 4d df to json file

df<-readRDS(file = '../../../Downloads/earthquake_data.RDS')

# Simplify datetime — convert to sequential numeric "time index"
df <- df %>%
  mutate(
    datetime = ymd_hms(originTime.UTC.),
    time = as.numeric(difftime(datetime, min(datetime), units = "days")),
    z = -abs(depth),   # negative so quakes appear below surface
    x = longitude,
    y = latitude,
    mag = magnitude,
    desc = description
  ) %>%
  select(x, y, z, mag, time, desc)


write_json(df, "data/quakes.json", pretty = TRUE, auto_unbox = TRUE)



#create the map details in json format
baltic_bathy <- getNOAA.bathy(25, 26.5, 36, 37, resolution = 0.1)
longitudes <- as.numeric(rownames(baltic_bathy))  # Extract  longitudes from row names
latitudes<- as.numeric(colnames(baltic_bathy))  # Extract latitudes from column names

depth_matrix <- as.matrix(baltic_bathy) #create a depth matrix for visualization
depth_matrix <- depth_matrix / 1000
##Separate positive and negative depth values for different visualization
depth_matrix_positive <- depth_matrix
depth_matrix_positive[depth_matrix < 0] <- NA  # Keep land

depth_matrix_negative <- depth_matrix
depth_matrix_negative[depth_matrix >= 0] <- NA  # Keep water

# Convert to JSON-ready lists
land <- list()
water <- list()

for (i in seq_along(longitudes)) {
  for (j in seq_along(latitudes)) {
    z_land <- depth_matrix_positive[i, j]
    z_water <- depth_matrix_negative[i, j]
    if (!is.na(z_land)) {
      land <- append(land, list(list(
        x = longitudes[i],
        y = latitudes[j],
        z = z_land
      )))
    }
    if (!is.na(z_water)) {
      water <- append(water, list(list(
        x = longitudes[i],
        y = latitudes[j],
        z = z_water
      )))
    }
  }
}

bathy_json <- list(land = land, water = water)
write_json(bathy_json, "baltic_bathy.json", pretty = TRUE, auto_unbox = TRUE)
