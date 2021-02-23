# Code from live GIS workshop

# Load some libraries
library(tidyverse)

# Read in the fish atlas data
atlas <- read.csv("data/nysm-fish-atlas-database.csv",
                  stringsAsFactors = FALSE
                  )

# Shake hands with your data
str(atlas)

# Check out the names
names(atlas)

# Now, let's get dates instead of letters
str(atlas$date)

# Do this in base R
atlas$Date <- as.Date(
  x = as.character(atlas$date),
  format = "%m/%d/%Y"
)

# Do this with tidy workflow
atlas <- atlas %>%
  mutate(Date = as.Date(as.character(date), 
         format = "%m/%d/%Y"
         ))

# Let's make sure it worked
str(atlas$Date)


# Now we can add years
library(lubridate)

atlas$year <- year(atlas$Date)

# Summarize the atlas data
# by species, year, long, lat
atlas_sum <- atlas %>%
  group_by(species, year, x, y) %>%
  summarize(n_records = n(), .groups = "keep")


# Remove points with nonsensical long/lat
atlas_data <- atlas_sum %>%
  filter(
    !is.na(y) & !is.na(x) & x <= 0 & (y > 40 & y <= 60)
  )

# These are spatial libraries we'll use
library(rgdal)
library(sp)
library(sf)

# Let's read in the state outline
NY <- readOGR("data/NYS/State_Shoreline.shp", verbose = FALSE)

# Make a copy
temp_data <- atlas_data

# Now the magic
coordinates(temp_data) <- c("x", "y")

# Assign the projection string
proj4string(temp_data) <- CRS("+proj=longlat +datum=WGS84")

# Get CRS for NY shapefile
ny_crs <- proj4string(NY)

# Transorm CRS for temp_data
coord_utm <- spTransform(temp_data, CRS(ny_crs))

# Assign our new UTMs to the atlas_data
atlas_data$x <- coord_utm@coords[, 1]
atlas_data$y <- coord_utm@coords[, 2]
coordinates(atlas_data) <- ~ x + y
proj4string(atlas_data) <- CRS(ny_crs)

# Subset the data to get one species

sort(unique(atlas_data$species))

fish_data <- 
  atlas_data[atlas_data$species == "Alosa aestivalis", ]

# Let's build a map!
ggplot() + 
  geom_sf(data = st_as_sf(NY), fill = "gray87") +
  geom_point(
    data = data.frame(fish_data),
    mapping = aes(x = x, y = y, color = "tomato4"),
    alpha = 0.5, size = 2, show.legend = FALSE
  ) +
  labs(x = "Longitude", y = "Latitude")

# colors()

# What about by different years?
fish_data$Period <- "Historic"
fish_data$Period[fish_data$year >= 1976] <- "Recent"

# Let's build a map!
ggplot() + 
  geom_sf(data = st_as_sf(NY), fill = "gray87") +
  geom_point(
    data = data.frame(fish_data),
    mapping = aes(x = x, y = y, color = Period),
    alpha = 0.5, size = 2, show.legend = FALSE
  ) +
  labs(x = "Longitude", y = "Latitude") + 
  facet_wrap( ~ Period)



# What about by different years?
fish_data$Period <- "Historic"
fish_data$Period[fish_data$year >= 1976] <- "Middle"
fish_data$Period[fish_data$year >= 2000] <- "Recent"


# Let's build a map!
ggplot() + 
  geom_sf(data = st_as_sf(NY), fill = "gray87") +
  geom_point(
    data = data.frame(fish_data),
    mapping = aes(x = x, y = y, color = Period),
    alpha = 0.5, size = 2, show.legend = FALSE
  ) +
  labs(x = "Longitude", y = "Latitude") + 
  facet_wrap( ~ Period)













