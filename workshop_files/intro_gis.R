library(tidyverse)


## **Data overview and management**

atlas <- read.csv("data/nysm-fish-atlas-database.csv", stringsAsFactors = FALSE)

# Like this:
str(atlas)


#Now that we have an idea of what the data set looks like, let's take a little closer look. First, there are a couple of things that we can do that will clean up our code down the road a little bit. Let's have a quick look at our column names again.

names(atlas)

## **Data manipulation**

atlas <- atlas %>%
  mutate(date = as.Date(as.character(date), format = "%m/%d/%Y"))

library(lubridate)
atlas$year <- year(atlas$date)

atlas_sum <- atlas %>%
  group_by(species, year, x, y) %>%
  summarize(n_records = n(), .groups = "keep")

# Remove points with missing or
# cray longitude (x) and latitude (y)
atlas_data <- atlas_sum %>%
  filter(!is.na(y) & !is.na(y) & x <= 0 & (y >= 40 & y <= 60))

## **Spatial data**
library(rgdal)
NY <- readOGR("data/NYS/State_Shoreline.shp", verbose = FALSE)

# Load sp package
library(sp)

# Make a copy that we will use for coordinate transformation
temp_data <- atlas_data

# Assign longitude and latitude to a
# SpatialPoints-class obj. This converts df to
# a spatial dataframe
coordinates(temp_data) <- c("x", "y")

proj4string(temp_data) <- CRS("+proj=longlat +datum=WGS84")

# Get the CRS for the NY shapefile so we can match them
ny_crs <- proj4string(NY)

# Get UTMs for the longitudes and latitudes using
# the coordinate system of our shape file
coord_utm <- spTransform(temp_data, CRS(ny_crs))

# Assign the coordinates to new columns
# in our dataframe
atlas_data$x <- coord_utm@coords[, 1]
atlas_data$y <- coord_utm@coords[, 2]
coordinates(atlas_data) <- ~ x + y
proj4string(atlas_data) <- CRS(ny_crs)

## **Plotting spatial data**
#Let's go ahead and grab a single species for this.

# If you want to see a list of species you can choose from, remember you can sort the unique values of `atlas$species` and scroll through them if you like and pick a fave. If you can't identify your favorite fish using it's Latin binomial, shame on you.

# Like this
species_in_atlas <- sort(unique(atlas_data$species))

# You could also print() this
head(species_in_atlas)

fish_data <- atlas_data[atlas_data$species == "Alosa aestivalis", ]

# We'll use the ggplot2 library for our plots
# and we'll need a couple of handy functions
# from the sf package, too.
library(ggplot2)
library(sf)

# Make the plot
ggplot() +
  geom_sf(data = st_as_sf(NY), fill = "gray97") +
  geom_point(
    data = data.frame(fish_data),
    mapping = aes(x = x, y = y, color = "tomato4", ),
    alpha = 0.8, size = 2, show.legend = FALSE
  ) +
  labs(x = "Longitude", y = "Latitude") +
  ggtitle(paste0(unique(fish_data$species), " distribution")) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = .5),
    text = element_text(size = 10)
  )

# Perform a spatial intersect between
# the NY shapefile (polygon) and
# the SpatialPoints object.
# Note that order is important here.
ins <- sp::over(fish_data, NY)

# Then, we can drop the points that
# do not not intersect with the polygon,
# now saving over the original data set.
dd <- fish_data[!is.na(ins[, 1]), ]

# Make the plot
ggplot() +
  geom_sf(data = st_as_sf(NY), fill = "gray97") +
  geom_point(
    data = data.frame(dd),
    mapping = aes(x = x, y = y, color = "tomato4", ),
    alpha = 0.8, size = 2, show.legend = FALSE
  ) +
  labs(x = "Longitude", y = "Latitude") +
  ggtitle(paste0(unique(dd$species), " distribution")) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = .5),
    text = element_text(size = 10)
  )

# I only make two changes to the code above:<br>
# 1. Change `color = "tomato4` to `color = year` in the `aes()` mapping within `geom_point()`. <br>
# 2. Remove `show.legend = FALSE` to show a legend now that it has meaning.

# Make the plot
ggplot() +
  geom_sf(data = st_as_sf(NY), fill = "gray97") +
  geom_point(
    data = data.frame(fish_data),
    mapping = aes(x = x, y = y, color = year),
    alpha = 0.8, size = 2, show.legend = FALSE
  ) +
  labs(x = "Longitude", y = "Latitude") +
  ggtitle(paste0(unique(fish_data$species), " distribution")) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = .5)
  )

# Make the plot
ggplot() +
  geom_sf(data = st_as_sf(NY), fill = "gray97") +
  geom_point(
    data = data.frame(fish_data),
    mapping = aes(x = x, y = y, color = year),
    alpha = 0.8, size = 2, show.legend = FALSE
  ) +
  scale_colour_gradient(low = "black", high = "gray90") +
  labs(x = "Longitude", y = "Latitude") +
  ggtitle(paste0(unique(fish_data$species), " distribution")) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = .5)
  )

# Make the plot
ggplot() +
  geom_sf(data = st_as_sf(NY), fill = "gray97") +
  geom_jitter(
    data = data.frame(fish_data),
    mapping = aes(x = x, y = y, color = year),
    alpha = 0.8, size = 2,
    height = 1e4, width = 1e4
  ) +
  scale_colour_gradient(low = "black", high = "gray90") +
  labs(x = "Longitude", y = "Latitude") +
  ggtitle(paste0(unique(fish_data$species), " distribution")) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = .5)
  )

# Make new column to indicate whether observations
# occured before or after 1970
fish_data$Period <- "Historic"
fish_data$Period[fish_data$year >= 1970] <- "Recent"

# Make the plot
ggplot() +
  geom_sf(data = st_as_sf(NY), fill = "gray97") +
  geom_jitter(
    data = data.frame(fish_data),
    mapping = aes(x = x, y = y, color = Period),
    alpha = 0.5, size = 2,
    height = 1e4, width = 1e4
  ) +
  labs(x = "Longitude", y = "Latitude") +
  ggtitle(paste0(unique(fish_data$species), " distribution")) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = .5)
  )

# Make the plot
ggplot() +
  geom_sf(data = st_as_sf(NY), fill = "gray97") +
  geom_jitter(
    data = data.frame(fish_data),
    mapping = aes(x = x, y = y, color = Period),
    alpha = 0.5, size = 2,
    height = 1e4, width = 1e4
  ) +
  labs(x = "Longitude", y = "Latitude") +
  ggtitle(paste0(unique(fish_data$species), " distribution")) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = .5)
  ) +
  facet_wrap(~Period)

# Historical (1934-1970) and recent (1970-present) records of Alosa 
# aestivalis in New York, USA (Carlson et al. 2016). Records missing exact 
# dates or locations are not displayed.
# Make the plot
ggplot() +
  geom_sf(data = st_as_sf(NY), fill = "gray97") +
  geom_jitter(
    data = data.frame(fish_data),
    mapping = aes(x = x, y = y, color = Period),
    alpha = 0.5, size = 2,
    height = 1e4, width = 1e4
  ) +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw() +
  theme(
    axis.title.x = element_text(vjust = -3),
    axis.title.y = element_text(vjust = 3),
    panel.spacing = unit(.05, "npc"),
    strip.background = element_blank(),
    text = element_text(size = 14)
  ) +
  facet_wrap(~Period)


