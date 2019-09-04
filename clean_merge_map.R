
# Libraries ---------------------------------------------------------------
if (!require(pacman)) install.packages("pacman")
pacman::p_load(lubridate)
pacman::p_load(magrittr)
pacman::p_load(maps)
pacman::p_load(tidyverse)
pacman::p_load(cowplot)
pacman::p_load(ggsn)
pacman::p_load(ggmap)
pacman::p_load(grid)

conflict_prefer("filter", "dplyr")

# Data loading and cleaning ----------------------------------------------------
# Buprestid catalog data
buprestid_catalog <- read.csv(here::here("buprestidcat.csv"))

sites <- read.csv(here::here("sitetotals_v.csv")) %>% 
   select(site, lat, long)

sites <- sites %>%
   mutate(
      year = case_when(
         site == "AG" |
            site == "BC" |
            site == "BP" |
            site == "FH" |
            site == "GL" |
            site == "MB" |
            site == "NS" | site == "SH" | site == "SP" | site == "TF" ~ 2017,
         TRUE ~ 2018
      )
   )

# Mapping data
# Minnesota county information Use the maps package and get the unique county ID
# (aka FIPS) for each county in Minnesota. This command first gets the fips data
# into a more workable format.
states_county <- do.call("rbind",
                         strsplit(
                            as.character(county.fips$polyname), ","
                         ))
# Take the county.fips data, create new variables for state and county, filter
# by Minnesota, then select only the variable we need
mn_fips <- county.fips %>% 
   mutate(state = states_county[, 1], county = states_county[, 2]) %>% 
   filter(state == "minnesota") %>% 
   select(state, county, fips)



# Filter and clean up data
mn_buprestids <- buprestid_catalog %>% 
   filter(stateprovince == "Minnesota")

# Lower case county names in catalogue for joining consistency and remove
# "county" from any entries
mn_buprestids$county <- tolower(mn_buprestids$county)
mn_buprestids <- mn_buprestids %>%
   # Remove county if it was entered and then remove any remaining whitespace
   mutate(county = str_split(county, "county", simplify = T)[, 1],
          county = trimws(county, which = "right"))



# Check for county names in your data that need to be cleaned to match the fips
# data
# Create a negation of the %in% function
`%!in%` <- Negate(`%in%`)
# If you want to see the list of county names in the FIPS data
sort(unique(mn_fips$county))
# Ask which county names present in the buprestid catalogue are not present in
# the fips data and create an index of their positions so we can see which ones
# are missing
check <- which(
   unique(mn_buprestids$county) %!in% unique(mn_fips$county)
)
# Use the index to check which county names are missing/wrong and use this to
# guide your cleaning
sort(unique(mn_buprestids$county)[check])
# Mistakes include spelling errors and putting cities in place of county
# Issues I don't know if I can resolve or things I'm not 100% on:
# "Mille lac lakes" is a lake that straddles three counties.
# "Lakeland" isn't a county, but it looks like it's a city in Washington co.
# "Santa rita" doesn't appear to be a place in MN. It is a church though?
# Resolve the anoka/isanti (and aonka/isanti) issue If they put Itasca park, do
# they actually mean Itasca park or the county Itasca? Also, Itasca state park
# straddles at least 2 counties.

# Clean county names. This chunk could be removed if you decide to manually
# clean up the excel sheet. The case_when function works by looking at the
# county variable and seeing if county is equal to (==) the string on the right
# side of the ==. If county matches that (for instance, "aonka"), then it will
# replace "aonka" with the string on the right side of the ~ (so "aonka" become
# "anoka"). The last statement that is evaluated (county == county ~ county)
# ensures that anything that is not listed previously stays the same.
mn_buprestids <- mn_buprestids %>% 
   # Fix common spelling mistakes. Add more following this format as necessary
   # but don't delete the last one (county == county ~ county).
   mutate(county = case_when(county == "aonka" ~ "anoka",
                             county == "garrison" ~ "crow wing",
                             county == "hennipen" ~ "hennepin",
                             county == "itsaca" ~ "itasca",
                             county == "lakeland" ~ "washington",
                             county == "latsch state park" ~ "winona",
                             county == "new brighton" ~ "ramsey",
                             county == "olmdsted" ~ "olmsted",
                             county == "ottertail" ~ "otter tail",
                             county == "preston" ~ "fillmore",
                             county == "ramsay" ~ "ramsey",
                             county == "st. louis" ~ "st louis",
                             county == "wyoming" ~ "chisago",
                             county == county ~ county))

# Can rerun the check code from above to see if there are any remaining county
# issues
check <- which(
   unique(mn_buprestids$county) %!in% unique(mn_fips$county)
)
# Use the index to check which county names are missing/wrong and use this to
# guide your cleaning
sort(unique(mn_buprestids$county)[check])

# While we're at it, lets check the species list for any potential typos
View(sort(unique(mn_buprestids$scientificname)))

# Summarise Buprestid data ------------------------------------------------


# Extract the year an insect was identified from the dateidentified variable
mn_buprestids$id_date <- str_sub(mn_buprestids$dateidentified, -4) 
mn_buprestids <- mn_buprestids %>% 
   mutate(collect_year = as.numeric(as.character(year)),
          id_year = as.numeric(as.character(id_date)))

# Calculate most recent collection date (or most recent identification date if
# no collection date is available)
mn_buprestids_summary <- mn_buprestids %>% 
   group_by(scientificname, county) %>% 
   # Give each species/county pair an associated column with the most recent
   # collection date and most recent id date
   summarise(recent_collect_date = max(collect_year),
             recent_id_date = max(id_year)) %>% 
   # Create a recent date column for either the most recent collection
   # (preferred) or id (not preferred)
   mutate(recent_date = ifelse(!is.na(recent_collect_date),
                               recent_collect_date,
                               recent_id_date),
          # Change most recent catch year to the most recent decade
          last_catch_decade = (recent_date - recent_date %% 10),
          last_catch_20 = (recent_date - recent_date %% 20)) 

# add count for each species in each county
mn_buprestids_summary <- mn_buprestids %>% 
   count(scientificname, county) %>% 
   inner_join(mn_buprestids_summary)

mn_buprestids_summary <- mn_buprestids_summary %>% 
   complete(scientificname,
            county,
            fill = list(n = 0)) %>% 
   filter(scientificname != "")


# Set up data for mapping ----------------------------------------------------
# Need to get the lat/long coordinates for each county so ggplot know how to map
# them

# Join FIPs with summary data. This merges the two data frames
# (mn_buprestids_summary and mn_fips) by matching up rows based on similar
# county names. 
mn_buprestids_summary <- full_join(mn_buprestids_summary,
                                 mn_fips,
                                 by = c("county" = "county")) %>% 
   # Complete makes sure each insect has a row for each county in MN
   complete(scientificname,
            county,
            fill = list(n = 0)) %>% 
   filter(scientificname != "") # remove any rows with no scientific name associated

mn_coords <- map_data(map = "county") %>% 
   # Rename variables to be consistent
   mutate(state = region, county = subregion) %>% 
   filter(state == "minnesota") %>% 
   # Select only relevant data
   select(long, lat, group, state, county)

# Get lat long data for each county so we can draw the shapes
mn_buprestids_map <- inner_join(mn_buprestids_summary,
                                mn_coords,
                                by = "county") %>% 
   # remove duplicated rows
   select(-state.x, -state.y) 
  


# Making the maps ---------------------------------------------------------



# There are two ways to make these maps (that I can think of). I'll provide code
# for both and try to explain them. Hopefully one will make sense enough that 
# you can edit the code as needed.
# You'll be using the mn_buprestids_map file for this.

# The jist of this for loop is that it will take your overall data set and
# subset it, making one subset for each species present in the data set. Once it
# has the subset of data, it'll plot the data. We'll put code after the for loop
# for saving all the plots. We'll start with the abundance plots

# Need a list to store each plot in. Lists are a useful data structure for
# storing multiple plots. This is done outside the for loop and it is an empty
# list


# Abundance maps ----------------------------------------------------------


### Make the plots###

abundance_plot_list <- list()
for (i in unique(mn_buprestids_map$scientificname)) {
   # Subset data
   subset_buprestids <- mn_buprestids_map %>% 
      filter(scientificname == i)
   # Remove subsets that for some reason have no counts?
   if (sum(subset_buprestids$n) == 0) {next}
   # plot the data
   abundance_plot_list[[i]] <- ggplot(subset_buprestids,
                            aes(
                               x = long,
                               y = lat,
                               group = group,
                               fill = n
                            )) +
      # Set the color for the county outlines
      geom_polygon(color = "black") +
      # Change your fill type and legend title
      # Set white as the "zero" value then the remaining colors from terrain
      # The values = determines the breaks in the color scale.
      # You might want to play around with using a different color for the zero
      # value or a different set of colors for the rest of the scale.
      scale_fill_gradientn(
         colours = c("white", rev(terrain.colors(5))),
         values = seq(0, 1, by = 0.2),
         # Breaks are set using a custom function, found here: 
         # https://stackoverflow.com/questions/15622001/how-to-display-only-integer-values-on-an-axis-using-ggplot2
         breaks = function(x) floor(pretty(seq(0, (max(x) + 1) * 1.1))),
         name = "Legend Title"
      ) +
      # Change to alter aspect ratio to what you want. Potentially other
      # options besides coord_fixed too if you can't get one you like.
      coord_fixed(1.3) +
      # Theme_void removes all color from outside the minnesota boundary
      theme_void() +
      # Use legend.key.size to set the size of legend. You'll need to play with
      # this so the legend displays at an appropriate size once you save the
      # maps.
      theme(legend.key.size = unit(0.2, "cm")) +
      # Use barheight to change the height of the legend bar so the numbers
      # aren't smushed. This will also require trial and error. Also, I have no
      # idea what unit it is in.
      guides(fill = guide_colorbar(barheight = 4)) 
}

### Check the plots ###
# Take off the $`Acmaeodera pulchella` if you want to check all the plots or
# change the species name to a different one to check a different specific plot
abundance_plot_list$`Acmaeodera pulchella`


### Save the plots ###
# Now, code to save these plots
# Need to get a list of the species names so we can properly save the files
spp_names <- names(abundance_plot_list)
# Need to paste together the species names, a tag so you know its the abundance
# map, and the format you want to save the pictures in (check ?ggsave for other
# format options if you don't want jpeg).
abundance_file_names <- as.list(paste0(spp_names, "_abundance", ".jpeg"))
# Now we have two lists: one with 42 plots and one with 42 names. We can use the
# map function to take these two lists together, and apply the ggsave function
# to them to save a plot with a particular file name
# You can tell ggsave about how you want to save the file by adding more
# arguments after the ggsave argument
# map2(abundance_file_names, abundance_plot_list, # the 2 lists to map over
#      ggsave, # function to apply
#      height = 3.5, # additional arguments to ggsave
#      width = 2.67,
#      units = "in",
#      dpi = 300)



# Last collection maps ----------------------------------------------------

# This will work similarly to the last for loop except this time we're
# interested in plotting the decade (or decade range) of last capture.
# Make last_catch a factor for plotting
mn_buprestids_map$last_catch_decade <- as.factor(mn_buprestids_map$last_catch_decade)
mn_buprestids_map$last_catch_20 <- as.factor(mn_buprestids_map$last_catch_20)
# Lets also make a custom color scale for the different decades
collected_plot_list <- list()
for (i in unique(mn_buprestids_map$scientificname)) {
   # Subset data
   subset_buprestids <- mn_buprestids_map %>% 
      filter(scientificname == i)
   # Remove subsets that for some reason have no counts?
   if (sum(subset_buprestids$n) == 0) {next}
   # plot the data
      collected_plot_list[[i]] <- ggplot(subset_buprestids,
                               aes(
                                  x = long,
                                  y = lat,
                                  group = group,
                                  fill = last_catch_20
                               )) +
         # Set the color for the county outlines
         geom_polygon(color = "black") +
         # Change your fill type and legend title
         # Set white as the "zero" value then the remaining colors from terrain
         # The values = determines the breaks in the color scale
         scale_fill_hue(l = 60, # adjust luminance
                        c = 100, # adjust chroma
                        na.value = "white", 
                        drop = F, # Keep unused factor labels
                        labels = c("1900 - 1919",
                                   "1920 - 1939",
                                   "1940 - 1959",
                                   "1960 - 1979",
                                   "1980 - 1999",
                                   "2000 - 2018",
                                   "No record"),
                        name = "Legend title") +
         # Change to alter aspect ratio to what you want. Potentially other
         # options besides coord_fixed too if you can't get one you like.
         coord_fixed(1.3) +
         # Theme_void removes all color from outside the minnesota boundary
         theme_void() +
         # Use legend.key.size to set the size of legend. You'll need to play
         # with this so the legend displays at an appropriate size once you save
         # the maps.
         theme(legend.key.size = unit(0.2, "cm")) +
         scale_x_continuous(limits = c(min(subset_buprestids$long),
                                       max(subset_buprestids$long))) +
         scale_y_continuous(limits = c(min(subset_buprestids$lat),
                                       max(subset_buprestids$lat)))
}



### Check the plots ###
# Take off the $`Acmaeodera pulchella` if you want to check all the plots or
# change the species name to a different one to check a different specific plot
collected_plot_list$`Acmaeodera pulchella`

### Save the plots ###
# Now, code to save these plots
# This will use the same spp_names list from above

collected_file_names <- as.list(paste0(spp_names, "_collected", ".jpeg"))
# Now we have two lists: one with 42 plots and one with 42 names. We can use the
# map function to take these two lists together, and apply the ggsave function
# to them to save a plot with a particular file name
# map2(collected_file_names, collected_plot_list,
#      ggsave,
#      height = 3.5, # additional arguments to ggsave
#      width = 2.67,
#      units = "in",
#      dpi = 300)

combined_maps <- map2(abundance_plot_list, collected_plot_list, plot_grid, ncol = 2)




# Site map ----------------------------------------------------------------
sites$year <- as.factor(sites$year)
sites$Year <- sites$year

site_map <-
   ggplot(mn_coords, aes(x = long, y = lat, group = group)) +
   geom_polygon(fill = "white", colour = "black") +
   coord_fixed(1.3) +
   # Theme_void removes all color from outside the minnesota boundary
   theme_void() +
   geom_jitter(data = sites,
              aes(
                 x = long,
                 y = lat,
                 group = NULL,
                 colour = Year
              ),
              size = 0.7,
              alpha = 0.65,
              width = 0.01) +
   scalebar(
      data = mn_coords,
      dist = 50,
      st.size = 2,
      height = 0.02,
      model = 'WGS84',
      transform = T,
      dist_unit = "km"
   ) +
   north(mn_coords, symbol = 3, scale = 0.15) +
   scale_fill_manual(values = c("red", "yellow"),
                     aes(x = long, y = lat))

   

ggsave("sitemap.tiff",
       plot = site_map,
       units = "mm",
       width = 84,
       dpi = 600)


# Wasp radius map

# Set API
key <- read.table(here::here("ggmaps_id.txt"),
                  stringsAsFactors = FALSE)
key <- key[1, 1]
register_google(key = key)


# Make map

# create circles data frame from the centers data frame
make_circles <- function(centers, radius, nPoints = 100) {
   # centers: the data frame of centers with name
   # radius: radius measured in kilometer - have to  convert
   radius <- radius / 1000
   meanLat <- mean(centers$lat)
   # length per longitude changes with lattitude, so need correction
   radiusLon <- radius / 111 / cos(meanLat / 57.3)
   radiusLat <- radius / 111
   circleDF <- data.frame(name = rep(centers$name, each = nPoints))
   angle <- seq(0, 2 * pi, length.out = nPoints)
   
   circleDF$lon <-
      unlist(map2(centers$lon, radiusLon, function(.x, .y) {
         .x + .y * cos(angle)
      }))
   circleDF$lat <-
      unlist(map2(centers$lat, radiusLat, function(.x, .y) {
         .x + .y * sin(angle)
      }))
   #circleDF$lon <- unlist(lapply(centers$lon, function(x) x + radiusLon * cos(angle)))
   #circleDF$lat <- unlist(lapply(centers$lat, function(x) x + radiusLat * sin(angle)))
   return(circleDF)
}


AG <- sites %>% 
   filter(site == "AG")

centers <- AG %>% 
   select(lat, lon = long)
radius <- 0.2
nPoints <- 100
radius <- radius / 1000
meanLat <- mean(centers$lat)
# length per longitude changes with lattitude, so need correction
radiusLon <- radius / 111 / cos(meanLat / 57.3)
radiusLat <- radius / 111
angle <- seq(0, 2 * pi, length.out = nPoints)

circleDF_lon <- centers$lon + (radiusLon * cos(angle))
   
circleDF_lat <- centers$lat + (radiusLat * sin(angle))
circleDF <- data.frame(
   lon = circleDF_lon,
   lat = circleDF_lat
) 

site_map <- get_map(location = c(lon = -93.158732, lat =  45.031724),
                    maptype = "terrain",
                    source = "stamen",
                    zoom = 17)

# Get background image with tree canopies
canopy_bkgrd <- png::readPNG(here::here("ag_site.png"))

wasp_radius_map <- ggmap(site_map, extent = "normal") +
   # annotation_custom(rasterGrob(canopy_bkgrd,
   #                              width = unit(1, "npc"),
   #                              height = unit(1, "npc")),
   #                   -Inf, Inf, -Inf, Inf) +
   background_image(canopy_bkgrd) +
   # Center point
   geom_point(data = AG, aes(x = -93.15882, y = 45.031710),
              size = 1.2,
              colour = "red") +
   theme_void()+
   geom_point(data = AG, aes(x = -93.15870, y = 45.031720),
              size = 82,
              shape = 21,
              colour = "red",
              stroke = 1.5)  +
   coord_cartesian()
   
wasp_radius_map
ggsave(wasp_radius_map,
       filename = "wasp_radius_map.tiff",
       units = "mm",
       dpi = 600,
       width = 84,
       height = 84)
