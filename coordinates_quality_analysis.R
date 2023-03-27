# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# set up
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

options(scipen=999)

library(dplyr)
library(tidyverse)
library(sf)
library(tigris)
library(tidygeocoder)
library(tmap)

### read in data files
facilities <- read.csv("data/files/facilities.csv")
submissions <- read.csv("data/files/submissions.csv")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# investigate facilities spreadsheet
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### identify facility rows without complete lat/long data
facilities_missing <- facilities %>%
  filter(is.na(Lat)==T | is.na(Lng)==T)

### quantify missingness in facility spreadsheet
length(unique(facilities_missing$EPAFacilityID))
length(unique(facilities_missing$EPAFacilityID)) / length(unique(facilities$EPAFacilityID)) * 100
#5092 facilities missing coordinates (24.2% of all facilities in database)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# investigate submissions spreadsheet
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### create fields (missing_Fac and missing_FRS) to identify rows with missing lat/long data 
submissions <- submissions %>%
  mutate(missing_Fac = case_when((is.na(FacLat)==T | is.na(FacLng)==T) ~ 1,
                                 .default = 0),
         missing_FRS = case_when((is.na(FRSLat)==T | is.na(FRSLng)==T) ~ 1,
                                 .default = 0))

### quantify missingness for FRS lat/long vs. Fac lat/long
length(submissions$EPAFacilityID[submissions$missing_Fac==1])
#0 submissions missing facility coordinate data

length(submissions$EPAFacilityID[submissions$missing_FRS==1])
length(submissions$EPAFacilityID[submissions$missing_FRS==1]) / length(submissions$EPAFacilityID) * 100
length(unique(submissions$EPAFacilityID[submissions$missing_FRS==1]))
#5092 unique facilities (13,358 submission rows) missing FRS coordinate data (14.8% of all submissions in database)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# identify data quality concerns in FacLat/FacLng
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

###convert submissions to sf dataframe
submissions <- st_as_sf(submissions, coords = c("FacLng", "FacLat"), crs=4269)
us <- nation(resolution = "5m")

###find those submission facility coordinates outside of US
submissions <- st_join(submissions, us, join = st_within) %>%
  mutate(InUS = case_when(GEOID=="US" ~ 1,
                          .default = 0)) %>%
  select(-c(AFFGEOID, NAME, GEOID))

###quantify number of submissions with coordinates outside of US
length(submissions$EPAFacilityID[submissions$InUS==0])
length(submissions$EPAFacilityID[submissions$InUS==0]) / length(submissions$EPAFacilityID) * 100
length(unique(submissions$EPAFacilityID[submissions$InUS==0]))
#1837 unique facilities (2628 submission rows) with facility coordinates (FacLat/FacLng) outside the US (2.9% of all submissions in database)

###create table of submissions rows with invalid coordinates, sorted by newest dates
invalid_coordinates <- submissions %>%
  filter(InUS == 0) %>%
  arrange(desc(ReceiptDate))

###quatify number of submissions with invalid coordinates that were received since 2010
length(invalid_coordinates$EPAFacilityID[invalid_coordinates$ReceiptDate >= "2010-01-01"])
length(invalid_coordinates$EPAFacilityID[invalid_coordinates$ReceiptDate >= "2010-01-01"]) / length(invalid_coordinates$EPAFacilityID) * 100
#107 submissions with invalid coordinates were submitted since 2010 (4.1% of submissions with invalid coordinates)

### interactive map of submission coordinates
tmap_mode("view")
m1 <- tm_basemap("CartoDB.PositronNoLabels") + 
  tm_scale_bar() + 
  tm_shape(us, unit="imperial") +
  tm_borders(col="black") +
  tm_shape(submissions) +
  tm_dots(col="blue",
          size = 0.025,
          title="Facility Locations") +
  tm_tiles("CartoDB.PositronOnlyLabels") 
m1

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# facilities with multiple different submission coordinates
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### facilities (identified by EPAFacilityID) with multiple FacLat/FacLng coordinates
multiples <- submissions %>%
  group_by(EPAFacilityID) %>%
  mutate(total_submissions = n()) %>%
  distinct(geometry, .keep_all = TRUE) %>%
  mutate(coordinate_pairs = n())

### quantify number of facilities with multiple FacLat/FacLng coordinates
length(unique(multiples$EPAFacilityID[multiples$total_submissions>1 & multiples$coordinate_pairs>1]))
length(unique(multiples$EPAFacilityID[multiples$total_submissions>1 & multiples$coordinate_pairs>1])) / length(unique(multiples$EPAFacilityID[multiples$total_submissions>1])) * 100
#11353 unique facilities with more than one coordinate pairs provided across several submissions (66% of facilities with more than one submission)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# geocoding addresses from facilities spreadsheet
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# need to add in a loop to implement this in batches
facilities_missing <- facilities_missing %>%
  mutate(addr = paste(Addr1, Addr2, sep=", ")) %>%
  geocode(street = "addr",
          city = "City",
          state = "State",
          postalcode = "ZipCode",
          method="osm")