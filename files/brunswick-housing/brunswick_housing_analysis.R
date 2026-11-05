# =================================================================================================
# Some statistical notes on housing in Brunswick, Maine
# Every figure and table in the slides (October 2026), in one script
#
# Jack O'Brien, Bowdoin College (jobrien@bowdoin.edu)
#
# HOW TO RUN
#   Put this script in a folder with the input files below and run it from that folder:
#     Rscript brunswick_housing_analysis.R
#   It needs a Census API key for tidycensus (tidycensus::census_api_key("...", install = TRUE)) and
#   an internet connection for the Census API, TIGER/Line shapefiles, the 2006-2010 ACS summary file,
#   and geocoding. Packages: dplyr, tidyr, purrr, ggplot2, ggrepel, patchwork, scales, sf, tigris,
#   tidycensus, tidygeocoder.
#
# INPUTS
#   Downloaded by the script:
#     - American Community Survey 5-year estimates, Brunswick town and its block groups, 2005-2009 to
#       2020-2024: B25003 and B25004 (rental vacancy), B25056 (contract rent), B25074 (renter income
#       vs. rent), B25106 (housing costs as a share of income, by tenure)
#     - Census TIGER/Line 2023 (town boundary, roads, water); Census Geocoder and OpenStreetMap
#   Files read from disk:
#     - RentalListings2025-04-18.csv          Town of Brunswick short-term rental listings report,
#                                             April 18, 2025
#     - brunswick_zillow_response.csv         daily scrape of Zillow rental listings for Brunswick,
#                                             June 2025 - April 2026
#     - data/brunswick_housing.gpkg           unit-level housing database built from the Town of
#       data/brunswick_housing_units.csv      Brunswick's assessing data (parcels and property cards)
#       data/brunswick_rental_units.csv
#     - data/zillow_geocodes.csv              geocoding cache for the Zillow addresses (optional;
#                                             rebuilt if missing)
#
# OUTPUTS (slide numbers refer to the October 2026 slides)
#   brunswick_str_map_notext.pdf          slide 1   town map of STR listings
#   brunswick_cost_burden.pdf             slide 2   page 1: cost burden by income, owners vs. renters
#                                         slide 3   page 2: cost burden over time
#   brunswick_rent_densities.pdf          slide 4   page 1: offered vs. current rents, whole town
#                                         slide 5   page 2: the same by neighborhood
#   brunswick_year_rent_densities.pdf     slide 7   page 1: distribution of rent over time
#                                         slide 6   page 3: rental vacancy rate over time
#   fig_rent_2012_2017.pdf,
#   fig_vacancy_2012_2017.pdf             slide 9   rents and vacancy, estimates starting 2012-2017
#   brunswick_str_map.pdf                 slides 11-12  STR maps, whole town and downtown
#   Tables, printed to the console:
#     slide 2   cost-burdened households by tenure, with odds
#     slide 10  STR listings by building type and area, rental units in the whole town,
#               and the STR share of rental housing (in distinct units)
#   Also written: brunswick_rent_distributions.csv, data/str_listings_by_area.csv
#
# The sections below run in order; later sections use objects made by earlier ones.
# =================================================================================================

# ==================================================================================================
# 1. Neighborhood definitions
#    (from brunswick_neighborhoods.R)
# ==================================================================================================
# Neighborhood definitions for Brunswick, ME, shared by the Zillow and rent-distribution scripts
#   Downtown:       the in-town grid around Maine St and Bowdoin, split into four quadrants
#                   at the Maine St / McKeen St / Bath Rd crossroads
#   East Brunswick: Brunswick Landing (former NAS) and Cook's Corner, incl. Gurnet and Old Bath Rds
#   Mere Point:     the Mere Point peninsula
#   The Farms:      rural west and north Brunswick (Old Portland, River, Lunt Rds, etc.)
library(sf)
library(dplyr)
library(tigris)

options(tigris_use_cache = TRUE)

# --- Street lines that bound the downtown quadrants (Census TIGER/Line) ----------
downtown_roads <- roads("ME", county = "Cumberland", year = 2023) |>
  filter(FULLNAME %in% c("Maine St", "McKeen St", "Bath Rd", "Baribeau Dr")) |>
  st_transform(4326) |>
  st_crop(c(xmin = -70.00, xmax = -69.90, ymin = 43.88, ymax = 43.93))

road_coords <- function(name) {
  downtown_roads |> filter(FULLNAME == name) |> st_coordinates() |> as.data.frame()
}

# Interpolate a street's position: longitude of a N-S street at a given latitude,
# or latitude of an E-W street at a given longitude (held constant past its ends)
street_lon_at <- function(name, lat) {
  xy <- road_coords(name) |> arrange(Y)
  approx(xy$Y, xy$X, xout = lat, rule = 2, ties = mean)$y
}
street_lat_at <- function(name, lon) {
  xy <- road_coords(name) |> arrange(X)
  approx(xy$X, xy$Y, xout = lon, rule = 2, ties = mean)$y
}

downtown_regions <- c(
  "Downtown NW" = "Downtown NW: north of McKeen, west of Maine",
  "Downtown SW" = "Downtown SW: south of McKeen, west of Maine, east of Baribeau",
  "Downtown NE" = "Downtown NE: north of Bath Rd, east of Maine",
  "Downtown SE" = "Downtown SE: south of Bath Rd, east of Maine"
)

neighborhood_levels <- c("Downtown", "East Brunswick", "Mere Point", "The Farms", "Other")

# Returns a data frame with `neighborhood` and, for downtown points, `downtown_region`
assign_neighborhood <- function(lat, lon) {
  west_of_maine   <- lon < street_lon_at("Maine St", lat)
  south_of_mckeen <- lat < street_lat_at("McKeen St", lon)
  north_of_bath   <- lat > street_lat_at("Bath Rd", lon)
  east_of_baribeau <- lon > street_lon_at("Baribeau Dr", lat)

  in_downtown_box <- between(lat, 43.895, 43.922) & between(lon, -69.990, -69.950)
  region <- case_when(
    !in_downtown_box | is.na(lat)                         ~ NA_character_,
    west_of_maine & !south_of_mckeen & lon > -69.980      ~ "Downtown NW",
    west_of_maine & south_of_mckeen & east_of_baribeau    ~ "Downtown SW",
    !west_of_maine & north_of_bath                        ~ "Downtown NE",
    !west_of_maine & !north_of_bath                       ~ "Downtown SE",
    TRUE                                                  ~ NA_character_
  )

  neighborhood <- case_when(
    is.na(lat)                         ~ NA_character_,
    !is.na(region)                     ~ "Downtown",
    lat < 43.86 & lon < -69.99         ~ "Mere Point",
    lat > 43.855 & lon > -69.935       ~ "East Brunswick",
    lat > 43.86 & lon < -70.00         ~ "The Farms",
    TRUE                               ~ "Other"
  )

  data.frame(neighborhood    = factor(neighborhood, levels = neighborhood_levels),
             downtown_region = factor(region, levels = names(downtown_regions)))
}

# ==================================================================================================
# 2. Zillow rental listings (offered rents)
#    (from brunswick_zillow_data.R)
# ==================================================================================================
# Zillow rental listings for Brunswick, ME, with geocoded addresses and neighborhoods
# Input: brunswick_zillow_response.csv (daily scrape of Zillow rental listings)
# Creates `listings` (one row per listing per scrape date) and `cached` (geocodes)
library(dplyr)
library(tidygeocoder)

# --- Listings -------------------------------------------------------------------
listings <- read.csv("brunswick_zillow_response.csv", check.names = FALSE) |>
  transmute(date    = as.Date(Date, "%m/%d/%Y"),
            listing = `Unit Address`,
            rent    = as.numeric(gsub("[$,]", "", `Starting Price / Month (Median of Ranges)`))) |>
  filter(listing != "", !is.na(rent))

# Street address from a listing title such as
# "Haven West 121 Pegasus St, Brunswick, ME (2 bd)" or "MSMT Clancy House, 9 Cedar St, ..."
street_address <- function(x) {
  x <- sub("(\\s*\\([^)]*\\))+\\s*$", "", x)               # "(1bd)", "(2 bd) (4 units available)"
  parts <- strsplit(x, ",\\s*")
  vapply(parts, function(p) {
    p <- p[grepl("(^|\\s)\\d+\\S*\\s+[A-Za-z]", p)][1]    # first piece with a house number and street
    if (is.na(p)) return(NA_character_)
    p <- sub("^.*?(?:^|\\s)(?=\\d)", "", p, perl = TRUE)   # drop a building name before it
    p <- sub("\\s+(#|APT\\b|UNIT\\b|FLOOR\\b|- ).*$", "", p, ignore.case = TRUE)
    trimws(sub("\\s+\\d+\\s+[A-Za-z].*$", "", p))          # "25 River Road 25 River Rd" -> first copy
  }, character(1))
}

listings <- listings |> mutate(street = street_address(listing))

# --- Geocode (cached) -------------------------------------------------------------
geo_file <- "data/zillow_geocodes.csv"
cached <- if (file.exists(geo_file)) read.csv(geo_file) else
  data.frame(street = character(), lat = numeric(), lon = numeric())

to_geocode <- setdiff(na.omit(unique(listings$street)), cached$street)
if (length(to_geocode) > 0) {
  new <- tibble(street = to_geocode, q = paste0(street, ", Brunswick, ME 04011")) |>
    geocode_combine(queries = list(list(method = "census"), list(method = "arcgis")),
                    global_params = list(address = "q"), lat = lat, long = lon) |>
    select(street, lat, lon)
  cached <- bind_rows(cached, new)
  write.csv(cached, geo_file, row.names = FALSE)
}

listings <- left_join(listings, cached, by = "street")
listings <- bind_cols(listings, assign_neighborhood(listings$lat, listings$lon))

# ==================================================================================================
# 3. Current rents by neighborhood, ACS 2020-2024
#    (from brunswick_rent_data.R)
# ==================================================================================================
# Rent data by Brunswick neighborhood: offered rents (Zillow) and current rents (ACS)
# Inputs: brunswick_zillow_response.csv (via brunswick_zillow_data.R),
#         data/brunswick_housing.gpkg and data/brunswick_housing_units.csv (from brunswick_housing_db.R),
#         ACS 5-year contract rent (B25056) by block group
# Creates the binned distributions (`dist`), `medians`, and the inputs behind them;
# writes brunswick_rent_distributions.csv
library(dplyr)
library(tidyr)
library(sf)
library(tidycensus)

acs_year <- 2024  # 2020-2024 ACS 5-year
z95 <- qnorm(0.975)

# Areas compared: the four downtown quadrants, downtown as a whole, and the other three neighborhoods
# Downtown SW and SE are pooled, as are Mere Point and the Farms, since each alone has few renters
area_levels <- c("Downtown NW", "Downtown NE", "Downtown South", "Downtown (all)",
                 "East Brunswick", "Mere Point & the Farms", "All of Brunswick")
combine_areas <- function(x) {
  case_when(x %in% c("Downtown SW", "Downtown SE") ~ "Downtown South",
            x %in% c("Mere Point", "The Farms")     ~ "Mere Point & the Farms",
            TRUE                                    ~ x)
}

# Each unit (listing or parcel) belongs to its downtown quadrant, to its neighborhood, and to the town
expand_areas <- function(df) {
  bind_rows(
    df |> filter(!is.na(downtown_region)) |> mutate(area = as.character(downtown_region)),
    df |> filter(!is.na(neighborhood), neighborhood != "Other") |>
      mutate(area = if_else(neighborhood == "Downtown", "Downtown (all)", as.character(neighborhood))),
    df |> filter(!is.na(neighborhood)) |> mutate(area = "All of Brunswick")
  ) |>
    mutate(area = factor(combine_areas(area), levels = area_levels))
}

# Rent bins: ACS B25056 categories, collapsed where Brunswick has few units
bins <- tibble(variable = sprintf("B25056_%03d", 3:26)) |>
  mutate(lower = c(0, seq(100, 750, 50), 800, 900, 1000, 1250, 1500, 2000, 2500, 3000, 3500),
         upper = lead(lower, default = Inf),
         bin = cut(lower, c(0, 1000, 1250, 1500, 2000, 2500, 3000, Inf), right = FALSE,
                   labels = c("< $1,000", "$1,000–1,249", "$1,250–1,499", "$1,500–1,999",
                              "$2,000–2,499", "$2,500–2,999", "$3,000+")))
bin_levels <- levels(bins$bin)
bin_breaks <- c(0, 1000, 1250, 1500, 2000, 2500, 3000, Inf)

# --- Offered rents: Zillow, one observation per distinct listing ----------------------
# A listing's title changes with its unit count ("(12 units available)"), so drop that;
# a listing's offered rent is the median of its asking rents over the days it was active
offered_listings <- listings |>
  filter(!is.na(neighborhood)) |>
  mutate(listing_id = gsub("\\s+", " ", sub("\\s*\\(\\d+ units available\\)", "", listing))) |>
  group_by(listing_id, neighborhood, downtown_region) |>
  summarise(rent = median(rent), .groups = "drop") |>
  expand_areas() |>
  mutate(bin = cut(rent, bin_breaks, right = FALSE, labels = bin_levels))

# Wilson score interval for a binomial proportion
wilson <- function(x, n, z = z95) {
  p <- x / n
  centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half   <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  list(lo = pmax(0, centre - half), hi = pmin(1, centre + half))
}

offered <- offered_listings |>
  count(area, bin, .drop = FALSE) |>
  group_by(area) |>
  mutate(total = sum(n), share = n / total,
         lo = wilson(n, total)$lo, hi = wilson(n, total)$hi) |>
  ungroup() |>
  mutate(source = "Offered rents (Zillow listings, 2025–26)")

# --- Current rents: ACS contract rent by block group ----------------------------------
acs <- get_acs(geography = "block group", state = "ME", county = "Cumberland",
               table = "B25056", year = acs_year, survey = "acs5", geometry = TRUE) |>
  st_transform(4326)

bg_geom <- acs |> distinct(GEOID, geometry)

# Allocate each block group's renters to areas in proportion to the parcels' possibly-rental
# units (assessor rental units plus units without an owner-occupant, excluding STRs)
rental_units <- read.csv("data/brunswick_housing_units.csv", colClasses = c(account = "character")) |>
  filter(unit_tenure %in% c("Rental", "Not owner-occupied (rental, second home, or vacant)")) |>
  count(account, name = "units")

parcels <- st_read("data/brunswick_housing.gpkg", quiet = TRUE) |>
  distinct(account, .keep_all = TRUE) |>
  inner_join(rental_units, by = "account") |>
  st_point_on_surface() |>
  st_transform(4326)

xy <- st_coordinates(parcels)
parcels <- parcels |>
  mutate(lon = xy[, "X"], lat = xy[, "Y"]) |>
  bind_cols(assign_neighborhood(xy[, "Y"], xy[, "X"])) |>
  st_join(bg_geom, join = st_within) |>
  st_drop_geometry() |>
  filter(!is.na(GEOID))

bg_shares <- parcels |>
  expand_areas() |>
  group_by(GEOID, area) |>
  summarise(units = sum(units), .groups = "drop") |>
  left_join(parcels |> group_by(GEOID) |> summarise(bg_units = sum(units)), by = "GEOID") |>
  mutate(share = units / bg_units)

acs_tbl <- st_drop_geometry(acs) |>
  filter(GEOID %in% bg_shares$GEOID) |>
  mutate(se = moe / qnorm(0.95))     # ACS margins of error are 90% intervals

# Allocated counts by detailed ACS category, with standard errors
# (allocation shares are treated as fixed; SE of a weighted sum = sqrt(sum(w^2 se^2)))
allocated <- acs_tbl |>
  inner_join(bg_shares |> select(GEOID, area, share), by = "GEOID", relationship = "many-to-many") |>
  group_by(area, variable) |>
  summarise(est = sum(share * estimate), se = sqrt(sum(share^2 * se^2)), .groups = "drop")

cash_total <- allocated |> filter(variable == "B25056_002") |> select(area, total = est, total_se = se)

current <- allocated |>
  inner_join(bins, by = "variable") |>
  group_by(area, bin) |>
  summarise(n = sum(est), se = sqrt(sum(se^2)), .groups = "drop") |>
  complete(area, bin, fill = list(n = 0, se = 0)) |>
  left_join(cash_total, by = "area") |>
  mutate(share = n / total,
         # Census Bureau formula for the SE of a proportion (ratio formula when the radicand is negative)
         radicand = se^2 - share^2 * total_se^2,
         share_se = sqrt(if_else(radicand >= 0, radicand, se^2 + share^2 * total_se^2)) / total,
         lo = pmax(0, share - z95 * share_se), hi = pmin(1, share + z95 * share_se),
         source = sprintf("Current rents (ACS %d–%d contract rent)", acs_year - 4, acs_year))

# --- Medians ----------------------------------------------------------------------
# ACS median by linear interpolation within the detailed categories
binned_median <- function(lower, upper, n) {
  cum <- cumsum(n); half <- sum(n) / 2
  i <- which(cum >= half)[1]
  if (is.na(i) || sum(n) == 0) return(NA_real_)
  if (is.infinite(upper[i])) return(lower[i])
  lower[i] + (half - (cum[i] - n[i])) / n[i] * (upper[i] - lower[i])
}

medians <- allocated |>
  inner_join(bins, by = "variable") |>
  arrange(area, lower) |>
  group_by(area) |>
  summarise(current_median = binned_median(lower, upper, est)) |>
  left_join(cash_total |> select(area, renter_households = total), by = "area") |>
  left_join(offered_listings |> group_by(area) |>
              summarise(offered_median = median(rent), listings = n()), by = "area") |>
  mutate(area = factor(area, levels = area_levels)) |>
  arrange(area)

# Affordable rent: 30% of the median renter household's monthly income. Renter income by block group
# comes from B25074 (household income by gross rent as a share of income, renters only), allocated
# like rents; the median is interpolated within its seven income ranges
income_bins <- tibble(variable = sprintf("B25074_%03d", c(2, 11, 20, 29, 38, 47, 56)),
                      lower = c(0, 10000, 20000, 35000, 50000, 75000, 100000),
                      upper = lead(lower, default = Inf))
renter_income <- get_acs(geography = "block group", state = "ME", county = "Cumberland",
                         variables = income_bins$variable, year = acs_year, survey = "acs5") |>
  inner_join(bg_shares |> select(GEOID, area, share), by = "GEOID", relationship = "many-to-many") |>
  group_by(area, variable) |>
  summarise(est = sum(share * estimate), .groups = "drop") |>
  inner_join(income_bins, by = "variable") |>
  arrange(area, lower) |>
  group_by(area) |>
  summarise(renter_income = binned_median(lower, upper, est))
medians <- medians |>
  left_join(renter_income |> mutate(area = factor(area, levels = area_levels)), by = "area") |>
  mutate(affordable_rent = 0.3 * renter_income / 12)
print(medians)

dist <- bind_rows(current, offered) |>
  mutate(area = factor(area, levels = area_levels),
         bin = factor(bin, levels = bin_levels))
write.csv(dist |> select(area, source, bin, n, share, lo, hi), "brunswick_rent_distributions.csv",
          row.names = FALSE)

# ==================================================================================================
# 4. Earlier rents and the town-wide rental vacancy rate, every ACS release
#    (from brunswick_rent_history.R)
# ==================================================================================================
# Earlier current rents: ACS 2011-2015 and 2006-2010 5-year contract rent (B25056) by block group
# Run after brunswick_rent_data.R (uses `parcels`, `expand_areas`, `binned_median`, `bins`)
# Creates `hist_bins`, `hist_tbl`, `hist_shares`, `hist_medians`, and the town-wide series `town_tbl` and `town_vacancy`
# Both vintages use 2010 block groups, so renters are allocated through those rather than the 2020 ones.
# The Census API has no block groups for 2006-2010, so that vintage comes from the ACS summary file.
library(dplyr)
library(tidyr)
library(sf)
library(tidycensus)

hist_years <- c(2010, 2015)

# 2011-2015 uses the same categories as 2020-2024; 2006-2010 tops out at "$2,000 or more"
bins_2010 <- tibble(variable = sprintf("B25056_%03d", 3:23)) |>
  mutate(lower = c(0, seq(100, 750, 50), 800, 900, 1000, 1250, 1500, 2000),
         upper = lead(lower, default = Inf))
hist_bins <- bind_rows(bins |> select(variable, lower, upper) |> mutate(year = 2015),
                       bins_2010 |> mutate(year = 2010))

# --- 2011-2015: Census API -------------------------------------------------------------
acs_2015 <- get_acs(geography = "block group", state = "ME", county = "Cumberland",
                    table = "B25056", year = 2015, survey = "acs5", geometry = TRUE) |>
  st_transform(4326)
bg_geom_2010 <- acs_2015 |> distinct(GEOID, geometry)

# --- 2006-2010: summary file, sequence 97 (B25056 starts at column 213) -----------------
sf_url <- "https://www2.census.gov/acs2010_5yr/summaryfile/2006-2010_ACSSF_By_State_By_Sequence_Table_Subset/Maine/Tracts_Block_Groups_Only/"
sf_dir <- "data/acs2010_5yr"
if (!dir.exists(sf_dir)) {
  dir.create(sf_dir, recursive = TRUE)
  download.file(paste0(sf_url, "20105me0097000.zip"), file.path(sf_dir, "seq97.zip"), quiet = TRUE)
  unzip(file.path(sf_dir, "seq97.zip"), exdir = sf_dir)
  download.file(paste0(sf_url, "g20105me.csv"), file.path(sf_dir, "g20105me.csv"), quiet = TRUE)
}
sf_geo <- read.csv(file.path(sf_dir, "g20105me.csv"), header = FALSE, colClasses = "character") |>
  filter(V3 == "150") |>                                           # block groups
  transmute(logrecno = as.integer(V5), GEOID = sub("^15000US", "", V49))
read_seq <- function(prefix, value) {
  read.csv(file.path(sf_dir, paste0(prefix, "20105me0097000.txt")), header = FALSE) |>
    select(logrecno = V6, V213:V236) |>
    pivot_longer(-logrecno, names_to = "col", values_to = value) |>
    mutate(variable = sprintf("B25056_%03d", as.integer(sub("V", "", col)) - 212)) |>
    select(-col)
}
acs_2010 <- read_seq("e", "estimate") |>
  inner_join(read_seq("m", "moe"), by = c("logrecno", "variable")) |>
  inner_join(sf_geo, by = "logrecno") |>
  mutate(moe = if_else(moe < 0, NA_real_, as.numeric(moe)))   # -1 etc. flag unavailable MOEs

# --- Allocate block-group renters to areas, as for 2020-2024 ----------------------------
hist_shares <- parcels |>
  select(-GEOID) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) |>
  st_join(bg_geom_2010, join = st_within) |>
  st_drop_geometry() |>
  filter(!is.na(GEOID)) |>
  (\(p) expand_areas(p) |>
     group_by(GEOID, area) |>
     summarise(units = sum(units), .groups = "drop") |>
     left_join(p |> group_by(GEOID) |> summarise(bg_units = sum(units)), by = "GEOID"))() |>
  mutate(share = units / bg_units)

hist_tbl <- bind_rows(
  st_drop_geometry(acs_2015) |> select(GEOID, variable, estimate, moe) |> mutate(year = 2015),
  acs_2010 |> select(GEOID, variable, estimate, moe) |> mutate(year = 2010)
) |>
  filter(GEOID %in% hist_shares$GEOID) |>
  mutate(se = coalesce(moe, 0) / qnorm(0.95))

hist_allocated <- hist_tbl |>
  inner_join(hist_shares |> select(GEOID, area, share), by = "GEOID", relationship = "many-to-many") |>
  group_by(year, area, variable) |>
  summarise(est = sum(share * estimate), .groups = "drop")

hist_medians <- hist_allocated |>
  inner_join(hist_bins, by = c("year", "variable")) |>
  arrange(year, area, lower) |>
  group_by(year, area) |>
  summarise(median = binned_median(lower, upper, est), .groups = "drop") |>
  left_join(hist_allocated |> filter(variable == "B25056_002") |> select(year, area, renter_households = est),
            by = c("year", "area"))
print(hist_medians, n = Inf)

# --- Town-wide series: every ACS 5-year release before the current one ---------------------
# Brunswick town is a county subdivision, so no allocation is needed (its 2020-2024 total matches
# the sum of the allocated block groups). Releases before 2011-2015 top out at "$2,000 or more".
town_years <- 2009:(acs_year - 1)
town_tbl <- purrr::map_dfr(town_years, function(y) {
  get_acs(geography = "county subdivision", state = "ME", county = "Cumberland",
          table = "B25056", year = y, survey = "acs5") |>
    filter(NAME == "Brunswick town, Cumberland County, Maine") |>
    mutate(year = y)
}) |>
  inner_join(bind_rows(lapply(town_years, function(y) {
    (if (y < 2015) bins_2010 else bins |> select(variable, lower, upper)) |> mutate(year = y)
  })), by = c("year", "variable"))

# --- Town-wide rental vacancy rate, every ACS 5-year release --------------------------------
# Rate = vacant for rent / (renter-occupied + vacant for rent + rented, not occupied), as in DP04_0005
vacancy_vars <- c(renter_occ = "B25003_003", for_rent = "B25004_002", rented_not_occ = "B25004_003")
town_vacancy <- purrr::map_dfr(2009:acs_year, function(y) {
  get_acs(geography = "county subdivision", state = "ME", county = "Cumberland",
          variables = vacancy_vars, year = y, survey = "acs5") |>
    filter(NAME == "Brunswick town, Cumberland County, Maine") |>
    mutate(year = y)
}) |>
  mutate(se = moe / qnorm(0.95)) |>
  group_by(year) |>
  summarise(for_rent = estimate[variable == "for_rent"], for_rent_se = se[variable == "for_rent"],
            stock = sum(estimate), stock_se = sqrt(sum(se^2))) |>
  # Census Bureau formula for the SE of a proportion (ratio formula when the radicand is negative)
  mutate(rate = for_rent / stock,
         radicand = for_rent_se^2 - rate^2 * stock_se^2,
         rate_se = sqrt(if_else(radicand >= 0, radicand, for_rent_se^2 + rate^2 * stock_se^2)) / stock,
         lo = pmax(0, rate - qnorm(0.975) * rate_se), hi = rate + qnorm(0.975) * rate_se,
         release = sprintf("%d–%d", year - 4, year))

# ==================================================================================================
# 5. Neighborhood map
#    (from brunswick_rent_map.R)
# ==================================================================================================
# Map of the neighborhood boundaries used for the rent comparisons (creates `p_map`)
# Run after brunswick_rent_data.R
library(ggplot2)
library(patchwork)

map_cols <- c("Downtown NW" = "#8DD3C7", "Downtown NE" = "#FDB462", "Downtown South" = "#BEBADA",
              "East Brunswick" = "#FB8072", "Mere Point & the Farms" = "#80B1D3",
              "Other" = "#D9D9D9")  # ColorBrewer Set3

map_area <- function(df) {
  df |> mutate(map_area = factor(combine_areas(if_else(!is.na(downtown_region), as.character(downtown_region),
                                                       as.character(neighborhood))), levels = names(map_cols)))
}

town <- county_subdivisions("ME", county = "Cumberland", year = 2023) |>
  filter(NAME == "Brunswick") |> st_transform(4326)
town_roads <- roads("ME", county = "Cumberland", year = 2023) |>
  filter(RTTYP %in% c("I", "U", "S")) |> st_transform(4326) |> st_intersection(st_geometry(town))

parcel_pts <- parcels |> map_area() |> filter(!is.na(map_area))
listing_pts <- listings |> distinct(street, lat, lon, neighborhood, downtown_region) |>
  filter(!is.na(lat)) |> map_area()

area_map <- function(xlim, ylim, title, point_size) {
  ggplot() +
    geom_sf(data = town, fill = "grey97", color = "grey40", linewidth = 0.4) +
    geom_sf(data = town_roads, color = "grey78", linewidth = 0.3) +
    geom_sf(data = downtown_roads, color = "grey30", linewidth = 0.6) +
    geom_point(data = parcel_pts, aes(lon, lat, fill = map_area, size = units),
               shape = 21, color = "grey30", stroke = 0.2, alpha = 0.85) +
    geom_point(data = listing_pts, aes(lon, lat, fill = map_area), shape = 24,
               color = "black", stroke = 0.35, size = point_size) +
    scale_fill_manual(values = map_cols, name = NULL, drop = FALSE,
                      guide = guide_legend(override.aes = list(shape = 22, size = 4, alpha = 1))) +
    scale_size_area(max_size = 5, name = "Possibly-rental\nunits on parcel",
                    breaks = c(1, 10, 50)) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    labs(subtitle = title) +
    theme_void(base_size = 20) +
    theme(plot.subtitle = element_text(face = "bold", margin = margin(b = 4)))
}

tb <- st_bbox(town)
p_town <- area_map(tb[c(1, 3)], tb[c(2, 4)], "Neighborhoods", 1.6)
p_downtown <- area_map(c(-69.990, -69.948), c(43.894, 43.923),
                       "Downtown areas\n(dark lines: Maine St, McKeen St, Bath Rd, Baribeau Dr)", 2.2) +
  theme(panel.border = element_rect(fill = NA, color = "grey30"))

p_map <- (p_town | p_downtown) +
  plot_layout(guides = "collect", widths = c(1, 1.1)) +
  plot_annotation(
    title = "Neighborhood boundaries used for the rent distributions",
    subtitle = paste("Circles: assessor parcels with possibly-rental units (sized by units).",
                     "\nTriangles: Zillow listing addresses."),
    caption = paste("Sources: Town of Brunswick assessing data; daily Zillow scrape; U.S. Census TIGER/Line 2023.",
                    "\nColors: ColorBrewer Set3."),
    theme = theme(plot.title = element_text(face = "bold", size = 28),
                  plot.subtitle = element_text(size = 22),
                  plot.caption = element_text(size = 14, color = "grey40", hjust = 0)))

# ==================================================================================================
# 6. Rent distributions and vacancy figures (slides 4-7)
#    (from brunswick_rent_densities.R)
# ==================================================================================================
# Offered rents (Zillow) vs. current rents (ACS) by Brunswick neighborhood: overlaid densities
# Output: brunswick_rent_densities.pdf and brunswick_year_rent_densities.pdf (town-wide, every ACS release)
library(dplyr)
library(tidyr)
library(ggplot2)

set.seed(20251013)
bw     <- 125                  # kernel bandwidth ($), the same for both sources
grid   <- c(from = 0, to = 5000)
n_grid <- 501
n_sims <- 500
top_code_upper <- 4500         # ACS "$3,500 or more" spread over $3,500-4,500
no_band_areas  <- "Mere Point & the Farms"   # too few renters for informative bands

kde <- function(x, w = NULL) {
  d <- density(x, weights = w, bw = bw, from = grid["from"], to = grid["to"], n = n_grid)
  d$y * 100                    # share of units per $100 of monthly rent
}
rent_grid <- seq(grid["from"], grid["to"], length.out = n_grid)

# --- Offered rents: kernel density of listing rents, bootstrap bands -------------------
offered_density <- offered_listings |>
  group_by(area) |>
  group_modify(function(d, key) {
    boot <- replicate(n_sims, kde(sample(d$rent, replace = TRUE)))
    tibble(rent = rent_grid, density = kde(d$rent),
           lo = apply(boot, 1, quantile, 0.025), hi = apply(boot, 1, quantile, 0.975))
  }) |>
  ungroup() |>
  mutate(source = "Offered rents (Zillow listings, 2025–26)")

# --- Current rents: ACS categories spread evenly within each range, then smoothed ------
# The curve is f(x) = sum_k n_k g_k(x) / T, where g_k is category k's smoothed uniform shape,
# n_k its allocated count, and T = sum_k n_k. Bands use the delta method for this ratio
# (the Census Bureau's ratio formula), treating counts as independent across block groups and
# categories: Var f(x) = sum_k Var(n_k) (g_k(x) - f(x))^2 / T^2.
# (Redrawing counts with negative draws clipped to zero is biased here: most block-group cells
# are zero with a +/-11 MOE, so clipping adds phantom renters to every empty, mostly low-rent, range.)
acs_bins <- bins |> mutate(upper = if_else(is.infinite(upper), top_code_upper, upper))
spread_pts <- acs_bins |>
  group_by(variable) |>
  reframe(rent = seq(lower + 5, upper - 5, by = 10))

shape <- sapply(split(spread_pts$rent, spread_pts$variable), kde)   # grid x category: g_k(x)

current_density <- acs_tbl |>
  filter(variable %in% acs_bins$variable) |>
  inner_join(bg_shares |> select(GEOID, area, share), by = "GEOID", relationship = "many-to-many") |>
  group_by(area, variable) |>
  summarise(n = sum(share * estimate), var = sum(share^2 * se^2), .groups = "drop") |>
  group_by(area) |>
  group_modify(function(d, key) {
    g <- shape[, d$variable]
    total <- sum(d$n)
    f <- drop(g %*% d$n) / total
    f_se <- sqrt(drop((g - f)^2 %*% d$var)) / total
    tibble(rent = rent_grid, density = f,
           lo = pmax(0, f - qnorm(0.975) * f_se), hi = f + qnorm(0.975) * f_se)
  }) |>
  ungroup() |>
  mutate(source = sprintf("Current rents (ACS %d–%d contract rent)", acs_year - 4, acs_year))

source_levels <- c(unique(current_density$source), unique(offered_density$source))

densities <- bind_rows(current_density, offered_density) |>
  mutate(area = factor(area, levels = area_levels),
         source = factor(source, levels = source_levels),
         lo = if_else(area %in% no_band_areas, NA_real_, lo),
         hi = if_else(area %in% no_band_areas, NA_real_, hi))

median_lines <- medians |>
  select(area, current_median, offered_median) |>
  pivot_longer(-area, names_to = "which", values_to = "median") |>
  mutate(source = if_else(which == "current_median", unique(current_density$source),
                          unique(offered_density$source))) |>
  mutate(source = factor(source, levels = source_levels))

# --- Plot -------------------------------------------------------------------------------
dollars <- scales::dollar_format(accuracy = 1)
font <- 1.5                    # enlarges axis text and titles, panel labels, and legend text
facet_labels <- medians |>
  mutate(label = sprintf("%s\n~%s renter households, %d listings",
                         area, scales::comma(renter_households, accuracy = 1), listings))
facet_labels <- setNames(facet_labels$label, facet_labels$area)

source_cols <- setNames(c("#80B1D3", "#FB8072"),            # ColorBrewer Set3
                        source_levels)

# Method notes (formerly the figure caption):
#   Current rents: ACS 5-year contract rent (B25056), renter households paying cash rent, allocated from
#   block groups to areas in proportion to possibly-rental units on assessor parcels. Each ACS range's
#   households are spread evenly across it ("$3,500 or more" across $3,500-4,500) before smoothing;
#   bands are delta-method (ratio) standard errors from block-group margins of error.
#   Earlier rents (year plot only): town-level ACS releases 2005-2009 to 2019-2023, spread and smoothed
#   the same way ("$2,000 or more" before 2011-2015 across $2,000-3,000); not adjusted for inflation.
#   Offered rents: distinct Zillow listings, June 2025-April 2026, each at its median asking rent;
#   bootstrap bands. All curves use a Gaussian kernel with bandwidth `bw`.
#   Contract rent excludes utilities; some listings include them. Downtown South combines SW and SE.

# Rent affordable at 30% of the median renter household's income: dark dashed line, labeled at the top
afford_note <- "Black dashed line: rent affordable at 30% of the median renter household's income."
affordable_layers <- function(areas, size) {
  d <- medians |> filter(area %in% areas)
  list(geom_vline(data = d, aes(xintercept = affordable_rent), linetype = "dashed", color = "grey15",
                  linewidth = 0.6),
       geom_text(data = d, aes(x = affordable_rent, y = Inf, label = paste("Affordable:", dollars(affordable_rent))),
                 inherit.aes = FALSE, hjust = -0.05, vjust = 1.5, size = size, color = "grey15"))
}

# One page of density panels for the given areas; `scale` enlarges text and legend for fewer panels,
# `text_scale` enlarges only the text
density_page <- function(areas, title, nrow, scale = 1, subtitle_extra = "", text_scale = 1) {
  ggplot(filter(densities, area %in% areas), aes(rent, density, color = source, fill = source)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.3, color = NA, na.rm = TRUE) +
    geom_line(linewidth = 0.9 * sqrt(scale)) +
    geom_vline(data = filter(median_lines, area %in% areas), aes(xintercept = median, color = source),
               linetype = "dashed", linewidth = 0.5 * sqrt(scale), show.legend = FALSE) +
    affordable_layers(areas, size = 3 * font * text_scale) +
    facet_wrap(~ area, nrow = nrow, labeller = as_labeller(facet_labels)) +
    scale_color_manual(values = source_cols, name = NULL, aesthetics = c("color", "fill")) +
    scale_x_continuous(labels = scales::dollar, breaks = seq(0, 5000, 1000),
                       expand = expansion(c(0, 0.01))) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = scales::breaks_extended(Q = c(1, 5, 2, 4)),
                       expand = expansion(c(0, 0.03))) +
    labs(title = title,
         subtitle = paste0("Density of monthly rents; dashed lines mark medians. ",
                           "Shaded bands are 95% confidence intervals for current and offered rents.\n",
                           afford_note, subtitle_extra),
         x = "Monthly rent", y = "Share of units per $100 of rent") +
    guides(color = guide_legend(nrow = 2,
                                override.aes = list(linewidth = 1.5 * scale))) +
    theme_minimal(base_size = 10 * sqrt(scale) * text_scale) +
    theme(legend.position = "top", legend.justification = "left",
          legend.text = element_text(size = 9 * scale * font * text_scale),
          legend.key.width = unit(1.5 * scale, "lines"), legend.key.height = unit(scale, "lines"),
          axis.text = element_text(size = rel(0.8 * font)), axis.title = element_text(size = rel(font)),
          plot.title = element_text(face = "bold", size = 14 * sqrt(scale) * text_scale),
          strip.text = element_text(hjust = 0, size = 8.5 * sqrt(scale) * font * text_scale, lineheight = 1.05),
          panel.grid.minor = element_blank(),
          panel.spacing = unit(1, "lines"), panel.spacing.x = unit(2.5 * text_scale^2, "lines"),
          plot.margin = margin(5.5, 20, 5.5, 5.5))
}

# --- The whole town: current and offered rents ---------------------------------------------
now_sources <- c(unique(current_density$source), unique(offered_density$source))
town_now <- densities |> filter(area == "All of Brunswick", source %in% now_sources)
town_info <- medians |> filter(area == "All of Brunswick")
scale <- 1.6                   # larger text and legend for a single panel
text_scale <- 1.2              # text only, for the slides

p_all <- ggplot(town_now, aes(rent, density, color = source, fill = source)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.3, color = NA) +
  geom_line(linewidth = 0.9 * sqrt(scale)) +
  geom_vline(data = filter(median_lines, area == "All of Brunswick"),
             aes(xintercept = median, color = source), linetype = "dashed", linewidth = 0.5 * sqrt(scale),
             show.legend = FALSE) +
  affordable_layers("All of Brunswick", size = 3.4 * font * text_scale) +
  scale_color_manual(values = source_cols, name = NULL, aesthetics = c("color", "fill")) +
  scale_x_continuous(labels = scales::dollar, breaks = seq(0, 5000, 1000),
                     expand = expansion(c(0, 0.01))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = scales::breaks_extended(Q = c(1, 5, 2, 4)),
                     expand = expansion(c(0, 0.03))) +
  labs(title = "Offered vs. current rents, Brunswick, Maine",
       subtitle = paste0("Density of monthly rents; dashed lines mark medians. ",
                         "Shaded bands are 95% confidence intervals for current and offered rents.\n",
                         afford_note, "\n",
                         sprintf("~%s renter households, %d listings",
                                 scales::comma(town_info$renter_households, accuracy = 1), town_info$listings)),
       x = "Monthly rent", y = "Share of units per $100 of rent") +
  guides(color = guide_legend(nrow = 2, override.aes = list(linewidth = 1.5 * scale))) +
  theme_minimal(base_size = 10 * sqrt(scale) * text_scale) +
  theme(legend.position = "top", legend.justification = "left",
        legend.text = element_text(size = 9 * scale * font * text_scale),
        legend.key.width = unit(1.5 * scale, "lines"), legend.key.height = unit(scale, "lines"),
        axis.text = element_text(size = rel(0.8 * font)), axis.title = element_text(size = rel(font)),
        plot.title = element_text(face = "bold", size = 14 * sqrt(scale) * text_scale),
        panel.grid.minor = element_blank())

# --- The whole town, every ACS 5-year release (separate file) ------------------------------
# Earlier releases are town-level (county subdivision) estimates, spread and smoothed as above;
# the current release is the town-wide curve above (its allocated block groups sum to the town).
year_density <- town_tbl |>
  mutate(upper = if_else(is.infinite(upper), if_else(year < 2015, 3000, top_code_upper), upper)) |>
  filter(estimate > 0) |>
  group_by(year) |>
  group_modify(function(d, key) {
    pts <- d |> reframe(rent = seq(lower + 5, upper - 5, by = 10), w = estimate / length(rent), .by = variable)
    tibble(rent = rent_grid, density = kde(pts$rent, pts$w / sum(pts$w)))
  }) |>
  ungroup() |>
  bind_rows(town_now |> filter(source == now_sources[1]) |> transmute(year = acs_year, rent, density, lo, hi))
year_labeled <- c(2011, 2017, 2018, acs_year)
year_peaks <- year_density |> filter(year %in% year_labeled) |>
  group_by(year) |> slice_max(density, n = 1, with_ties = FALSE) |> ungroup()
n_years <- n_distinct(year_density$year)
year_cols <- setNames(hcl(h = seq(10, 290, length.out = n_years), c = 100, l = 55),   # rainbow, red (earliest)
                      sort(unique(year_density$year)))                                # to violet (current)

p_years <- ggplot(year_density, aes(rent, density, color = factor(year))) +
  geom_line(aes(linewidth = year == acs_year)) +
  # Labels for selected years, spread out to the right of each curve's mode, with leader lines
  ggrepel::geom_text_repel(data = year_peaks, aes(label = year), size = 6.75 * font, hjust = 0,
                           direction = "y", nudge_x = 1500 - year_peaks$rent, xlim = c(1500, NA),
                           min.segment.length = 0, segment.color = "grey75", segment.size = 0.3,
                           box.padding = 0.1, max.overlaps = Inf, seed = 1) +
  scale_color_manual(values = year_cols, guide = "none") +
  scale_linewidth_manual(values = c(`FALSE` = 0.7, `TRUE` = 1.3), guide = "none") +
  scale_x_continuous(labels = scales::dollar, breaks = seq(0, 5000, 1000),
                     expand = expansion(c(0, 0.01))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = scales::breaks_extended(Q = c(1, 5, 2, 4)),
                     expand = expansion(c(0, 0.03))) +
  labs(title = "Distribution of rent over time in Brunswick, Maine",
       subtitle = paste0("Density of monthly contract rent in each ACS 5-year estimate,\nfrom 2005–2009 (red) to ",
                         acs_year - 4, "–", acs_year, " (violet);\nselected curves labeled by final year ",
                         "(2009 = 2005–2009).\n",
                         sprintf("~%s renter households in %d–%d",
                                 scales::comma(town_info$renter_households, accuracy = 1), acs_year - 4, acs_year)),
       x = "Monthly rent", y = "Share of units per $100 of rent") +
  theme_minimal(base_size = 10 * sqrt(scale)) +
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30),
        plot.title = element_text(face = "bold", size = 32.5), plot.subtitle = element_text(size = 22.5),
        panel.grid.minor = element_blank(), plot.margin = margin(5.5, 45, 5.5, 5.5))

# Second page: each release on its own panel, same colors and axes, with the current release in grey
# behind each earlier one for comparison
year_current <- year_density |> filter(year == acs_year) |> select(rent, density) |>
  cross_join(tibble(year = setdiff(unique(year_density$year), acs_year)))
p_years_panels <- ggplot(year_density, aes(rent, density, color = factor(year))) +
  geom_line(data = year_current, color = "grey50", alpha = 0.5, linewidth = 0.9) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ year, ncol = 4, labeller = as_labeller(function(y) paste0(as.integer(y) - 4, "–", y))) +
  scale_color_manual(values = year_cols, guide = "none") +
  scale_x_continuous(labels = scales::dollar, breaks = seq(0, 4000, 2000),
                     expand = expansion(c(0, 0.01))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = scales::breaks_extended(Q = c(1, 5, 2, 4)),
                     expand = expansion(c(0, 0.03))) +
  labs(title = "Distribution of rent over time in Brunswick, Maine",
       subtitle = paste0("Density of monthly contract rent in each ACS 5-year estimate, one estimate per panel; ",
                         "grey curve: ", acs_year - 4, "–", acs_year, " for comparison."),
       x = "Monthly rent", y = "Share of units per $100 of rent") +
  theme_minimal(base_size = 10 * sqrt(scale)) +
  theme(axis.text = element_text(size = rel(0.8 * font)), axis.title = element_text(size = rel(font)),
        strip.text = element_text(hjust = 0, size = rel(font)),
        plot.title = element_text(face = "bold", size = 14 * sqrt(scale)),
        panel.grid.minor = element_blank(), panel.spacing.x = unit(2, "lines"),
        plot.margin = margin(5.5, 20, 5.5, 5.5))

p_hoods <- density_page(c("Downtown NW", "Downtown NE", "Downtown South", "East Brunswick", "Mere Point & the Farms"),
                        "Offered vs. current rents by neighborhood, Brunswick, Maine", nrow = 2,
                        subtitle_extra = "\nBands are omitted for Mere Point & the Farms, which has too few renters.",
                        text_scale = 1.2)

# --- Rental vacancy rate over time, whole town ---------------------------------------------
p_vacancy <- ggplot(town_vacancy, aes(substr(release, 1, 4), rate)) +
  geom_col(fill = source_cols[[1]], width = 0.75) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.3, linewidth = 0.4, color = "grey30") +
  geom_text(aes(y = hi, label = scales::percent(rate, accuracy = 0.1)), vjust = -0.6,
            size = 4 * font, color = "grey20") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA),
                     expand = expansion(c(0, 0.08))) +
  labs(title = "Rental vacancy rate over time, Brunswick, Maine",
       subtitle = paste("Units vacant for rent as a share of the rental stock (renter-occupied, vacant for rent, and rented",
                        "but not yet occupied),
by ACS 5-year estimate. Error bars are 95% confidence intervals."),
       x = "First year of ACS 5-year estimate", y = "Rental vacancy rate") +
  theme_minimal(base_size = 12.5 * sqrt(scale)) +
  theme(axis.text = element_text(size = rel(0.8 * font)), axis.title = element_text(size = rel(font)),
        plot.title = element_text(face = "bold", size = 17.5 * sqrt(scale)),
        panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        plot.margin = margin(5.5, 20, 5.5, 5.5))

cairo_pdf("brunswick_rent_densities.pdf", width = 14, height = 8.5, onefile = TRUE)
print(p_all)
print(p_hoods)
print(p_map + plot_annotation(caption = NULL))
invisible(dev.off())

cairo_pdf("brunswick_year_rent_densities.pdf", width = 14, height = 8.5, onefile = TRUE)
print(p_years)
print(p_years_panels)
print(p_vacancy)
invisible(dev.off())

# ==================================================================================================
# 7. Rents and vacancy for the estimates starting 2012-2017 (slide 9)
#    (from slides/natural_vacancy_figures.R)
# ==================================================================================================
# Figures for natural_vacancy_rate.tex, slide 2: rent densities and rental vacancy rates for the
# ACS 5-year estimates starting 2012-2017 (2012-2016 to 2017-2021), with a rainbow spread over just these six estimates
# Run from the slides directory; rebuilds the data via the main densities script
# Output: fig_rent_2012_2017.pdf, fig_vacancy_2012_2017.pdf

slide_years <- 2016:2021     # final years of the estimates starting 2012-2017
slide_cols <- setNames(hcl(h = seq(10, 290, length.out = length(slide_years)), c = 100, l = 55), slide_years)
slide_theme <- theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), plot.margin = margin(4, 10, 4, 4))

p_slide_rent <- ggplot(filter(year_density, year %in% slide_years), aes(rent, density, color = factor(year))) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ year, ncol = 3, labeller = as_labeller(function(y) paste0(as.integer(y) - 4, "–", y))) +
  scale_color_manual(values = slide_cols, guide = "none") +
  scale_x_continuous(labels = scales::label_dollar(scale = 1e-3, suffix = "k"), breaks = seq(0, 3000, 1000),
                     limits = c(0, 3000),
                     expand = expansion(c(0, 0.01))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = scales::breaks_extended(Q = c(1, 5, 2, 4)),
                     expand = expansion(c(0, 0.03))) +
  labs(x = "Monthly rent", y = "Share per $100") +
  slide_theme +
  theme(strip.text = element_text(hjust = 0, size = rel(1.1)), panel.spacing.x = unit(1.8, "lines"))

p_slide_vacancy <- ggplot(filter(town_vacancy, year %in% slide_years), aes(release, rate, fill = factor(year))) +
  geom_col(width = 0.75) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.3, linewidth = 0.4, color = "grey30") +
  geom_text(aes(y = hi, label = scales::percent(rate, accuracy = 0.1)), vjust = -0.5, size = 4, color = "grey20") +
  scale_fill_manual(values = slide_cols, guide = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA),
                     expand = expansion(c(0, 0.15))) +
  labs(x = "ACS 5-year estimate", y = "Rental vacancy rate") +
  slide_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.major.x = element_blank())

ggsave("fig_rent_2012_2017.pdf", p_slide_rent, width = 5, height = 3.2, device = cairo_pdf)
ggsave("fig_vacancy_2012_2017.pdf", p_slide_vacancy, width = 5, height = 3.2, device = cairo_pdf)

# ==================================================================================================
# 8. Housing cost burden, owners vs. renters (slides 2-3)
#    (from brunswick_cost_burden.R)
# ==================================================================================================
# Housing cost burden in Brunswick, owners vs. renters, for every ACS 5-year release
# Output: brunswick_cost_burden.pdf
#   page 1: share paying 30%+ of income for housing, by household income, latest release
#   page 2: share paying 30%+ of income for housing, all incomes, by release
#   page 3: as page 1, one panel per release
# Source: ACS table B25106 (tenure by housing costs as a percentage of household income) for
# Brunswick town; the Census API has it from 2006-2010 on. Housing costs are gross rent for renters
# and selected monthly owner costs for owners. Households with zero or negative income, and renters
# paying no cash rent, have no computed share and are left out.
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidycensus)

years <- 2010:2024
z95 <- qnorm(0.975)

b25106 <- purrr::map_dfr(years, function(y) {
  labels <- load_variables(y, "acs5") |>
    filter(grepl("^B25106_", name)) |>
    transmute(variable = name, label = gsub(":", "", label))
  get_acs(geography = "county subdivision", state = "ME", county = "Cumberland",
          table = "B25106", year = y, survey = "acs5") |>
    filter(NAME == "Brunswick town, Cumberland County, Maine") |>
    inner_join(labels, by = "variable") |>
    mutate(year = y)
}) |>
  separate_wider_delim(label, "!!", names = c(NA, NA, "tenure", "income", "burden"),
                       too_few = "align_start") |>
  filter(!is.na(income), !income %in% c("Zero or negative income", "No cash rent")) |>
  mutate(tenure = if_else(grepl("^Owner", tenure), "Owners", "Renters"),
         se = moe / qnorm(0.95))     # ACS margins of error are 90% intervals

income_levels <- c("Less than $20,000", "$20,000 to $34,999", "$35,000 to $49,999",
                   "$50,000 to $74,999", "$75,000 or more", "All incomes")
income_labels <- c("<$20k", "$20–35k", "$35–50k", "$50–75k", "$75k+", "All")

# Share paying 30%+ within each income range, plus all ranges together
# (Census Bureau formula for the SE of a proportion; ratio formula when the radicand is negative)
burden_share <- function(d) {
  d |>
    summarise(burdened = sum(estimate[burden %in% "30 percent or more"]),
              burdened_se = sqrt(sum(se[burden %in% "30 percent or more"]^2)),
              total = sum(estimate[is.na(burden)]), total_se = sqrt(sum(se[is.na(burden)]^2)),
              .groups = "drop") |>
    mutate(share = burdened / total,
           radicand = burdened_se^2 - share^2 * total_se^2,
           share_se = sqrt(if_else(radicand >= 0, radicand, burdened_se^2 + share^2 * total_se^2)) / total,
           lo = pmax(0, share - z95 * share_se), hi = pmin(1, share + z95 * share_se))
}

burden <- bind_rows(
  b25106 |> group_by(year, tenure, income) |> burden_share(),
  b25106 |> group_by(year, tenure) |> burden_share() |> mutate(income = "All incomes")
) |>
  mutate(income = factor(income, levels = income_levels, labels = income_labels),
         release = sprintf("%d–%d", year - 4, year))

print(burden |> filter(income == "All") |> select(release, tenure, total, share, lo, hi), n = Inf)

# --- Plots --------------------------------------------------------------------------------
font <- 1.5
tenure_cols <- c(Owners = "#FDB462", Renters = "#80B1D3")   # ColorBrewer Set3
dodge <- position_dodge(width = 0.8)
burden_theme <- theme_minimal(base_size = 12) +
  theme(legend.position = "top", legend.justification = "left",
        legend.text = element_text(size = rel(font)),
        axis.text = element_text(size = rel(0.8 * font)), axis.title = element_text(size = rel(font)),
        strip.text = element_text(hjust = 0, size = rel(font)),
        plot.title = element_text(face = "bold", size = 17),
        panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        plot.margin = margin(5.5, 20, 5.5, 5.5))

p_by_income <- ggplot(burden, aes(income, share, fill = tenure)) +
  geom_col(position = dodge, width = 0.8) +
  geom_errorbar(aes(ymin = lo, ymax = hi), position = dodge, width = 0.3, linewidth = 0.3, color = "grey30") +
  facet_wrap(~ release, ncol = 5) +
  scale_fill_manual(values = tenure_cols, name = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1),
                     expand = expansion(c(0, 0.02))) +
  labs(title = "Housing cost burden by household income, Brunswick, Maine",
       subtitle = paste("Share of households paying 30% or more of income for housing, by ACS 5-year estimate.",
                        "Error bars are 95% confidence intervals.\nIncomes are in each estimate's final-year dollars."),
       x = "Household income", y = "Share paying 30%+ of income") +
  burden_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.6 * font)),
        panel.spacing.x = unit(1.5, "lines"))

latest <- max(years)
p_latest <- ggplot(filter(burden, year == latest), aes(income, share, fill = tenure)) +
  geom_col(position = dodge, width = 0.8) +
  geom_errorbar(aes(ymin = lo, ymax = hi), position = dodge, width = 0.3, linewidth = 0.4, color = "grey30") +
  scale_fill_manual(values = tenure_cols, name = NULL) +
  scale_x_discrete(labels = sub(" to ", " to\n", sub("Less than ", "Less than\n",
                                                     c(head(income_levels, -1), "All\nincomes")))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1),
                     expand = expansion(c(0, 0.02))) +
  labs(title = sprintf("Housing cost burden by household income, Brunswick, Maine, %d–%d", latest - 4, latest),
       subtitle = paste("Share of households paying 30% or more of income for housing, ACS 5-year estimate.",
                        "\nError bars are 95% confidence intervals."),
       x = sprintf("Household income (%d dollars)", latest), y = "Share paying 30%+ of income") +
  burden_theme +
  # Fonts 50% larger than burden_theme, for the slides
  theme(legend.text = element_text(size = 27), axis.text = element_text(size = 21.6),
        axis.title = element_text(size = 27), plot.title = element_text(face = "bold", size = 25.5),
        plot.subtitle = element_text(size = 18))

p_by_year <- ggplot(filter(burden, income == "All"), aes(factor(year - 4), share, fill = tenure)) +
  geom_col(position = dodge, width = 0.8) +
  geom_errorbar(aes(ymin = lo, ymax = hi), position = dodge, width = 0.3, linewidth = 0.4, color = "grey30") +
  scale_fill_manual(values = tenure_cols, name = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA),
                     expand = expansion(c(0, 0.05))) +
  labs(title = "Cost-burdened owners and renters over time, Brunswick, Maine",
       subtitle = paste("Share of households paying 30% or more of income for housing, by ACS 5-year estimate.",
                        "\nError bars are 95% confidence intervals."),
       x = "First year of ACS 5-year estimate", y = "Share paying 30%+ of income") +
  burden_theme +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 24),
        legend.text = element_text(size = 24), plot.title = element_text(face = "bold", size = 30),
        plot.subtitle = element_text(size = 22))

cairo_pdf("brunswick_cost_burden.pdf", width = 14, height = 8.5, onefile = TRUE)
print(p_latest)
print(p_by_year)
print(p_by_income)
invisible(dev.off())

# --- Table (slide 2): cost-burdened households by tenure, latest release ---------------------
# Same households as the figure: zero/negative income and no-cash-rent renters are left out
burden_table <- b25106 |>
  filter(year == latest, !is.na(burden)) |>
  group_by(tenure) |>
  summarise(cost_burdened     = sum(estimate[burden == "30 percent or more"]),
            not_cost_burdened = sum(estimate[burden != "30 percent or more"])) |>
  mutate(odds = cost_burdened / not_cost_burdened)
print(burden_table)
message(sprintf("Cost burdened: %.0f%% of households; odds ratio, renters vs. owners: %.1f",
                100 * sum(burden_table$cost_burdened) /
                  sum(burden_table$cost_burdened + burden_table$not_cost_burdened),
                burden_table$odds[burden_table$tenure == "Renters"] /
                  burden_table$odds[burden_table$tenure == "Owners"]))

# ==================================================================================================
# 9. Short-term rental maps and table (slides 1, 10-12)
#    (from brunswick_str_map.R)
# ==================================================================================================
# Map of short-term rental (STR) listings in Brunswick, ME, with a stacked bar
# of how many listings could be mapped and how many could not
# Requires data/brunswick_housing.gpkg from brunswick_housing_db.R
library(sf)
library(tigris)
library(tidygeocoder)
library(dplyr)
library(ggplot2)
library(patchwork)

options(tigris_use_cache = TRUE)

# --- Town boundary, roads, and water from Census TIGER/Line -------------------
town <- county_subdivisions("ME", county = "Cumberland", year = 2023) |>
  filter(NAME == "Brunswick")

roads <- roads("ME", county = "Cumberland", year = 2023) |>
  st_intersection(st_geometry(town))
major_roads <- roads |> filter(RTTYP %in% c("I", "U", "S"))

water <- area_water("ME", county = "Cumberland", year = 2023) |>
  st_intersection(st_geometry(town))

# --- STR listings ---------------------------------------------------------------
# Counted in listings, not units: listings without an address can't be matched
# to each other, so duplicates across sites can't be removed for them
listings <- read.csv("RentalListings2025-04-18.csv", colClasses = "character",
                     check.names = FALSE) |>
  filter(`Meets STR Definition` == "Yes")

# Locate by parcel number first, then by geocoding the address
parcels <- st_read("data/brunswick_housing.gpkg", quiet = TRUE) |>
  distinct(account, .keep_all = TRUE) |>
  st_point_on_surface() |>
  st_transform(4326)
parcel_xy <- parcels |>
  mutate(p_lon = st_coordinates(geom)[, 1], p_lat = st_coordinates(geom)[, 2]) |>
  st_drop_geometry() |>
  select(account, building_type, p_lon, p_lat)

addresses <- listings |>
  filter(Address != "") |>
  distinct(Address) |>
  geocode_combine(queries = list(list(method = "census"), list(method = "osm")),
                  global_params = list(address = "Address"),
                  lat = a_lat, long = a_lon)

listings <- listings |>
  left_join(parcel_xy, by = c("Parcel Number" = "account")) |>
  left_join(addresses, by = "Address") |>
  mutate(lon = coalesce(p_lon, a_lon),
         lat = coalesce(p_lat, a_lat),
         mapped = !is.na(lon),
         type = case_when(
           building_type == "Single-family" ~ "Single-family home",
           # A single-family home with an apartment has two units
           building_type %in% c("Single-family with apartment", "Condo/PUD unit", "2-unit",
                                "3-4 unit", "5+ unit",
                                "Mixed-use commercial/apartment") ~ "Condo or multi-unit",
           TRUE ~ "Unknown building type"))

# One point per location and building type, sized by the number of listings there
str_sf <- listings |>
  filter(mapped) |>
  count(lon = round(lon, 5), lat = round(lat, 5), type, name = "n_listings") |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(st_crs(town)) |>
  st_filter(town) |>
  arrange(desc(n_listings))

n_mapped   <- sum(str_sf$n_listings)
n_unmapped <- nrow(listings) - n_mapped
n_no_info  <- sum(listings$Address == "" & listings$`Parcel Number` == "")
message(sprintf("%d STR listings: %d mapped at %d locations, %d not mapped",
                nrow(listings), n_mapped, nrow(str_sf), n_unmapped))

# --- Map ----------------------------------------------------------------------
type_cols <- c("Single-family home"     = "#e06c66",  # soft red
               "Condo or multi-unit"    = "#3d8c8c",  # teal
               "Unknown building type"  = "grey55")
rest_col  <- "grey85"   # mapped, outside the area shown
unmap_col <- "grey70"

str_map <- function(xlim, ylim, road_width = 1, max_size = 6) {
  ggplot() +
    geom_sf(data = town, fill = "grey97", color = "grey30", linewidth = 0.5) +
    geom_sf(data = water, fill = "#cfe3f2", color = NA) +
    geom_sf(data = roads, color = "grey82", linewidth = 0.2 * road_width) +
    geom_sf(data = major_roads, color = "grey55", linewidth = 0.6 * road_width) +
    geom_sf(data = str_sf, aes(size = n_listings, fill = type), shape = 21,
            color = "grey15", stroke = 0.3, alpha = 0.85) +
    scale_size_area(name = "STR listings at location", max_size = max_size,
                    breaks = c(1, 2, 4, 8),
                    guide = guide_legend(override.aes = list(fill = "grey60"))) +
    scale_fill_manual(name = NULL, values = type_cols, breaks = names(type_cols),
                      guide = guide_legend(override.aes = list(size = 3.5))) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_void(base_size = 11) +
    theme(legend.position = "bottom", legend.box = "vertical")
}

# --- Stacked bar ----------------------------------------------------------------
# bar_data: status (factor, bottom segment last), n, fill, text color
str_bar <- function(bar_data) {
  ggplot(bar_data, aes(x = 1, y = n, fill = fill, group = status)) +
    geom_col(width = 0.6, color = "white", linewidth = 0.5) +
    geom_text(aes(label = ifelse(n < 15, "",
                                 ifelse(status %in% names(type_cols), as.character(n),
                                        sprintf("%s\n%d", status, n))),
                  color = text_col),
              position = position_stack(vjust = 0.5), size = 3.2, lineheight = 0.9) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
    labs(x = NULL, y = "STR listings") +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank())
}

caption <- paste("Listings are located at their assessing parcel when the report gives a parcel number, otherwise by geocoding the address.",
                 "\nCondo or multi-unit includes single-family homes with an apartment. The same unit is often listed on several sites.",
                 "\nSources: Town of Brunswick STR listings report and assessing data; U.S. Census TIGER/Line 2023; Census Geocoder; OpenStreetMap.")
page_theme <- theme(plot.title = element_text(size = 16, face = "bold"),
                    plot.subtitle = element_text(size = 9.5, color = "grey25"),
                    plot.caption = element_text(size = 7.5, color = "grey40", hjust = 0),
                    plot.background = element_rect(fill = "white", color = NA))
unmapped_text <- if (n_no_info == n_unmapped) {
  sprintf("%d have no address or parcel number and are not shown.", n_unmapped)
} else {
  sprintf("%d are not (%d have no address or parcel number).", n_unmapped, n_no_info)
}

# --- Page 1: whole town -------------------------------------------------------
bb <- st_bbox(town)
# Mapped listings by building type, with not mapped on top
type_counts <- function(pts) {
  sapply(names(type_cols), function(t) sum(pts$n_listings[pts$type == t]))
}
town_types <- type_counts(str_sf)
town_bar <- data.frame(
  status = factor(c(names(type_cols), "Not mapped"),
                  levels = rev(c(names(type_cols), "Not mapped"))),
  n = c(town_types, n_unmapped),
  fill = c(type_cols, unmap_col),
  text_col = c("white", "white", "white", "grey15")
)

p_town <- (str_map(bb[c(1, 3)], bb[c(2, 4)]) | str_bar(town_bar)) +
  plot_layout(widths = c(8, 1)) +
  plot_annotation(
    title = "Short-term rentals in Brunswick, Maine",
    subtitle = paste0(sprintf("%d listings meeting the town's STR definition (listings report, April 18, 2025).\n%d are mapped; ",
                              nrow(listings), n_mapped), unmapped_text),
    caption = caption, theme = page_theme)

# --- Page 2: downtown and Bowdoin (same area as brunswick_map.R) ---------------
downtown <- st_bbox(c(xmin = -69.985, xmax = -69.935, ymin = 43.895, ymax = 43.925),
                    crs = 4326) |>
  st_as_sfc() |>
  st_transform(st_crs(town))
db <- st_bbox(downtown)
in_downtown <- st_intersects(str_sf, downtown, sparse = FALSE)[, 1]
downtown_types <- type_counts(str_sf[in_downtown, ])
n_downtown <- sum(downtown_types)

downtown_bar <- data.frame(
  status = factor(c(names(type_cols), "Rest of town", "Not mapped"),
                  levels = rev(c(names(type_cols), "Rest of town", "Not mapped"))),
  n = c(downtown_types, n_mapped - n_downtown, n_unmapped),
  fill = c(type_cols, rest_col, unmap_col),
  text_col = c("white", "white", "white", "grey15", "grey15")
)

p_downtown <- (str_map(db[c(1, 3)], db[c(2, 4)], road_width = 1.6, max_size = 9) |
                 str_bar(downtown_bar)) +
  plot_layout(widths = c(8, 1)) +
  plot_annotation(
    title = "Short-term rentals in downtown Brunswick and around Bowdoin",
    subtitle = sprintf(paste0("%d of the %d mapped STR listings are in this area (listings report, April 18, 2025).\n",
                              "%d listings have no location and could be anywhere in town."),
                       n_downtown, n_mapped, n_unmapped),
    caption = caption, theme = page_theme)

print(town_bar[, c("status", "n")])
print(downtown_bar[, c("status", "n")])

# --- Table: listings by building type and area ---------------------------------
# East Brunswick as in brunswick_neighborhoods.R; its west edge is the downtown box's east edge
xy <- st_coordinates(st_transform(str_sf, 4326))
str_sf$area <- case_when(
  in_downtown                      ~ "Downtown",
  xy[, 2] > 43.855 & xy[, 1] > -69.935 ~ "East Brunswick",
  TRUE                             ~ "Elsewhere"
)
area_table <- str_sf |>
  st_drop_geometry() |>
  group_by(type, area) |>
  summarise(n = sum(n_listings), .groups = "drop") |>
  bind_rows(data.frame(type = "Unknown building type", area = "Not mapped", n = n_unmapped)) |>
  tidyr::pivot_wider(names_from = area, values_from = n, values_fill = 0)
print(area_table)
write.csv(area_table, "data/str_listings_by_area.csv", row.names = FALSE)

pdf("brunswick_str_map.pdf", width = 11, height = 8.5)
print(p_town)
print(p_downtown)
dev.off()

# Town map alone, with no text, legend, or bar, for the slides' title page
pdf("brunswick_str_map_notext.pdf", width = 9, height = 7)
print(str_map(bb[c(1, 3)], bb[c(2, 4)]) + theme(legend.position = "none"))
dev.off()

# --- Table (slide 10), last column: rental units in the whole town, by building type ------------
# Rental units are units that are rented or have no owner-occupant (which may include second homes),
# excluding STRs; building types grouped as in the STR table
rental_units <- read.csv("data/brunswick_rental_units.csv")
rental_by_type <- rental_units |>
  mutate(type = case_when(
    building_type == "Single-family" ~ "Single-family home",
    building_type %in% c("Single-family with apartment", "Condo/PUD unit", "2-unit", "3-4 unit", "5+ unit",
                         "Mixed-use commercial/apartment") ~ "Condo or multi-unit",
    TRUE ~ "Other (mobile homes, senior living, tax-exempt and boarding)")) |>
  count(type, name = "rental_units")
print(rental_by_type)
message(sprintf("Rental units in the whole town: %d", nrow(rental_units)))

# --- Slide 10 bullets: STR share of rental housing, in distinct units -------------------------
# Mapped listings at the same location with the same unit number are one unit (a unit is often
# listed on several sites). Share = STR units / (rental units + STR units).
in_box <- function(lon, lat) lon > -69.985 & lon < -69.935 & lat > 43.895 & lat < 43.925  # downtown
str_units <- listings |>
  filter(mapped) |>
  mutate(unit = toupper(gsub("[^A-Za-z0-9]", "", `Unit Number`))) |>
  distinct(lon5 = round(lon, 5), lat5 = round(lat, 5), unit, .keep_all = TRUE) |>
  mutate(downtown = in_box(lon, lat))
rental_units$downtown <- in_box(rental_units$lon, rental_units$lat)
town_share     <- nrow(str_units) / (nrow(rental_units) + nrow(str_units))
downtown_share <- sum(str_units$downtown) / (sum(rental_units$downtown) + sum(str_units$downtown))
message(sprintf(paste("%d mapped STR listings are %d distinct units (%d downtown).",
                      "STR share of rental housing: town %.1f%%, downtown %.1f%%.",
                      "Town share as a fraction of a 7%% natural vacancy rate: %.0f%%"),
                sum(listings$mapped), nrow(str_units), sum(str_units$downtown),
                100 * town_share, 100 * downtown_share, 100 * town_share / 0.07))
