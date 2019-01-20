#' Make Map with Some Areas Obscured
#'
#' This function will return a map of a country with
#' only certain regions visible, based on supplying
#' it a list of which regions should be shown.
#'
#' The original impetus for this function was to produce maps which would show 
#' the places we'd visit within a particular country, at the level of the
#' administrative subdivision.  The terminology reflects this.  Before running
#' this function, you'll need a dataframe that has a variable of ISO_3166_A2 
#' codes for regions.  The function will grab the map data based on the country 
#' you ask for but in the next version it'll accept an existing sf shape
#' instead.  It will also need more flexibility in how it takes a set of 
#' subregions to include in the finished map, partly to cope with the issue of 
#' subregions codings being so 'flat' in the case of the United Kingom (e.g.
#' England is GB-ENG, which means if it's present and not explicitly removed,
#' it won't matter what counties or cities are included.  The whole place will
#' be visible.
#' 
#' @param country a 2-letter iso code that will be passed to ne_states().
#'   No defaults.
#' @param visited_places any df that includes ISO_3166 as a variable listing
#'   the subregions to include on the finished map.
#' @keywords ISO, maps, sf
#' @export
map_visited_regions <- function(country, visited_places){
  
  # get sf file of map information for country
  country_sf <- ne_states(
    iso_a2 = country,
    returnclass = "sf")
  
  # store bbox for country for later use (so map will be sized to country, not
  # its pieces)
  country_bbox <- st_bbox(country_sf)
  
  # convert ISO column from column to factor if necessary
  if(is.factor(visited_places)){
    visited_places$ISO_3166 <- as.character(visited_places$ISO_3166)
  }

  # ZZZ strip out any whitespace in the ISO column
  visited_places$ISO_3166 <- str_replace_all(visited_places$ISO_3166, " ", "")

  # stip out the UK 'subnations' because they have their own ISO_31662_a2 codes
  # and so would obscure their own smaller subregional units.
  # Do other countries present similar problems?
  # https://en.wikipedia.org/wiki/ISO_3166-2:GB
  uk_nations <- c(
    "GB-ENG", "GB-NIR", "GB-SCT", "GB-WLS", "GB-CYM",
    "GB-EAW", "GB-GBN", "GB-UKM")
  visited_places <- visited_places %>%
    filter(!ISO_3166 %in% uk_nations)
  
  # separate the ISO code variable as a vector
  include_these <- visited_places %>% pull(ISO_3166)
  
  # filter out unwanted subregions and map what's left
  my_map <- country_sf %>%
    group_by(name_en) %>%
    filter(iso_3166_2 %in% include_these) %>%
    ggplot() + 
      geom_sf() +
      coord_sf(
        xlim = c(country_bbox[1], country_bbox[3]),
        ylim = c(country_bbox[2], country_bbox[4])) +
      theme_bw()
  
  return(my_map)
}
