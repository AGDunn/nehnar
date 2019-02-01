#' Make Map with Some Areas Obscured
#'
#' This function will return a map of a country with
#' only certain regions visible, based on supplying
#' it a list of which regions should be shown.
#'
#' Originally, this function was to show the places we'd visit within a
#' particular country, at the level of the administrative subdivision.  The
#' terminology reflects this.  Before running this function, you'll need a
#' dataframe that has a variable of ISO_3166_A2 codes for regions.  The
#' function will grab the map data based on the country you ask for but in the 
#' next version it'll accept an existing sf shape instead.  Can return
#' `nation'-level of detail for UK but will retain their internal borders and
#' make a nonsense of things if more than one country is included in the map.
#' 
#' @param country a 2-letter iso code that will be passed to ne_states().
#'   No default.
#' @param visited_places any df that includes ISO_3166 as a variable listing
#'   the subregions to include on the finished map.  No default
#' @param label_map include a map legend? Deafult TRUE. Not yet implemented.
#' @param uk_nations display the four main subregions of the UK (England,
#'   Scotland, Northern Ireland and Wales).  Default FALSE.
#' @keywords ISO, maps, sf
#' @export
map_visited_regions <- function(country, visited_places, label_map = TRUE,
                         uk_nations=FALSE){
  
  # get sf file of map information for country
  country_sf <- ne_states(
    iso_a2 = country,
    returnclass = "sf")
  
  # set bounding box for map to include full landmass of all countries
  # included in function call.
  country_bbox <- st_bbox(country_sf)
  
  # convert ISO column from factor to character if necessary
  if(is.factor(visited_places)){
    visited_places$ISO_3166 <- as.character(visited_places$ISO_3166)
  }

  # strip out any whitespace in the ISO column
  visited_places$ISO_3166 <- str_replace_all(visited_places$ISO_3166, " ", "")

  # take the vector of places which have been visited
  show_these <- visited_places %>% pull(ISO_3166)
  
  # add column, check_inclusion, that holds nation- or county-level ID,
  # depending on uk_nations TRUE/FALSE.  Filter mapping data based on this
  # column.
  if(!uk_nations){
    country_sf <- country_sf %>%
      group_by(name_en) %>%
      mutate(check_inclusion = iso_3166_2) %>%
      filter(check_inclusion %in% show_these)
  } else {
    show_these <- str_replace_all(show_these, "GB-", "")
    country_sf <- country_sf %>%
      group_by(name_en) %>%
      mutate(check_inclusion = gu_a3) %>%
      filter(check_inclusion %in% show_these)
  }

  # make the map
  my_map <- country_sf %>%
    ggplot() + 
      geom_sf() +
      coord_sf(
        xlim = c(country_bbox[1], country_bbox[3]),
        ylim = c(country_bbox[2], country_bbox[4])) +
      theme_bw()

# add legend if requested ZZZ
# if(label_map){
#   my_map <- my_map
# }

  return(my_map)
}
