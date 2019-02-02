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
#' @param country a 2-letter ISO code that will be passed to ne_states().
#'   No default.
#' @param visited_places any df that includes ISO_3166 as a variable listing
#'   the subregions to include on the finished map.  No default.
#' @param add_legend include a map legend? Default TRUE.
#' @param uk_countries display the four main subregions of the UK (England,
#'   Scotland, Northern Ireland and Wales).  Default FALSE.
#' @keywords ISO, maps, sf
#' @export
map_visited_regions <- function(country, visited_places, add_legend = TRUE,
                         uk_countries=FALSE){
# ZZZ add warnings block for silly argument values
  
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
  
  # check whether UK nation-level map; based on that create a column,
  # check_inclusion, to filter on and another, name_of_level, for sensible text
  # in the legend.  Sets legend title to match.
  if(!uk_countries){
    country_sf <- country_sf %>%
      group_by(name_en) %>%
      mutate(check_inclusion = iso_3166_2, name_of_level = name) %>%
      filter(check_inclusion %in% show_these)
      leg_title = "UK Countries and Province"
  } else {
    show_these <- str_replace_all(show_these, "GB-", "")
    country_sf <- country_sf %>%
      group_by(name_en) %>%
      mutate(check_inclusion = gu_a3, name_of_level = geonunit) %>%
      filter(check_inclusion %in% show_these)
      leg_title = "Counties and Local Authorities"
  } # ZZZ consider adding a way to fill/line based on uk_countries

  # make the map, with colours; check whether to add legend
  if(add_legend){
    my_map <- country_sf %>%
      ggplot() + 
        geom_sf(aes(fill = name_of_level)) +
        coord_sf(
          xlim = c(country_bbox[1], country_bbox[3]),
          ylim = c(country_bbox[2], country_bbox[4])) +
        theme_bw() +
        scale_fill_discrete(name = leg_title)
  } else {
    my_map <- country_sf %>%
      ggplot() + 
        geom_sf(aes(fill = name_of_level), show.legend = FALSE) +
        coord_sf(
          xlim = c(country_bbox[1], country_bbox[3]),
          ylim = c(country_bbox[2], country_bbox[4])) +
        theme_bw() +
        scale_fill_discrete(name = leg_title)
  }

  return(my_map)
}
